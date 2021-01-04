# TimescaleDB 2.0 with Hasura Part 2 - Multi-node

![Hasura TimescaleDB](/assets/timescale-hasura.png)

Multi-node is the most interesting feature of version 2.0 that provides the ability to create a cluster of TimescaleDB instances to scale both reads and writes. A multi-node TimescaleDB cluster consists of:

- One access node to handle ingest, data routing and act as an entry point for user access;
- One or more data nodes to store and organize distributed data.

In this post, I will try setting up this Multi-node feature and run with Hasura.

> This is the second part of series:
> - [Part 1 - From version 1 to 2.0](/posts/2020-12-31-TimescaleDB-2.0-with-Hasura-Part-1:-From-Version-1-to-2.0.html)
> - [Part 2 - Multi-node](/posts/2021-01-01-TimescaleDB-2.0-with-Hasura-Part-2:-Multi-node.html)
> - [Part 3 - High availability](/posts/2021-01-02-TimescaleDB-2.0-with-Hasura-Part-3:-High-Availability.html)

> The example code is uploaded on [Github](https://github.com/hgiasac/hasura-timescaledb2-example)

## Infrastructure setup

![TimescaleDB Multi-node diagram](/assets/timescale-multinode.png)

Cluster requirements:
- Network infrastructure is ready. The access node have to ensure that it is able to connect to data nodes when adding them.
- Authentication between nodes. The access node distributes requests to data nodes through client library, so it still need authentication credential. 

There are 3 authentication mechanisms:
  - Trust authentication
  - Password authentication
  - Certificate authentication

In this demo, we will use first mechanism. You can think it is insecure. I don't disagree. However, it will be fine if we place TimescaleDB cluster into private cloud, and expose access node to internet only. Docker environment can emulate it.

*You can read more detail about authentication in [official TimescaleDB docs](https://docs.timescale.com/latest/getting-started/setup-multi-node-basic/setup-multi-node-auth##node-communication)*

First of all, we need to register data nodes to the access node. 

```sql
SELECT add_data_node('data1', host => 'timescaledb-data1');
```

`host` can be IP or DNS name. In docker, you can use alias name. 

> Note: this function requires running in `AUTOCOMMIT` mode. So you can only run it with `psql`, or custom migration CLI. However, I don't recommend using migration, because Multi-node clusters configuration is different if you run multiple development environments (dev, staging, production...)

You can add nodes manually with `psql`. Fortunately the Postgres docker image supports initialization hooks `/docker-entrypoint-initdb.d`. You can use bash script to add them automatically.

```sh
# multinode/scripts/add-data-nodes.sh
psql -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB" <<-EOSQL
  SELECT add_data_node('$node', host => '$node');
EOSQL
```

> Can we register reversely from data node to access node?

It is possible. However you have to customize Postgres image. The temporary Postgres server for initialization scripts isn't exposed to external network ([source](https://github.com/docker-library/postgres/blob/03e769531fff4c97cb755e4a608b24935ceeee27/docker-entrypoint.sh#L238)). Therefore we can't run remote `psql` to register node.

Another solutions are:
- Creating a initialization container that waits all Postgres server online, then run `psql` script.
- Patroni with `postInit` hooks.  

## Create Distributed hypertable

Next step, we need to create distributed hypertable instead of normal, non-distributed hypertable. The difference of Distributed hypertable is, the data is stored across data node instances. The access node stores metadata of data nodes and distributes the requests and queries appropriately to those nodes, then aggregates the results received from them. Non-distributed  `hypertable` still stores data locally in current server.

The function is similar to `hypertable`. We run it after creating table

```sql
CREATE TABLE conditions (
  time        TIMESTAMPTZ       NOT NULL DEFAULT NOW(),
  location    TEXT              NOT NULL,
  temperature DOUBLE PRECISION  NULL,
  humidity    DOUBLE PRECISION  NULL
);

SELECT create_distributed_hypertable('conditions', 'time');
```

Here we encounter another issue. 

![Create Distribution Hypertable Error](/assets/timescale-create-distribution-hypertable-error.png)

Prepared transactions are disabled by default. `timescaledb-tune` doesn't automatically enable this setting too. We have to set it manually.

```ini
max_prepared_transactions = 150
enable_partitionwise_aggregate = on
```

You can mount manual `postgres.conf` file in Docker container. However, it can't take advantage of `timescaledb-tune`. Fortunately the Postgres imae also supports config flags. You can set it as arguments.

```yaml
timescaledb-data1:
  image: timescale/timescaledb:2.0.0-pg12
  command:
    - "-cmax_prepared_transactions=150"
  # ...
```

Restart services and run the function again. You can verify by querying hypertable information:

```sql
select * from timescaledb_information.hypertables;
```

| hypertable_schema | hypertable_name | owner | num_dimensions | num_chunks | compression_enabled | is_distributed | replication_factor | data_nodes | tablespaces |
| ----------------- | --------------- | ----- | -------------- |------------| ------------------- | -------------- | ------------------ | ---------- | ----------- |
| public | conditions | postgres | 1 | 2 | f | t | 1 | {timescaledb-data1,timescaledb-data2} | |

You may notice in `is_distributed` column that represent whether this hypertable is distributed or not. On data nodes, this column is `false` or non-distributed. That means if the hypertable is detached, it can work as normal hypertable. 

## Work with data

In this test, I will run SQL script to insert 1 million rows. Let's see how long the execution takes and number of rows per data node.

```sql
INSERT INTO conditions
    SELECT time, 'US', (random()*30)::int, random()*80
    FROM generate_series(
      '2020-01-01 00:00:00'::timestamptz, 
      '2020-01-01 00:00:00'::timestamptz + INTERVAL '999999 seconds', 
      '1 second')
    AS time;

Planning Time: 1.297 ms
Execution Time: 11763.459 ms
```

| access | timescaledb-data1 | timescaledb-data2 |
| ------ | ----------------- | ----------------- |
| 1,000,000  | 395,200 | 604,800 |

The distribution ratio isn't usually 1:1. The variance is larger when we insert large number of concurrent requests.
The insert speed is very fast. It takes about 11 seconds to insert 1 million row. In contrast, UPDATE and DELETE operations take very long time.
The below table show performance comparison between non-distributed hypertable and distributed hypertable, in milliseconds.

| Operation | Hypertable | Hypertable (JIT) | Distributed hypertable | Distributed hypertable (JIT) | 
| --------- | ---------- | ---------------- | ---------------------- | ---------------------------- |
| INSERT    | 3,707.195  | 4,586.120        | 12,700.597             | 11,763.459                   |      
| UPDATE    | 4,353.752  | 4,404.471        | 184,707.671            | 204,340.519                  |
| SELECT    | 174.012    | 210.479          | 2,011.194              | 1,057.094                    |
| DELETE    | 737.954    | 883.095          | 159,165.419            | 184,945.111                  |

- Distributed hypertable performance is much worse than non-distributed one. In my opinion, the access node has to distribute requests to data nodes through network. This causes higher latency cost than inserting directly into disk. Moreover, the access node take more computing power to query from all data nodes, then aggregate the final result.
- UPDATE and DELETE operations are extremely slow. We should avoid modifying data.
- JIT mode isn't really boost performance. From above table, the performance is slightly improved on Distributed hypertable, but slower on non-distributed one.

However, the main purpose of distributed hypertable is handling more write requests. The slower performance is expected trade-off.

## Foreign key and Relationship

For example, we create a table `locations`. `conditions` table reference `locations` through `location` column.

```sql
CREATE TABLE "public"."locations"(
  "id" Text NOT NULL, 
  "description" text NOT NULL, 
  PRIMARY KEY ("id") 
);

ALTER TABLE conditions ADD CONSTRAINT conditions_locations_fk 
  FOREIGN KEY (location) REFERENCES locations (id);
-- ERROR:  [timescaledb-data1]: relation "locations" does not exist
```

The foreign key can't be created. Because `locations` table is available in the access node only. Data nodes don't know about it. 
However, `JOIN` query is worked on the access node. TimescaleDB is smart enough to map relational data.

```sql
SELECT * FROM conditions JOIN locations ON conditions.location = locations.id;

             time              | location | temperature | humidity | id | description  
-------------------------------+----------+-------------+----------+----+--------------
 2020-12-31 16:32:42.328627+00 | US       |          10 |        1 | US | United State
 2020-12-31 16:34:01.648958+00 | US       |          12 |        1 | US | United State
 2020-12-31 16:34:35.241304+00 | US       |          13 |       13 | US | United State
```

## Continuous Aggregate View

We can't create Continuous Materialized view (see [Caveats](#caveats))

```log
postgres= CREATE MATERIALIZED VIEW conditions_summary_minutely
postgres-     WITH (timescaledb.continuous) AS
postgres-     SELECT time_bucket(INTERVAL '1 minute', time) AS bucket,
postgres-            AVG(temperature),
postgres-            MAX(temperature),
postgres-            MIN(temperature)
postgres-     FROM conditions
postgres-     GROUP BY bucket;
ERROR:  continuous aggregates not supported on distributed hypertables
```

Fortunately we still can create View or Materialized view. Therefore we can do workaround with View or the combination of Materialized view and scheduled job. The downside is worse performance than the continuous view.

```sql
CREATE MATERIALIZED VIEW conditions_summary_hourly  AS
    SELECT time_bucket(INTERVAL '1h', time) AS bucket,
           AVG(temperature),
           MAX(temperature),
           MIN(temperature)
    FROM conditions
    GROUP BY bucket;
    
CREATE OR REPLACE PROCEDURE conditions_summary_hourly_refresh(job_id int, config jsonb) LANGUAGE PLPGSQL AS
$$
BEGIN
  REFRESH MATERIALIZED VIEW conditions_summary_hourly;     
END
$$;

SELECT add_job('conditions_summary_hourly_refresh','1h');
```

## SQL function

Custom SQL functions for GraphQL queries work well. These functions are creating on Access node only. However, the query engine is smart enough to get data from data nodes.

```sql
CREATE OR REPLACE FUNCTION search_conditions(location text) 
    RETURNS SETOF conditions 
    LANGUAGE sql STABLE
AS $$
    SELECT * FROM conditions 
        WHERE conditions.location = location;
$$
```

However, Trigger doesn't work. The access node doesn't automatically create SQL functions and Triggers to data nodes. You have to create them on each data node. However it isn't good way to do.

There are more limitations that you can see in [Caveats](#caveats) section.

## Run with Hasura GraphQL Engine

It isn't different from original Postgres. GraphQL Engine service only needs connecting to the access node's database URL. There are still issues that are similar to non-distributed `hypertable`. View, SQL query function works as expected.

![Hasura Graphql Engine Console data](/assets/timescale-hasura-console-data.png)

![Hasura Graphql Engine Console Query](/assets/timescale-hasura-console-graphql.png)

However, because Foreign key doesn't work on distributed hypertable, GraphQL Engine can't automatically suggest relationship. You have to define manually.

![Manual Relationship](/assets/timescale-hasura-relationship-create.png)

![Relationship Query](/assets/timescale-hasura-relationship-query.png)

## Caveats

Beside non-distribution hypertable caveats, distributed hypertable has more downsides:
- Besides hypertable, another features aren't distributed: background jobs, Continuous aggregate view, compression policies, reordering chunks...
- Joins on data nodes are not supported.
- Consistent database structure required between access and data nodes.
- If you create SQL functions, foreign keys,... you have to create them in all data nodes. The access node can't automatically do it for you. 
- Native replication is still in experiment. 

The root cause is in the access node that can't manage consistent metadata with data nodes yet. It can't automatically sync database structure to data node except distributed hypertables. However, TimescaleDB 2.0 is still in early stage. The caveats still have chance to be solved in next versions.

You can read more detail in [TimescaleDB docs](https://docs.timescale.com/latest/using-timescaledb/limitations#distributed-hypertable-limitations)

## So, should I use it?

Yes, if you really need to scale write performance, and don't use Continuous aggregate view, compression policies features, at least in near future. In theory, you still can create them manually in each data node, since these hypertables in data nodes are normal one. However this workaround can lead to inconsistent database structures between nodes.

The access node is still a TimescaleDB server. You still can create tables and local hypertables and use them along with distributed hypertables.
