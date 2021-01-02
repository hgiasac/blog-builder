# Timescale 2 with Hasura Part 1 - From version 1 to 2.0

![Hasura Timescale](/assets/hasura-timescale.png)

As you probably know, [TimescaleDB](https://github.com/timescale/timescaledb) is an open-source database designed to make SQL scalable for time-series data. The most valuable features of TimescaleDB is `hypertable`, a high-level table that provides automatic partitioning across time and space (partitioning key).

Timescale 2.0 is a big major version upgrade that has many improvements from version 1. It introduces new interesting features and capabilities, especially horizontal multi-node scaling that can solve the limitation of write performance.

Because it is a PostgreSQL extension, it mostly works well with Hasura. However, there are several limitations. This article will tells you about known issues and workarounds.

> This is the first part of series:
> - [Part 1 - From version 1 to 2.0](/posts/2020-12-31-Timescale-2-with-Hasura-Part-1:-From-Version-1-to-2.0.html)
> - [Part 2 - Multi-node](/posts/2021-01-01-Timescale-2-with-Hasura-Part-2:-Multi-node.html)
> - [Part 3 - High availability](/posts/2021-01-02-Timescale-2-with-Hasura-Part-3:-High-Availability.html)

> The example code is uploaded on [Github](https://github.com/hgiasac/hasura-timescale2-example)

## Migrations and Breaking changes

### Manual works

TimescaleDB SQL API aren't supported by Hasura console. We have to use `Raw SQL` or create migration manually. In theory `hypertable` is the high level of table, we can create it in console. However, `hypertable` uses timestamp or number column as partition key. It requires including that column as primary key. Therefore we have to ignore primary key or use composite keys, and in practice we choose ignoring it. The issue is, Hasura console forces Primary key on table creation [#6235](https://github.com/hasura/graphql-engine/issues/6235). Manual migration creation is unavoidable.

### From 1.x to 2.0

The following table shows syntax comparison between Timescale 1.7 and 2.0:

| 1.7 | 2.0 | Comment |
| --- | --- | ------- |
| add_compress_chunks_policy | add_compression_policy | Add compression policy |
| remove_compress_chunks_policy | remove_compression_policy | Remove compression policy |
| add_drop_chunks_policy | add_retention_policy | Add retention policy |
| remove_drop_chunks_policy | remove_retention_policy | Remove retention policy |
| CREATE VIEW ... WITH (timescaledb.continuous) | CREATE MATERIALIZED VIEW ... (timescaledb.continuous); SELECT add_continuous_aggregate_policy(...);  | create materialized view (continuous aggregate) |
| DROP VIEW <view_name> CASCADE | DROP MATERIALIZED VIEW <view_name> | Drop materialized view (continuous aggregate) | 
| SELECT * FROM timescaledb_information.hypertables WHERE table_name = '<table_name>' | SELECT * FROM hypertable_size('<table_name>') | Get hypertable size |
| ^ | SELECT hypertable_index_size('<table_name>')             | Get hypertable index size |
| ^ | SELECT hypertable_detailed_size('<table_name>')          | Get hypertable index detailed size |  
| SELECT * FROM timescaledb_information.compressed_chunk_stats WHERE hypertable_name = '<table_name>' | SELECT * FROM hypertable_compression_stats('<table_name>') | Get compression stats |

Many of functions and SQL syntaxes are renamed. `timescaledb_information` view structures are very different. You can't list all hypertable sizes with `timescaledb_information.hypertables`, `timescaledb_information.compressed_hypertable_stats` but SELECT each table with `hypertable_size`, `hypertable_compression_stats` functions.

In version 2.0, there are 2 new interesting features:
- Scheduled job. Now you can run cronjob in Postgres to do many things, such as automatically refreshing Materialized view.
- Multi-node. It helps us scaling read/write into multiple data nodes.

Because of scheduled job. Continuous Aggregate Materialized View has a big refactor, as well as breaking changes, that we will explore more in the next section.

### Materialized View (Continuous Aggregate)

Continuous Aggregate Materialized View is Materialized View with auto refresh and partitioning. In migration point of view, high level SQL definition is translated to internal statements.

Input: 

```sql
CREATE MATERIALIZED VIEW conditions_summary_minutely
    WITH (timescaledb.continuous) AS
    SELECT time_bucket(INTERVAL '1 minute', time) AS bucket,
        AVG(temperature),
        MAX(temperature),
        MIN(temperature)
    FROM conditions
    GROUP BY bucket;
```

Output: 

```sql
CREATE OR REPLACE VIEW "public"."conditions_summary_minutely" AS
SELECT
  _materialized_hypertable_5.bucket,
  _timescaledb_internal.finalize_agg(
    'avg(double precision)' :: text,
    NULL :: name,
    NULL :: name,
    '{{pg_catalog,float8}}' :: name [],
    _materialized_hypertable_5.agg_2_2,
    NULL :: double precision
  ) AS avg,
  _timescaledb_internal.finalize_agg(
    'max(double precision)' :: text,
    NULL :: name,
    NULL :: name,
    '{{pg_catalog,float8}}' :: name [],
    _materialized_hypertable_5.agg_3_3,
    NULL :: double precision
  ) AS max,
  _timescaledb_internal.finalize_agg(
    'min(double precision)' :: text,
    NULL :: name,
    NULL :: name,
    '{{pg_catalog,float8}}' :: name [],
    _materialized_hypertable_5.agg_4_4,
    NULL :: double precision
  ) AS min
FROM
  _timescaledb_internal._materialized_hypertable_5
WHERE
  (
    _materialized_hypertable_5.bucket < COALESCE(
      _timescaledb_internal.to_timestamp(_timescaledb_internal.cagg_watermark(5)),
      '-infinity' :: timestamp with time zone
    )
  )
GROUP BY
  _materialized_hypertable_5.bucket
UNION ALL
SELECT
  time_bucket('00:01:00' :: interval, conditions."time") AS bucket,
  avg(conditions.temperature) AS avg,
  max(conditions.temperature) AS max,
  min(conditions.temperature) AS min
FROM
  conditions
WHERE
  (
    conditions."time" >= COALESCE(
      _timescaledb_internal.to_timestamp(_timescaledb_internal.cagg_watermark(5)),
      '-infinity' :: timestamp with time zone
    )
  )
GROUP BY
  (
    time_bucket('00:01:00' :: interval, conditions."time")
  );
```

It's annoyed to reuse definition in console if you lose the original script. Moreover, it doesn't support `CREATE OR REPLACE` to replace current definition, so you need to DROP and CREATE the view.

From TimescaleDB 2.0, the new API and framework for continuous aggregates separates policy from the core functionality ([#2118](https://github.com/timescale/timescaledb/issues/2118)). The continuous view creation syntax is changed:

```sql
-- old syntax
CREATE VIEW conditions_summary_minutely
WITH (timescaledb.continuous,
    timescaledb.refresh_lag = '1h',
    timescaledb.refresh_interval = '1h')
AS
    SELECT time_bucket(INTERVAL '1 minute', time) AS bucket,
        AVG(temperature),
        MAX(temperature),
        MIN(temperature)
    FROM conditions
    GROUP BY bucket;

-- new syntax
CREATE MATERIALIZED VIEW conditions_summary_minutely
    WITH (timescaledb.continuous) AS
    SELECT time_bucket(INTERVAL '1 minute', time) AS bucket,
           AVG(temperature),
           MAX(temperature),
           MIN(temperature)
    FROM conditions
    GROUP BY bucket;

-- the continuous aggregate policy is a separated job
SELECT add_continuous_aggregate_policy('conditions_summary_minutely',
    start_offset => INTERVAL '2 h',
    end_offset => INTERVAL '1 h',
    schedule_interval => INTERVAL '1 h');
```

The new continuous view separates continuous aggregate policy into another function `add_continuous_aggregate_policy` instead of parameters in one `CREATE VIEW` statement. Moreover, it leads another issue:

```sql
CREATE MATERIALIZED VIEW ... WITH DATA cannot run inside a transaction block
```

### FatalError: ... cannot run inside a transaction block

There are functions that requires running in `AUTOCOMMIT` mode. Every statement are automatically committed once submitted. `psql` turns on autocommit by default. If you ever notice that, `psql` executes SQL statements one by one. AFIAK, this error is thrown in these operations:
- Admin commands: CREATE/DROP database, tablespace,...
- CREATE INDEX CONCURRENTLY

Now this error is appeared on new Continuous materialized view too. It isn't problem if you create views with `psql`. However, Hasura migration CLI becomes useless, because it uses transaction to apply migration files.

It isn't impossible to run migration with database driver, if it satisfies both conditions:
- Executes SQL statement without transaction.
- Only one statement per request.

Unfortunately it is hard to change GraphQL Engine source code, because it relates to core functionality of the engine. Finally I came up an idea. It is easier to customize the CLI with native Go's lib/pq option.

This is a hack, so it isn't official in the main upstream. You can download the customized CLI [here](https://github.com/hgiasac/graphql-engine/releases), or try it in [the single node example](https://github.com/hgiasac/hasura-timescale2-example/tree/master/single).

```sh
hasura migrate apply --disable-transaction --database-url "<url>"
```

There are also caveats:
- Migration files are applied right away one by one, not applied as bulk SQL in one transaction. Therefore the migration can't be canceled. For example, you apply 3 migrations `A`, `B`, `C`. If there is any error in migration `C`, `A` and `B` were still applied.
- There must be only 1 SQL statement in `up.sql` if you use special statement such as `CREATE MATERIALIZED VIEW`.

## GraphQL

Because Timescale is an extension of Postgres, it is compatible with GraphQL Engine. However, there are several limitations of Timescale that affect Hasura:
- Hypertable doesn't require Primary key. Therefore `<hypertable_name>_by_pk` queries, mutations and subscriptions are disabled.
- Upsert isn't support in hypertable.
- Hypertable doesn't support foreign key. 
- Although we can create manual relationship between hypertables, the query performance should be considered.

## Console and Metadata

Console and metadata works well with Timescale 2.0. However, Continuous Aggregate View can't be deleted by console UI. Behind the scene it requests `DROP VIEW` SQL execution. The correct statement is `DROP MATERIALIZED VIEW`.

![Drop materialized view](/assets/drop-materialized-view-error.png)

Due to optional Primary key in hypertable, we can't view detail, update and delete row in data table. 

![Timescale Hasura console data table](/assets/timescale-hasura-console-data.png)

Therefore, most of Timescale features have to run in raw SQL. The console doesn't have many help here.

## Should I upgrade?

Yes, if you aren't afraid of migration breaking changes. The new Continuous Materialized View is also another concern.

Timescale 2.0 is also worth to upgrade with Multi-node scaling solution. We will explore in the next part of series: [Part 2 - Multi-node](/posts/2021-01-01-Timescale-2-with-Hasura-Part-2:-Multi-node.html)
