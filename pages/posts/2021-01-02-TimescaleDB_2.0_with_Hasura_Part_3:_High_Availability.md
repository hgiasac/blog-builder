# TimescaleDB 2.0 with Hasura Part 3 - High Availability

![Hasura TimescaleDB](/assets/timescaledb/timescale-hasura.png)

 Performance and High availability are critical challenges in production. Although TimescaleDB solves performance problems, we still need to care about remain parts. In this article, I suggest some high availability setup for single-node as well as explore multi-node replication solutions,

> This is final part of the series:
> - [Part 1 - From 1.x to 2.0](/posts/2020-12-31-TimescaleDB-2.0-with-Hasura-Part-1:-From-1.x-to-2.0.html)
> - [Part 2 - Multi-node](/posts/2021-01-01-TimescaleDB-2.0-with-Hasura-Part-2:-Multi-node.html)
> - [Part 3 - High availability](/posts/2021-01-02-TimescaleDB-2.0-with-Hasura-Part-3:-High-Availability.html)

> The example code is uploaded on [Github](https://github.com/hgiasac/hasura-timescaledb2-example)

## Single-node

TimescaleDB is an extension of Postgres, so you can use any replicated solution of Postgres. The common solution is streaming replication, combine with [repmgr](https://repmgr.org/) or [Patroni](https://patroni.readthedocs.io/en/latest) for automatic failover.

If you use Kubernetes, you can install [official helm repository of TimescaleDB](https://github.com/timescale/timescaledb-kubernetes) that use Patroni with 3 nodes by default.

![TimescaleDB single node Kubernetes diagram](/assets/timescaledb/timescale-create-distribution-hypertable-error.png)
TimescaleDB single node Kubernetes diagram. Source: https://github.com/timescale/timescaledb-kubernetes

[Timescale Cloud](https://www.timescale.com/cloud) and [Timescale Forge](https://forge.timescale.com/) are also great options. You can deploy high availability cluster easily with several clicks, and no worry about system management.

Hasura works well with TimescaleDB replication. However, it's better to use Hasura Cloud/Pro to take advantage of [read replicas](https://hasura.io/docs/1.0/graphql/cloud/read-replicas.html). Hasura Cloud can load balance queries and subscriptions across read replicas while sending all mutations and metadata API calls to the master.

![TimescaleDB single-node replication](/assets/timescaledb/timescale-single-replication.png) 

## Multi-node

Now we have multi-node cluster. However does it support high availability? If one data node is down, does TimescaleDB cluster still work?

With this test case, I stop data node `timescaledb-data1`, then test several operations:
- INSERT works. The access node can know status of data nodes , and distribute to alive nodes.
- However SELECT, UPDATE and DELETE operations are failed. 

```sql
postgres= INSERT INTO conditions VALUES (NOW(), 'US', 13, 1.3);
INSERT 0 1
postgres= SELECT * FROM conditions ORDER BY time DESC LIMIT 1;
ERROR:  could not connect to "timescaledb-data1"
DETAIL:  could not translate host name "timescaledb-data1" to address: Name does not resolve
```

So, it only support high availability on INSERT. The query planner isn't smart enough to exclude outage data nodes. Therefore, you have to ensure high availability for all data nodes.

I came up with an idea. Can we temporarily detach outage data node? No, it can't, even with `force` option.

```sql
postgres= SELECT detach_data_node('timescaledb-data1', force => true);
ERROR:  insufficient number of data nodes
DETAIL:  Distributed hypertable "conditions" would lose data if data node "timescaledb-data1" is detached.
HINT:  Ensure all chunks on the data node are fully replicated before detaching it.
```

## Native Replication

TimescaleDB 2.0 provides built-in, native replication support for multi-node. This feature is promising although it is still in development preview.
We aren't required any additional setup to use native replication. Just need to set `replication_factor` argument with integer value. This argument represents the number of data nodes that the same data is written to. The value must be in between 1 and total data nodes. The default value is 1. 

Let's see how many rows are inserted into 2 data nodes:

| access | timescaledb-data1 | timescaledb-data2 |
| ------ | ----------------- | ----------------- |
| 1,000,000  | 1,000,000 | 1,000,000 |

From above table, the number of rows are equal. That means the data is copied into both data nodes. This ensures the consistency of data. The trade off is slower performance. Data nodes take more work to replicate data. The delay depends on the number of replication factors.

| Operation | replication_factor = 1 (ms) | replication_factor = 2 (ms) | 
| --------- | --------------------------- | --------------------------- |
| INSERT    | 12,700.597             | 16,083.890             |      
| UPDATE    | 184,707.671            | 259,930.983            |
| SELECT    | 2,011.194              | 2,023.870              |
| DELETE    | 159,165.419            | 248,658.194            |

Surprisingly the `SELECT` performance is comparable with non-replication mode. The query planner knows how to include only one replica of each chunk in the query plan.
However, the outage behavior isn't correct as I thought. The `SELECT` query still throws error after stopping one data node.

```sql
postgres= SELECT COUNT(*) FROM conditions;
ERROR:  could not connect to "timescaledb-data1"           
DETAIL:  could not translate host name "timescaledb-data1" to address: Name does not resolve
```

Fortunately we can detach the data node now, with `force`.

```sql
-- Still error if you don't force detachment
postgres= SELECT detach_data_node('timescaledb-data1');
ERROR:  data node "timescaledb-data1" still holds data for distributed hypertable "conditions"

postgres= SELECT detach_data_node('timescaledb-data1', force => true);
WARNING:  distributed hypertable "conditions" is under-replicated
DETAIL:  Some chunks no longer meet the replication target after detaching data node "timescaledb-data1".
WARNING:  insufficient number of data nodes for distributed hypertable "conditions"
DETAIL:  Reducing the number of available data nodes on distributed hypertable "conditions" prevents full replication of new chunks.

-- the SELECT query works now
postgres= select COUNT(*) FROM conditions;

  count    
---------
 1000000
```

Unfortunately, we can't attach existing table from data node. What will we do? Drop the table in the data node and attach again? It 
isn't a good choice.

```sql
postgres= SELECT attach_data_node('timescaledb-data1', 'conditions', if_not_attached => true);
ERROR:  [timescaledb-data1]: relation "conditions" already exists
```

Up to now, the best choice for high availability solution for TimescaleDB multi-node feature is streaming replication. For ideal design, every node has 1 replica with failover support. 

![TimescaleDB multi-node replication](/assets/timescaledb/timescale-multinode-replication.png)

You also can sign up [Timescale Forge](https://docs.timescale.com/latest/getting-started/exploring-forge/forge-multi-node) to deploy Multi-node. At least with enterprise supports, Timescale team can help you solve many replication problems.

## Caveats

The common limitation of replication is data latency between master and replicas, especially when the master node received large amount of write requests. Although we can set synchronous replication mode, it isn't recommended. Write performance will be degrade radically. However it is acceptable if you don't requires real-time metrics.

Because of asynchronous replication, schema sync is also a problem. Sometimes we create a table on master, but it doesn't exist on replicas. In that case, there isn't another option than waiting and even reload metadata on GraphQL Engine.

## Conclusion

TimescaleDB single-node replication isn't different from vanilla Postgres. TimescaleDB team also provides high availability templates for common deployment system (Kubernetes) as cloud solutions, depending on our use cases.

Native replication feature is still in active development. It has many issues that need to be improved. At least the query planer should be smarter enough to when to ignore failure data nodes when reading data. Anyway, the replication solution is cool and worth to look forward.

You can give Hasura Cloud a try to enable Read Replicas's power. Unfortunately current OSS version doesn't support this feature. However, there will be fun things to play with read replicas with upcoming versions. Let's keep in touch with [Hasura blog](https://hasura.io/blog/) and [Events](https://hasura.io/events/) for new plans and cool demos about Hasura!