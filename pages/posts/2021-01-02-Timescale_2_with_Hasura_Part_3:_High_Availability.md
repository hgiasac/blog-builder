# Timescale 2 with Hasura Part 3 - High Availability

> This is the final part of series:
> - [Part 1 - From version 1 to 2.0](/posts/2020-12-31-Timescale-2-with-Hasura-Part-1:-From-Version-1-to-2.0.html)
> - [Part 2 - Multi-node](/posts/2021-01-01-Timescale-2-with-Hasura-Part-2:-Multi-node.html)
> - [Part 3 - High availability](/posts/2021-01-02-Timescale-2-with-Hasura-Part-3:-High-Availability.html)

> The example code is uploaded on [Github](https://github.com/hgiasac/hasura-timescale2-example)

## High availability

Now we have multi-node cluster. However does it support high availability? If one data node is down, does Timescale cluster still work?

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

Timescale 2.0 provides built-in, native replication support for multi-node. This feature is promising although it is still in development preview.
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

`timescaledb-data1` is still in data nodes list after detached. So in theory we are able to attach again. 

```sql
postgres= SELECT * FROM timescaledb_information.data_nodes;
     node_name     |  owner   |                      options                       
-------------------+----------+----------------------------------------------------
 timescaledb-data1 | postgres | {host=timescaledb-data1,port=5432,dbname=postgres}
 timescaledb-data2 | postgres | {host=timescaledb-data2,port=5432,dbname=postgres}

postgres= SELECT attach_data_node('timescaledb-data1', 'conditions', if_not_attached => true);
ERROR:  [timescaledb-data1]: relation "conditions" already exists
```

Well, another failure. We can't attach existing table from data node. What will we do? Drop the table in the data node and attach again? It 
isn't a good choice.

## Conclusion

Up to now, the best choice for high availability solution for Timescale multi-node feature is streaming replication. For ideal design, every node has 1 replica with fall-over support. 

Native replication feature is still in active development. It has many issues to improve. At least the query planer should be smarter enough to when to ignore failure data nodes when reading data. Anyway, the replication solution is cool and worth to look forward.
