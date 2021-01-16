# GraphQL Federation: Challenges and Future

## What it GraphQL Federation, again?

If you are familiar with GraphQL, this buzzword isn't new. In brief, GraphQL Federation is a microservices architecture for GraphQL that splits a monolithic graph into multiple smaller GraphQL services. 

Benefit

## Challenges

### Schema conflicts

#### The issue

To unify multiple graphs into one big graph, GraphQL gateway introspects all schemas from each graph, then merges them into a big schema. The gateway receives requests and routing them to target services. In really, there are chances that 2 or more teams define same type, with different fields, arguments in their schemas. This causes errors and even fails to start GraphQL gateway.

This issue is common on microservices-liked architectures. However, it's more serious in GraphQL Federation. RESTful API gateway only gets conflicts at URL path and method, and it can easily solve by renaming URL path, or modularize entire resource with subpath, e.g `/user => /module1/user`. In contrast, GraphQL schemas conflict not only operations, but also types. These types usually glue together with relationship and are reused in many places. It takes time and workload to resolve without breaking changes.

#### Solution and Best Practices

Unfortunately, there isn't official solution from popular frameworks. Apollo team instead introduces [Apollo Studio service](https://www.apollographql.com/docs/federation/managed-federation/overview/) which manages, validate and version GraphQL schema registry. It helps developers ensuring stability of the final schema before deploying to production. [Pipedrive] team also develops open source tool named [graphql-schema-registry](https://github.com/pipedrive/graphql-schema-registry) which is alternative to Apollo Studio.

In theory, it is possible if GraphQL gateway can rename schema types. However it isn't easy to implement. It seems easier to add prefix to all remote schema types. Hasura also has a discussion for this solution. You can track it at the issue ([#5863](https://github.com/hasura/graphql-engine/issues/5863)). New ideas are welcome!

To avoid this issue, the best practice is having an API architect who coordinates with all development teams and designs unified, high level API documentation. However, sometimes it is unavoidable. There are cases that we need to integrate with third party graphs with unpredicted conflicts. Gatsby is an example with multiple GraphQL data source plugins.

### Multi-layer authorization

### Tracing

### Performance

## The Future

### @defer and @stream

### Smart GraphQL Gateway
