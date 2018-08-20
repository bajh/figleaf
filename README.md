This is a tool written with the motivation of learning how to do something real with Haskell. It's a CLI that makes it easy to store and retrieve groups of environment variable, categorized by namespaces that are organized into a tree.

Autoenv and direnv are great for making it easy to switch your environment based on the project you're working on, but I find that when I'm working on microservices that have multiple dependencies, I often have groups of variables that all pertain to one of those dependencies, and during development I may want to swap out a whole group of those env vars at once.

For example, imagine you're doing work on a service that has a MySQL database For development, you probably run a local database that you can trash and recreate at will, but you might also have a remote development database instance with a larger history of more realistic test data. For each of those environments, you probably have a set of env vars like this:

```
DB_NAME=users
DB_HOST=127.0.0.1
DB_PORT=3308
DB_USER=app
DB_PASSWORD=123
```

Maybe you also have some upstream service that you need to call, and normally you make requests to a remote development instance of the upstream, but sometimes you also do development on the upstream service at the same time as you're working on the downstream service and want to verify their compatibility. That upstream service might have a bunch of variables likes:

```
API_HOST_PORT=127.0.0.1:8080
API_KEY=dogs
API_REQUEST_TIMEOUT=2s
```

In these two cases, you could store the env variables in Figleaf, under two different levels of namespaces, one for the DEV/LOCAL dependencies and one for each of the two categories of variables:

```
                     ____________default namespace___________________
                    /                                                 \
         ________db_____________                                _________upstream_______
        /      |                \                              /                         \
DB_NAME=users  DEV               LOCAL                         DEV                        LOCAL
               |                     |                         |-API_KEY=dogs             |-API_KEY=cats
               |-DB_HOST=dbhost      |-DB_HOST=localhost       |-API_HOST_PORT=apihost    |-API_HOST_PORT=localhost:7777
               |-DB_PORT=3306        |-DB_PORT=3308             
               |-DB_USER=app         |-DB_USER=app2
               |-DB_PASSWORD=blah    |-DB_PASSWORD=blah2
```

Figleaf has two commands: `set`, to update your stored config namespaces, and `apply`, to read one or more env vars, or a namespace, from your config so they can be applied to your environment.

So you could set some of the above env vars in your configuration using a command like `figleaf set db.DB_NAME=users db.DEV.DB_HOST=dbhost upstream.DEV.UPSTREAM_API_KEY=dogs`

Then if you wanted to run your service using the local DB and the remote dev upstream, you could compile a set of env vars to apply to your environment with `figleaf apply db.LOCAL upstream.DEV`. If you then wanted to start running the upstream API locally, you could run `figleaf apply upstream.LOCAL`.

Note that in this example, you could also use `DEV` and `LOCAL` as the top-level tree nodes, each having an `upstream` and `db` tree beneath them.

## Implementation

The tree of all stored env vars is saved in a file where the levels in the tree are represented by a series of \t characters. For example, the above would be stored as:

```
db
        DB_NAME=users
        DEV
                DB_HOST=dbhost
        LOCAL
                DB_HOST=localhost
upstream
        DEV
                API_KEY=dogs
        LOCAL
                API_KEY=cats
```

Parsing this is the most interesting aspect of the program and feels like it could be improved also.

## Improvements

### Applying changes to env

This tool is intended to make it easy to change your environment. But bash does not allow a sub-process (the figleaf command) to modify its parent process, so it can't do that directly. At the moment, the apply command simply prints out a list of environment variables like this

```
DB_PASSWORD=123
DB_NAME=users
```

that the calling shell can then apply. For example, `export $(figleaf PRD)` could be used to do this. Another feature I want to add is the ability to wrap a user-supplied command in a parent process so that figleaf can just set the environment you've asked it to apply and then call your command as a sub-process so that those environmental changes will be there. The downside, of course, would be that the changes would only persist for the lifetime of that one figleaf process, but I also think there could be some helpful add-ons that could make this usable.

### Conflicts

What happens if there's a conflict where you're applying the same key multiple times? Right now the result will just be seemingly arbitrary, so it's incumbent on the user to avoid letting this happen. Two approaches could be: give you the opportunity to override things or make choices; reject any attempt to apply a tree with multiple options.

### Efficiency

Sometimes you may want to update multiple key-value leaves in a single branch of the tree. Right now, this requires walking the whole tree to get to and replace each of those key-value pairs separately, but it would be more efficient to walk the tree up until the least distant shared ancestor of the two leaves, then replace that ancestor with a tree in which both key-value pairs has been updated. This would reduce the number of times the tree above the shared ancestor has to be walked. This would have the greatest effect on performance if trees grew very deep, but that seems generally unlikely.

It would also be cool to process the config file in a streaming manner.
