This is a simple CLI tool I wrote because I wanted to learn how to do something "real" with Haskell. The tool's goal is to make it easy to preserve and swap out groups of related environment variables. It lets you organize groups of key-value sets in a tree structure that is saved to a configuration file. You can then retrieve all of the key-value pairs stored in a particular branch of the configuration tree.

When working on microservices that have to interface with several external systems, I often want to do things like swap out one remote development backend I'm connecting to with a local version of that backend while continuing to use remote development backends for my other dependencies. This usually requires setting a couple different env variables for the backend I want to swap out, like a `HOST`, `PORT`, `API_KEY`, etc. Other times, I might want to swap out all of my environment variables to point to local services instead of remote ones.

Figleaf has two commands: `set`, to store key-values in a namespace path, and `apply`, to read one or more env vars, or an entire namespace sub-tree, from your config so they can be applied to your environment.

## Example

Imagine you're doing work on a service that has a MySQL database. For development and testing, you probably run a local database that you can trash and recreate at will, but you might also have a remote development database instance with a larger history of more realistic test data. For each of those environments, you probably have a set of env vars like this:

```
DB_NAME=users
DB_HOST=127.0.0.1
DB_PORT=3306
DB_USER=app
DB_PASSWORD=123
```

Maybe you also have some upstream service that you need to call, and normally you make requests to a remote development instance of the upstream, but sometimes you also do development on the upstream service at the same time as you're working on the downstream service and want to verify the compatibility of the two services. That upstream service might have a bunch of variables likes:

```
API_HOST_PORT=127.0.0.1:8080
API_KEY=dogs
API_REQUEST_TIMEOUT=2s
```

In these two cases, you could use Figleaf to store the env variables under two different levels of namespaces, one for the DEV/LOCAL dependencies and one for each of the two categories of variables:

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

You can set some of the above env vars in your Figleaf configuration using a command like `figleaf set db.DB_NAME=users db.DEV.DB_HOST=dbhost upstream.DEV.UPSTREAM_API_KEY=dogs`. Notice that namespace paths are represented by prefixing the environment variable key and value with a dot-separated path.

If you wanted to run your service using the local DB and the remote dev upstream, you could apply a group of env vars to your environment with `$(figleaf apply db.LOCAL upstream.DEV)`. If you then wanted to start running the upstream API locally, you could run `$(figleaf apply upstream.LOCAL)`.

Note that in this example, you could also use `DEV` and `LOCAL` as the top-level tree nodes, each having an `upstream` and `db` tree beneath them.

## Building

You'll need [Cabal](https://www.haskell.org/cabal/).

You can either compile and run using `cabal run [set|apply] arg1 [arg2...]` or you can use `cabal install` to put the binary wherever is specified in `~/.cabal/config`. By default, the binary goes to `~/.cabal/bin/FigLeaf` on Linux.

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

Parsing this is the most complex aspect of the program and feels like it could be improved also pending my developing a more mature understanding of monad transformers.

### Improvements

What happens if there's a conflict where you're applying the same key multiple times? Right now the result will just be seemingly arbitrary, so it's incumbent on the user to avoid letting this happen. Two approaches could be: give you the opportunity to override things or make choices; reject any attempt to apply a tree with multiple options.

### Character escaping

If you have newlines or tabs in environment variables now they'll mess everything up and I don't even prevent you from doing this so you're free to just totally bork your config beyond all recognition!

### Efficiency

Sometimes you may want to update multiple key-value leaves in a single branch of the tree. Right now, this requires walking the whole tree to get to and replace each of those key-value pairs separately, but it would be more efficient to walk the tree up until the least distant shared ancestor of the two leaves, then replace that ancestor with a tree in which both key-value pairs has been updated. This would reduce the number of times the tree above the shared ancestor has to be walked. This would have the greatest effect on performance if trees grew very deep, but that seems generally unlikely.

The way the config file is serialized now uses one \t character to represent each level of depth in the tree, meaning one byte per key-value pair per level of depth. If we're eight levels deep in the tree, it costs 8 byte of storage to encode each key-value pair, plus the storage of the key-value pair itself. Instead, it would probably make sense to just encode this information as a binary uint8 (since there probably will not be more than 256 levels to the tree).

It would also be cool to process the config file in a streaming manner so the whole thing wouldn't need to be read into memory at once.
