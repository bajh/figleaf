figleaf - a simple configuration manager

When doing local development, especially when working on microservices that have a large number of dependencies - each with many configuration options - mixing and matching environment variables can be a hassle. Environment variables are often grouped together and sometimes you want to swap out an entire group ad hoc, say if you want to go from connecting to a remote development DB to a local one.

This is a simple tool to allow you to adjust your environment on the fly with ease.

Environment variables can be grouped under environments (like "PRD", "STG", or "LOCAL") and namespaces.

You can save new variables locally using the `set` command:

`fig set PRD.api.DB_PASSWORD=slime`

Then you can either set all the variables you've saved in an environment, everything you've saved in a namespace, or a set of specific variables like this:

An environment
`eval $(fig enable PRD)`
A namespace
`eval $(fig enable PRD.api)`
A series of variables
`eval $(fig enable PRD.api.DB_PASSWORD PRD.api.DB_HOST)`