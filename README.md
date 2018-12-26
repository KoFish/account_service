account_service
===============

An exercise to create a simple REST server.

Configuration
-------------

The configuration is located in `config/sys.config`.

 - `pretty_output` (default: `false`) - When `true` the server responds
   with "human readable" json. Otherwise the json is minimized.

Dependencies
------------

 - docker

or

 - Erlang >19
 - rebar3

Usage
-----

Useful commands to test, run and compile the server is located in the
`justfile` (See [Just](https://github.com/casey/just), functionally
similar to `make`).

Docker usage
------------

The included Dockerfile can be used to run rebar3 for the application.
When using `docker-run` command the server can be terminated by
entering `q().` in the REPL.