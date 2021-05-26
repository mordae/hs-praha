# Praha: Practical Application Toolkit

Praha is an experiment inspired by [RIO](https://hackage.haskell.org/package/rio) to make handy pieces of code that fit well together without being as much opinionated and batteries-included as RIO or Yesod.

It tries to build on the dependencies in as straightforward way as possible so that you can easily use them without the middleman.


## praha - Prelude Replacement

Re-exports large portion of `base` as well as types from `containers`, `unordered-containers`, `text`, `bytestring`, `vector`, `unliftio-core` and `mtl`. Exports `cs` from `string-conversions` for further convenience.


## praha-logger - Monad Transformer for Logging

`MonadLogger` class and a `LoggerT` monad transformer for convenient use of the `fast-logger` infrastructure.


## praha-config: Practical Configuration Management

Practical configuration management based on process environment, good enough for typical web applications.


## praha-migrate: Practical PostgreSQL Schema Migrations

Practical PostgreSQL schema migrations compatible with compile-time file embedding.
