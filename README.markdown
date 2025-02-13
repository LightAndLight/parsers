parsers
-------

Motivation
----------

This repository is a fork of Ed Kmett's [`parsers`](https://github.com/ekmett/parsers) package.

The fork moves the type class instances for `attoparsec` and `parsec` into separate packages.
This makes the `parsers` package more modular.

The original `parsers` package depends on `attoparsec` and `parsec` by default.
If you use `parsers` to write a parser that you only run via `attoparsec`, then your project will still depend on `parsers`, which has become a spurious dependency for your program and potentially its dependents.
Additionally, if you depend on `parsers` in order to write instances for a new parser combinator library (like I did for [`sage`](https://github.com/LightAndLight/sage)), then you will transitively depend on both `attoparsec` and `parsec`.

The original `parsec` package has flags that can disable the `attoparsec` and `parsec`, but using flags for this doesn't work out in general[^1].
Library authors would need to be able to set flags for their dependencies, and `cabal-install` would have to coherently combine flags that were set by different dependencies.
Sounds pretty complex, and `cabal-install` doesn't need more complexity.

Factoring out type class instances into separate packages (`parsers` -> `{parsers,parsers-attoparsec,parsers-parsec}`) returns some agency to library authors.
Instead of using flags to choose their parser combinator dependencies, they explicitly depend on instances via a `parsers-*` package.
These "instances packages" preserve type class coherency by avoiding orphan instances;
the `parsers` type class instances are given for a newtype over a concrete parser.

[^1]: Relevant discussions:
    * <https://github.com/haskell/cabal/issues/2821>
    * <https://github.com/haskell/cabal/issues/8643>
