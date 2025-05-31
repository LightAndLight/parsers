parsers
=======

[![Hackage](https://img.shields.io/hackage/v/parsers.svg)](https://hackage.haskell.org/package/parsers) [![Build Status](https://github.com/ekmett/parsers/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/parsers/actions?query=workflow%3AHaskell-CI)

Goals
-----

This project provides convenient combinators for working with and building parsing combinator libraries.

Given a few simple instances, you get access to a large number of canned definitions.

Structure
---------

The `parsers` ecosystem is split into several packages:

* `parsers`: Core package with parser abstractions and instances for base's `ReadP`
* `parsers-attoparsec`: Instances for the `attoparsec` library
* `parsers-binary`: Instances for `binary`'s `Get` parser
* `parsers-parsec`: Instances for the `parsec` library

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

