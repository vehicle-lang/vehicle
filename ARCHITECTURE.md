NOTE: This file contains assorted notes on the design of the Vehicle compiler which should be ported to CONTRIBUTING.md.

# Architecture

- Parser for external language is generated via BNCF (version 2.9.0 and above)

- Syntax-driven elaboration to internal language

- Scope checking

- Type checking (includes some type-driven elaboration)

## Compiling to VNNLIB

- Normalisation

- Compile

## Compiling to Agda

- No normalisation

- Compile

## Compiling to loss-functions

- Normalise (?)

- Compile



## Parsers

- The parsers for Vehicle are generated via [`BNFC`](https://bnfc.readthedocs.io/)
  grammars located in the `vehicle-syntax/src/Vehicle/Syntax` folder.

## Logging

- Logs can be enabled by providing the `--logging` option on the command line.

- In the case of an internal developer error, logs may not be printed. In this case you
  can add a `traceShow text $` in front of the `do` in the `logDebug` in `Vehicle.Prelude.Logging`.

## Documentation

The documentation is hosted by ReadTheDocs (RTD). The documentation is automatically rebuilt.

## Coding conventions

- In order to maintain flexibility in adding extra fields to `Arg` and `Binder`
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.
