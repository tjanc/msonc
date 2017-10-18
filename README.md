# msonc
MSONc is an attempt at implementing an MSON parser in Haskell.

## Build & Run

### Using Stack
The [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) is a cross-platform program for developing Haskell projects.
It is aimed at Haskellers both new and experienced.
    
    cd msonc
    stack setup
    stack build

To run msonc,

    stack msonc-exe PATH

where `PATH` is the path to the text file to be parsed for MSON.
For example,

    stack msonc-exe test/fixtures/test1.mson

## Motivations
- maintainable reference implementation of MSON
- determine whether in this domain Haskell's benefits trump its oddities

## Backend
The current `msonc-exe` just prints the AST via `show` to standard output.
