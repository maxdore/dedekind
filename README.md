# Dedekind - a solver for boundary problems in Cubical Agda

This Haskell tool can be used to resolve proof obligations in [Cubical
Agda](https://agda.readthedocs.io/en/v2.6.4.1/language/cubical.html) which
involve connections and Kan compositions.

## Installation

The solver is written in Haskell and can be built locally with cabal:
```cabal build```

## Usage

The solver can be called on `.cube` files, which contain a cubical cell context
and boundary problems over the context. For example, we can define a cell
context with two paths as follows, and provide a list of boundary problems:

```
p : (i)[]
q : (i)[i = 0 -> p(1)]
---
? : (i)[i = 0 -> p(1) | i = 1 -> p(0)]
? : (i)[i = 0 -> p(0) | i = 1 -> q(1)]
```

See the [/examples](https://github.com/maxdore/dedekind/tree/main/examples)
folder for more examples.

A basic call to the solver is with `cabal run dedekind -- -f FILE.cube` (if you
have installed the solver on your path, you can also call `dedekind -f
FILE.cube`). The solver outputs code in Cubical Agda, which can be copied into a
formalisation.

For example, we can solve all problems in the file `examples/path.cube` with the
following command:

```cabal run dedekind -- -f examples/path.cube```

You can optionally provide the flag `-v` to get more information about the
solving process, and `-t 10` to specify a timeout in seconds. 

## Benchmark

The file `benchmark.sh` will solve all files in the folder `/examples` folder
and will provide some benchmarking information. Simply call `sh benchmark.sh` to
run the benchmarking suite.
