[![Build
Status](https://travis-ci.org/patrickdoc/hash-graph.svg?branch=master)](https://travis-ci.org/patrickdoc/hash-graph)

A Hashing-based Graph Library
=============================

This library provides a hashing-based graph implementation in Haskell.

I aim to maintain high-quality tests and documentation, high performance,
and a reasonably low memory profile. If any of those seem to be lacking, let me
know. See the
[benchmarks](https://github.com/patrickdoc/hash-graph/blob/master/benchmarks/benchmarks.md)
for performance data.

This library was originally intended to be a more current implementation of
the Functional Graph Library (FGL):
[Hackage](https://hackage.haskell.org/package/fgl),
[GitHub](https://github.com/haskell/fgl),
[Original Site](http://web.engr.oregonstate.edu/~erwig/fgl/haskell/).
However, I made changes to bring the api closer to other container libraries
and to be more consistent in input and output types.

Examples
--------

I've included some example code in the folder `examples`. These demonstrate some
of the practicals of building and using graphs. With `cabal >= 1.24` you should
be able to:

    cabal new-build examples

and then run them from with `dist-newstyle`. I find it easiest to just copy the
location from the final line of building that says something like:

    Linking /PATH/TO/EXE ...

Source Navigation
-----------------

The main module is `Data.HashGraph.Strict`. This defines most of the
functionality at the moment. I'm working on adding common graph algorithms to
`Data.HashGraph.Algorithms`.
