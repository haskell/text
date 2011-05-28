Tests
=====

This directory contains the tests for the Text library. To run these tests, you
will need the test data from:

    http://projects.haskell.org/text/text-testdata.tar.bz2

You should extract that archive to the same directory as this README (some tests
rely on this).

There are two categories of tests: functional tests (including QuickCheck
properties), and benchmarks.

Functional tests
----------------

TODO

Benchmarks
----------

The benchmarks are located in the `benchmarks` subdirectory. An overview of
what's in that directory:

    python            Python implementations of some benchmarks
    ruby              Ruby implementations of some benchmarks
    src               Source files of the haskell benchmarks
    benchmarks.cabal  Cabal file which compiles all benchmarks
    Makefile          Has targets for common tasks

To compile the benchmarks, navigate to the `benchmarks` subdirectory and run
`cabal configure && cabal build`. Then, you can run the benchmarks using:

    ./dist/build/benchmarks/benchmarks

However, since there quite a lot of benchmarks, you usually don't want to run
them all. Instead, use the `-l` flag to get a list of benchmarks:

    ./dist/build/benchmarks/benchmarks

And run the ones you want to inspect.
