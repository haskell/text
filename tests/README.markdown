Tests
=====

This directory contains the tests for the Text library. To run these
tests, you will need the test data from one of the following
locations:

* Mercurial master repository:
  [bitbucket.org/bos/text-test-data](https://bitbucket.org/bos/text-test-data)

* Git mirror repository:
  [github.com/bos/text-test-data](https://github.com/bos/text-test-data)

You should clone that repository to the same directory as this `README`
file, then run `make` to uncompress the test files.  Many tests will
fail if the test files are missing.

There are two categories of tests: functional tests (including QuickCheck
properties), and benchmarks.

Functional tests
----------------

The functional tests are located in the `tests` subdirectory. An overview of
what's in that directory:

    scripts           Various utility scripts
    src               Source files of the testing code
    text-tests.cabal  Cabal file which compiles all benchmarks
    Makefile          Has targets for common tasks

The `text-tests.cabal` builds:

- A copy of the text library, sharing the source code, but exposing all internal
  modules, for testing purposes
- The different test suites

To compile, run all tests, and generate a coverage report, simply use `make`.

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

And run the ones you want to inspect. If you want to configure the benchmarks
further, the exact parameters can be changed in `src/Data/Text/Benchmarks.hs`.
