# text [![Hackage](http://img.shields.io/hackage/v/text.svg)](https://hackage.haskell.org/package/text) [![Stackage LTS](http://stackage.org/package/text/badge/lts)](http://stackage.org/lts/package/text) [![Stackage Nightly](http://stackage.org/package/text/badge/nightly)](http://stackage.org/nightly/package/text)

Haskell library for space- and time-efficient operations over Unicode text.

# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/haskell/text/issues).

The main repo:

```bash
git clone https://github.com/haskell/text
```

To run benchmarks please clone and unpack test files:

```bash
cd text
git clone https://github.com/haskell/text-test-data benchmarks/text-test-data
make -Cbenchmarks/text-test-data
```

# Authors

The base code for this library was originally written by Tom Harper,
based on the stream fusion framework developed by Roman Leshchinskiy,
Duncan Coutts, and Don Stewart.

The core library was fleshed out, debugged, and tested by Bryan
O'Sullivan. Transition from UTF-16 to UTF-8 is by Andrew Lelechenko.
