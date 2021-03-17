# `text`: Fast, packed Unicode strings, using stream fusion [![Hackage](http://img.shields.io/hackage/v/text.svg)](https://hackage.haskell.org/package/text)

This package provides the Data.Text library, a library for the space-
and time-efficient manipulation of Unicode text in Haskell.

# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/haskell/text/issues).

The main repo:

```bash
git clone git://github.com/haskell/text.git
```

To run benchmarks please clone and unpack test files:

```bash
git clone https://github.com/bos/text-test-data benchmarks/text-test-data
cd benchmarks/text-test-data
make
```

# Authors

The base code for this library was originally written by Tom Harper,
based on the stream fusion framework developed by Roman Leshchinskiy,
Duncan Coutts, and Don Stewart.

The core library was fleshed out, debugged, and tested by Bryan
O'Sullivan <bos@serpentine.com>, and he is the current maintainer.
