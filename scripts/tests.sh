#!/bin/sh

set -ex

runtest() {
  HC=$1
  shift

  # EDIT last line to pass arguments

  cabal run text-tests:test:tests \
      --project-file=cabal.tests.project \
      --builddir="dist-newstyle/$HC" \
      --with-compiler="$HC" \
      -- "$@"
}

runtest ghc-8.10.2 "$@"
runtest ghc-8.8.4  "$@"
runtest ghc-8.6.5  "$@"
runtest ghc-8.4.4  "$@"
runtest ghc-8.2.2  "$@"
runtest ghc-8.0.2  "$@"

runtest ghc-7.10.3 "$@"
runtest ghc-7.8.4  "$@"
runtest ghc-7.6.3  "$@"
runtest ghc-7.4.2  "$@"
runtest ghc-7.2.2  "$@"
runtest ghc-7.0.4  "$@"
