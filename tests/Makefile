all: Properties bin/Bench

.PHONY: Properties
Properties: Properties.hs QuickCheckUtils.hs
	ghc --make -fforce-recomp Properties

bin/Bench: dist/setup-config *.hs
	./Setup build
	./Setup install

Setup: Setup.lhs
	ghc -O --make $@
	-rm -f $@.o $@.hi

.PHONY: testdata
testdata: text-testdata.tar.bz2

text-testdata.tar.bz2:
	curl -O http://projects.haskell.org/text/text-testdata.tar.bz2

dist/setup-config: text-tests.cabal Setup
	./Setup configure --user --prefix=`pwd`

clean:
	-rm -f *.o *.hi Setup
	-rm -rf dist bin
