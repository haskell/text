version := $(shell awk '/^version:/{print $$2}' ../text.cabal)
ghc := ghc
ghc-base-flags := -O -funbox-strict-fields \
	-package bytestring -package QuickCheck -package test-framework \
	-package test-framework-quickcheck -ignore-package text \
ghc-base-flags += -Wall -fno-warn-orphans -fno-warn-missing-signatures
ghc-flags := $(ghc-base-flags) -i../dist/build -package-name text-$(version)
ghc-hpc-flags := $(ghc-base-flags) -fhpc -fno-ignore-asserts -odir hpcdir \
	-hidir hpcdir -i..
lib := ../dist/build/libHStext-$(version).a
lib-srcs := $(shell grep '^  *Data' ../text.cabal | \
                    sed -e 's,\.,/,g' -e 's,$$,.hs,')

cabal := $(shell which cabal 2>/dev/null)

all: qc coverage

lib: $(lib)

$(lib): $(lib-srcs:%=../%)
ifeq ($(cabal),)
	cd .. && runghc Setup configure --user --prefix=$(HOME)
	cd .. && runghc Setup build
else
	cd .. && cabal configure
	cd .. && cabal build
endif

Properties.o: QuickCheckUtils.o

QuickCheckUtils.o: $(lib)

qc: Properties.o QuickCheckUtils.o
	$(ghc) $(ghc-flags) -threaded -o $@ $^ $(lib)

qc-hpc: Properties.hs QuickCheckUtils.hs $(lib-srcs:%=../%)
	-mkdir -p hpcdir
	@rm -f $@.tix
	$(ghc) $(ghc-hpc-flags) -ihpcdir --make -threaded -o $@ $<

coverage: qc-hpc-html/hpc_index.html

qc-hpc-html/hpc_index.html: qc-hpc
	./qc-hpc -a 500 +RTS -N2
	hpc markup qc-hpc --exclude=Main --exclude=Properties \
	  --exclude=QuickCheckUtils --srcdir=.. --srcdir=. --destdir=$(dir $@)

%.o: %.hs
	$(ghc) $(ghc-flags) -c -o $@ $<

hpcdir/%.o: %.hs
	$(ghc) $(ghc-hpc-flags) --make -c -o $@ $<

.PHONY: testdata
testdata: text-testdata.tar.bz2

text-testdata.tar.bz2:
	curl -O http://projects.haskell.org/text/text-testdata.tar.bz2

clean:
	-rm -rf *.o *.hi *.tix qc qc-hpc hpcdir .hpc qc-hpc-html
