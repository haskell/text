version := $(shell awk '/^version:/{print $$2}' ../text.cabal)
ghc := ghc
ghc-opt-flags = -O0
ghc-base-flags := -funbox-strict-fields -hide-all-packages \
	-package base -package mtl -package random -package directory \
	-package criterion -package deepseq -DASSERTS -DHAVE_DEEPSEQ \
	-package bytestring -ignore-package text \
	-fno-ignore-asserts
ghc-test-flags := -package QuickCheck -package test-framework -package deepseq \
	-package test-framework-quickcheck2 -package test-framework-hunit \
	-package HUnit
ghc-base-flags += -Wall -fno-warn-orphans -fno-warn-missing-signatures
ghc-flags := $(ghc-base-flags) -i../dist/build -package-name text-$(version)
ghc-hpc-flags := $(ghc-base-flags) -fhpc -fno-ignore-asserts -odir hpcdir \
	-hidir hpcdir -i..
lib := ../dist/build/libHStext-$(version).a
lib-srcs := $(shell grep '^  *Data' ../text.cabal | \
                    sed -e 's,\.,/,g' -e 's,$$,.hs,')

cabal := $(shell which cabal 2>/dev/null)

all: bm qc coverage regressions

lib: $(lib)

$(lib): $(lib-srcs:%=../%)
ifeq ($(cabal),)
	cd .. && runghc Setup configure --user --prefix=$(HOME)
	cd .. && runghc Setup build
else
	cd .. && cabal configure
	cd .. && cabal build
endif

Properties.o Regressions.o qc qc-hpc: ghc-flags += $(ghc-test-flags)
Properties.o: QuickCheckUtils.o SlowFunctions.o TestUtils.o

QuickCheckUtils.o: $(lib)

qc: Properties.o QuickCheckUtils.o SlowFunctions.o TestUtils.o
	$(ghc) $(ghc-flags) -threaded -o $@ $^ $(lib)

sb: SearchBench.o SlowFunctions.o
	$(ghc) $(ghc-flags) -threaded -o $@ $^ $(lib)

qc-hpc: Properties.hs QuickCheckUtils.hs SlowFunctions.hs TestUtils.hs $(lib-srcs:%=../%)
	-mkdir -p hpcdir
	@rm -f $@.tix
	$(ghc) $(ghc-hpc-flags) $(ghc-test-flags) $(ghc-opt-flags) -ihpcdir \
	  --make -threaded -o $@ $<

stdio-hpc: StdioCoverage.hs $(lib-srcs:%=../%)
	-mkdir -p hpcdir
	@rm -f $@.tix
	$(ghc) $(ghc-hpc-flags) $(ghc-test-flags) $(ghc-opt-flags) -ihpcdir \
	  --make -threaded -o $@ $<

coverage: coverage-html/hpc_index.html

coverage-html/hpc_index.html: qc-hpc stdio-hpc
	@rm -f qc-hpc.tix stdio-hpc.tix coverage.tix
	./qc-hpc -a 100 +RTS -N
	bash ./cover-stdio.sh
	hpc combine --output=coverage.tix --exclude=Main \
	  qc-hpc.tix stdio-hpc.tix
	hpc markup coverage --exclude=Main --exclude=Properties --exclude=Main \
	  --exclude=Data.Text.Fusion.CaseMapping --exclude StdioCoverage \
	  --exclude=SlowFunctions --exclude=TestUtils \
	  --exclude=QuickCheckUtils --srcdir=.. --srcdir=. --destdir=$(dir $@)
	@echo xdg-open $@

Regressions.o: TestUtils.o

regressions: Regressions.o TestUtils.o
	$(ghc) $(ghc-test-flags) -o $@ $^ $(lib)

Benchmarks.o: ghc-opt-flags = -O
bm Benchmarks.o: ghc-flags += -package utf8-string
bm: Benchmarks.o
	$(ghc) $(ghc-flags) -o $@ $^ $(lib)

SlowFunctions.o: ghc-opt-flags = -O2
SearchBench.o: ghc-opt-flags = -O
%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c -o $@ $<

hpcdir/%.o: %.hs
	$(ghc) $(ghc-hpc-flags) $(ghc-opt-flags) --make -c -o $@ $<

.PHONY: testdata
testdata: text-testdata.tar.bz2

text-testdata.tar.bz2:
	curl -O http://projects.haskell.org/text/text-testdata.tar.bz2

clean:
	-rm -rf *.o *.hi *.tix bm qc qc-hpc stdio-hpc hpcdir .hpc coverage-html
