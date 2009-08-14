version := $(shell awk '/^version:/{print $$2}' ../text.cabal)
ghc := ghc
ghcflags := -O -Wall -package bytestring -package QuickCheck \
	-hide-package text -i../dist/build -package-name text-$(version)
lib := ../dist/build/libHStext-$(version).a
lib-srcs := $(shell grep '^  *Data' ../text.cabal | \
                    sed -e 's,\.,/,g' -e 's,$$,.hs,')

all: qc

lib: $(lib)

$(lib): $(lib-srcs:%=../%)
	cd .. && runghc Setup configure --user --prefix=$(HOME)
	cd .. && runghc Setup build

qc: QuickCheckUtils.o Properties.o $(lib)
	$(ghc) $(ghcflags) -o $@ $^

%.o: %.hs
	$(ghc) $(ghcflags) -c -o $@ $<

.PHONY: testdata
testdata: text-testdata.tar.bz2

text-testdata.tar.bz2:
	curl -O http://projects.haskell.org/text/text-testdata.tar.bz2

clean:
	-rm -f *.o *.hi qc
