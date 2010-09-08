CC := $(shell icu-config --cc)
CFLAGS := -g $(shell icu-config --cflags)
CPPFLAGS := $(shell icu-config --cppflags)
LDFLAGS := $(CFLAGS) $(shell icu-config --ldflags --ldflags-icuio)
ghc := ghc

all := FileRead FileRead_prof Replace Replace_prof fileread_c

all: $(all)

%: %.hs
	$(ghc) -O --make -o $@ $^

%_prof: %.hs
	$(ghc) -prof -auto-all -O --make -o $@ $^

clean:
	-rm -f *.hi *.o $(all)
