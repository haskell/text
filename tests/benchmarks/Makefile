CC := $(shell icu-config --cc)
CFLAGS := -g $(shell icu-config --cflags)
CPPFLAGS := $(shell icu-config --cppflags)
LDFLAGS := $(CFLAGS) $(shell icu-config --ldflags --ldflags-icuio)

all := fileread

all: $(all)

fileread: fileread.o

clean:
	-rm -f *.hi *.o $(all)
