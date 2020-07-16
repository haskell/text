#!/bin/sh

set -ex

dump() {
	"runghc-$1"  Dump.hs > "db-$1.txt"
}

dump 7.0.4
dump 7.2.2
dump 7.4.2
dump 7.6.3
dump 7.8.4
dump 7.10.3

dump 8.0.2
dump 8.2.2
dump 8.4.4
dump 8.6.5
dump 8.8.4
dump 8.10.2

# dump 9.0.1
