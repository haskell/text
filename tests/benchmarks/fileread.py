#!/usr/bin/env python

import sys

def string(name):
    print len(open(name).read())

def lazystring(name):
    fp = open(name)
    n = 0
    d = True
    bs = 128 * 1024
    read = fp.read
    while d:
        d = len(read(bs))
        n += d
    print n

def lazytext(name):
    fp = open(name)
    n = 0
    d = True
    bs = 128 * 1024
    read = fp.read
    while d:
        s = read(bs)
        d = len(s.decode('utf-8', 'replace'))
        n += d
    print n

def text(name):
    print len(open(name).read().decode('utf-8', 'replace'))

if sys.argv[1] == 'bs':
    string(sys.argv[2])
if sys.argv[1] == 'lbs':
    lazystring(sys.argv[2])
elif sys.argv[1] == 'lazytext':
    lazytext(sys.argv[2])
elif sys.argv[1] == 'text':
    text(sys.argv[2])
