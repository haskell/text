#!/usr/bin/env python

import sys, time

def timeit(f):
    start = time.time()
    f()
    end = time.time()
    print end - start

for f in sys.argv[1:]:
    s = open(f).read()
    u = s.decode('utf8')
    timeit(lambda: s.upper())
    timeit(lambda: u.upper())
