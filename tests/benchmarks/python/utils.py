#!/usr/bin/env python

import sys, time

def benchmark_once(f):
    start = time.time()
    f()
    end = time.time()
    return end - start

def benchmark(f):
    runs = 100
    total = 0.0
    for i in range(runs):
        result = benchmark_once(f)
        sys.stderr.write('Run {0}: {1}\n'.format(i, result))
        total += result
    return total / runs

def with_utf8_file(filename, f):
    contents = open(filename).read().decode('utf-8')
    return f(contents)
