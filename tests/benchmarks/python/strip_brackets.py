#!/usr/bin/env python

import utils, sys

def strip_brackets(string):
    d = 0
    out = ''
    for c in string:
        if c == '{' or c == '[': d += 1

        if d > 0:
            out += ' '
        else:
            out += c

        if c == '}' or c == ']': d -= 1

    return out

for f in sys.argv[1:]:
    t = utils.benchmark(lambda: utils.with_utf8_file(f, strip_brackets))
    sys.stderr.write('{0}: {1}\n'.format(f, t))
