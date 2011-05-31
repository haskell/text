#!/usr/bin/env ruby

require './utils.rb'

def strip_brackets(str)
  d = 0
  out = ''

  str.each_char do |c|
    d += 1 if c == '{' || c == '['
    out << if d > 0 then ' ' else c end
    d -= 1 if c == '}' || c == ']'
  end

  out
end

ARGV.each do |f|
  t = benchmark { with_utf8_file(f) { |c| strip_brackets(c) } }
  STDERR.puts "#{f}: #{t}"
end
