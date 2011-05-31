#!/usr/bin/env ruby

require './utils.rb'

def word_count(str)
  freqs = Hash.new 0
  str.split.each do |w|
    freqs[w.downcase] += 1
  end
  freqs
end

ARGV.each do |f|
  t = benchmark { with_utf8_file(f) { |c| word_count(c) } }
  STDERR.puts "#{f}: #{t}"
end
