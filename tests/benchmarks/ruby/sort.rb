#!/usr/bin/env ruby

require './utils.rb'

def sort(str)
  str.lines.sort.join
end

ARGV.each do |f|
  t = benchmark do
    with_utf8_file(f) { |c| puts sort(c) }
  end
  STDERR.puts "#{f}: #{t}"
end
