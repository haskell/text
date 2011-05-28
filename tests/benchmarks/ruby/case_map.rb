#!/usr/bin/env ruby

require './utils.rb'

ARGV.each do |f|
  t = benchmark { with_utf8_file(f) { |c| c.upcase } }
  STDERR.puts "#{f}: #{t}"
end
