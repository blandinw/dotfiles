#!/usr/bin/env ruby

counts = {}

ARGF.each do |line|
  line.split(';').each do |cmd|
    cmd.strip!
    counts[cmd] ||= 0
    counts[cmd] += 1
  end
end

counts.sort{ |a,b| b[1] <=> a[1] }.each do |k, v|
  puts "#{v} #{k.strip}" if v >= 5
end
