#!/usr/bin/ruby

rotate_by = ARGV[0].to_i
max_len   = ARGV[1].to_i
str       = ARGV[2] + " "

s = str.split(//).rotate(rotate_by)[0...max_len].join
puts s
puts s
puts "#aa55ee"
