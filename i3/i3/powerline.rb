#!/usr/bin/ruby
# coding: utf-8

color_a = ARGV[0] || "#CCCCCC"
color_b = ARGV[1] || "#333333"
text_color = ARGV[2] || "#AAAAAA"
text = ARGV[3] || ""
lines = text.split("\n")
if lines.length == 3
  text = lines[0]
  text_color = lines[2] if /^#[a-fA-F0-9]{6}$/ =~ lines[2]
else
  text = lines[0] || ""
end

has_left_bg = !ARGV.include?("--no-left-bg")

left_bg = ARGV.include?("--no-left-bg") ? "" : %Q(background="#{color_a}")
left_fg = ARGV.include?("--no-left-fg") ? "" : %Q(foreground="#{color_b}")

right_bg = ARGV.include?("--no-right-bg") ? "" : %Q(background="#{color_b}")
right_fg = ARGV.include?("--no-right-fg") ? "" : %Q(foreground="#{text_color}")

arrow_span = ARGV.include?("--no-arrow") ? "" : %Q(<span #{left_bg} #{left_fg}></span>)

spans = %Q(#{arrow_span}<span #{right_bg} #{right_fg}>#{text}</span>)

puts spans
