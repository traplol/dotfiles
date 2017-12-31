#!/usr/bin/ruby

require 'net/http'
require 'cgi'
require 'json'
require 'pp'

class Coin
  def initialize sym, trim = false
    @coin_sym = sym.upcase
    @price = nil
    @trim = trim
  end

  def sym
    @coin_sym
  end

  def trim
    @trim
  end

  def price
    @price
  end

  def price= price
    @price = price
  end

  def to_s
    p = price
    if p
      if trim
        p = '%.0f' % p
      else
        p = '%.2f' % p
      end
    end
    "#{sym}:$#{p}"
  end
end

def get_prices coins
  return {} if coins.nil? || coins.empty?
  ids = coins.map(&:sym)
  url = "https://min-api.cryptocompare.com/data/price?fsym=USD&tsyms=#{ids.join(',')}"
  resp = Net::HTTP.get(URI(url))
  d = JSON.parse(resp)
  tmp = {}
  d.each do |k,v|
    tmp[k] = 1.0/v
  end
  coins.each do |c|
    usd = tmp[c.sym]
    next if usd.nil?
    c.price = usd
  end
  coins
end


coins = []
trim = false
i3 = false
ARGV.each do |arg|
  if arg == "--trim"
    trim = true
    next
  elsif arg == "--i3"
    i3 = true
    next
  end
  coins << Coin.new(arg, trim)
  trim = false
end

get_prices(coins)
output = coins.map(&:to_s).join(" ")
if i3
  puts " #{output} "
  puts " #{output} "
  puts "#93a1a1"
else
  puts "#{output}"
end
