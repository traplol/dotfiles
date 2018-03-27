#!/usr/bin/ruby


require 'net/http'
require 'uri'
require 'openssl'
require 'json'
require 'bigdecimal'
require 'pp'

class BigDecimal
  alias_method :old_to_s, :to_s

  def to_s(f="F")
    old_to_s(f)
  end

  def pretty_print(pp)
    pp.text("BigDecimal('#{to_s("F")}')")
  end
end

class Binance

  API_URI = "https://api.binance.com"

  def get_tickers(symbol="")
    if not symbol.empty?
      symbol = "symbol=#{symbol}"
    end
    prices = _get("/api/v3/ticker/price", symbol)
    if prices.is_a? Array
      prices.each do |v|
        v["price"] = BigDecimal.new(v["price"])
      end
      prices
    else
      return prices if not prices["code"].nil? # error short-circuit
      prices["price"] = BigDecimal.new(prices["price"])
    end
    tickers = {}
    prices.each do |t|
        tickers[t["symbol"]] = t["price"]
    end
    tickers
  end

  def _get(endpoint, params="")
    uri = "#{API_URI}/#{endpoint}?#{params}"
    url = URI.parse(uri)
    resp = Net::HTTP.start(url.host, url.port, use_ssl: true) do |http|
      req = Net::HTTP::Get.new(url)
      req.add_field("X-MBX-APIKEY", @apiKey)
      http.request(req)
    end
    begin 
      JSON.parse(resp.body)
    rescue => e
      {"error"=>"JSON.parse error",
       "msg"=>e.message.to_s}
    end
  end
end

def commify(n)
  n.to_s.reverse.gsub(/(\d{3})/,"\\1,").chomp(",").reverse
end

def calculate_prices(coins)
  api = Binance.new
  coins = coins.map(&:upcase)
  tickers = api.get_tickers
  prices = []
  btc_usdt = tickers["BTCUSDT"]
  if btc_usdt.nil?
    out = "Cannot determine BTC/USDT price..."
    return [out, out, "#ff0000"]
  end
  coins.each do |c|
    if c == "BTC"
      p = "BTCUSDT"
    else
      p = c + "BTC"
    end
    v = tickers[p]
    v = 1 if p == "BTCUSDT"
    d = {:coin => c, :price => (v || "unk")}
    prices << d
  end
  parts = []
  prices.each do |p|
    price = "unk"
    if p[:coin] == "BTC"
      price = "$#{(p[:price] * btc_usdt).to_i}"
    elsif p[:price] != "unk"
      v = p[:price] * 100_000_000
      price = "#{commify(v.to_i)}"
      #if v < 10.0
      #  price = "%.3f" % v
      #elsif v < 100.0
      #  price = "%.2f" % v
      #else
      #  price = "%.0f" % v
      #end
    end
    parts << "#{p[:coin]}:#{price}"
  end
  out = parts.join(" ")
  return [out, out]
end

calculate_prices(["BTC", "ETH", "NEO", "ICX", "VEN"]).each{|o| puts o}
#calculate_prices(["BTC"]).each{|o| puts o}
