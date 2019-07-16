#!/usr/bin/env ruby
# coding: utf-8
require 'fileutils'
require 'uri'
require 'optparse'

media_handled = /(avi|mkv|mp4)$/i
serie_pat = /\.(s\d{1,2}e\d{1,2})\.(final)?/i
year_pat = /\.(\d{4})\./

#puts ARGV
#puts "------"
ENV.each { |e| puts e }
puts "---XXXX ---"
# NAUTILUS_SCRIPT_SELECTED_URIS
files = ENV['NAUTILUS_SCRIPT_SELECTED_FILE_PATHS'].split(/\n/) if ENV['NAUTILUS_SCRIPT_SELECTED_URIS']
files ||= ARGV
files ||= Dir.entries('.')
files.each do |name|
  name = URI.unescape(name)
  puts "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
  puts name
  path = File.dirname(name)
  name = File.basename(name)
  if name =~ media_handled
    ext = File.extname(name)
    year = (name.match year_pat)[1] if name.match year_pat
    episode = (name.match serie_pat)[1] if name.match serie_pat
    new = name
            .downcase
            .gsub(media_handled, '')
            .gsub(/%20/, ' ')
            .gsub(/(\[|\%5b)\s?www.cpasbien....?(\]|\%5d)\s+/, '')
            .gsub(/\[\s?torrent9....?\]\s+/, '')
            .gsub(/\[\s?.*.?\]\s+/, '')
            .gsub(/(vost|frenc|fastsub|truefre|subforce|bluray|multi).*$/, '')
            .gsub(year_pat, '')
            .gsub(serie_pat, '')
            .tr('.', ' ')
            .gsub(/\s+/, ' ')
            .gsub(/\s+$/, '')
    new += ' ' + episode.upcase() if episode
    new += " (#{year})"  if year
    new += ext
    puts "#{path} : #{name} → #{path}/#{new}"
    system("notify-send ' #{path}: #{new}'")
    #%x[mv '#{File.join(path, name)}' '#{File.join(path, new)}']
    FileUtils.mv(File.join(path, name), File.join(path, new))
  end
end
