require_relative '../../lib/macros'
require_relative '../../lib/parser'
require_relative '../../lib/fortran'
require 'fileutils'
ENV['PATH'] = ENV['PATH'] + ":#{Dir.pwd}/bin"

read_configuration
