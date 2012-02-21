# -*- coding: utf-8 -*-
# -*- ruby -*-
require 'rspec/core/rake_task'
require 'cucumber/rake/task'
require 'rubygems'
require 'rubygems/package_task'
require 'rake/clean'

# temp files
CLEAN.include("**/*~")
# generated files
CLOBBER.include("lib/parser/*")


def antlr f
  puts "Generating #{f}"
  Dir.mkdir("lib/parser") if !File.directory?("lib/parser")
  `antlr4ruby #{f}`
  path, name = f.match(/(lib\/grammar\/(?:tree\/)?(.*?)).g/)[1..2]
  `mv #{path}*.rb lib/parser/`
  `cp #{name}.tokens lib/parser/`
end

file 'lib/parser/CGParser.rb' => 'lib/grammar/CG.g' do |f|
  antlr 'lib/grammar/CG.g'
end

file 'lib/parser/CG.rb' => ['lib/grammar/tree/CG.g', 'lib/parser/CGParser.rb'] do |f|
  antlr 'lib/grammar/tree/CG.g'
end

desc "Regenerate ANTLR Parsers"
task :antlr => 'lib/parser/CG.rb'

task :cuke => :antlr
Cucumber::Rake::Task.new(:cuke) do |task|
  task.cucumber_opts = ["--color", "--format html", "--out doc/feature_report.html", "--format pretty"]
end

task :spec => :antlr
RSpec::Core::RakeTask.new do |t|
  t.verbose = false
  t.rspec_opts = ["--color", "--format html", "--out doc/spec_report.html", "--format documentation"]
end

gemspec = Gem::Specification.new do |s|
  s.name = "ppm"
  s.summary = "Easily create high performance simulations with the PPM library."
  s.description = File.read(File.join(File.dirname(__FILE__), 'README'))
  s.requirements = [ 'PPM Library has to be installed' ]
  s.version = "0.0.1"
  s.authors = ["Omar Awile", "Milan Mitrović"]
  s.email = ["omar.awile@inf.ethz.ch", "milanm@ethz.ch"]
  s.homepage = "http://ppm-library.org"
  s.required_ruby_version = '~>1.9'
  s.files = FileList['bin/**/*', 'lib/**/*', 'features/**/*', 'examples/**/*', 'spec/**/*', 'doc/**/*']
  s.require_path = 'lib'
  s.executables << 'ppm'
  s.test_files = Dir["test/test*.rb"]
  s.has_rdoc = false
end

Gem::PackageTask.new(gemspec) do |package|
   package.need_zip = true
   package.need_tar = true
end
