# -*- ruby -*-
require 'rspec/core/rake_task'
require 'cucumber/rake/task'

require 'rake/clean'
# temp files
CLEAN.include("**/*~")
# generated files
CLOBBER.include("lib/parser/*")


def antlr(f)
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
