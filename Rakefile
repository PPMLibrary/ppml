# -*- ruby -*-
require 'rspec/core/rake_task'

require 'rake/clean'
# temp files
CLEAN.include("**/*~")
# generated files
CLOBBER.include("lib/parser")


desc "Run antlr4ruby to generate the parser."
task :antlr do
  Dir.mkdir("lib/parser") if !File.directory?("lib/parser")
end
# this should be changed to reflect dependencies between grammars
grammars = FileList.new("lib/grammar/*.g")
grammars.each do |f|
  task :antlr do
    puts "Generating #{f}"
    `antlr4ruby #{f}`
    name = f.match(/lib\/grammar\/(.*).g/)[1]
    `mv lib/grammar/#{name}*.rb lib/parser/`
    `mv #{name}.tokens lib/parser/`
  end
end

desc "Run Cucumber tests."
task :cuke do
  puts `cucumber --color --format html --out doc/feature_report.html --format pretty`
end

RSpec::Core::RakeTask.new do |t|
  t.verbose = false
  t.rspec_opts = ["--color", "--format html", "--out doc/spec_report.html", "--format documentation"]
end
