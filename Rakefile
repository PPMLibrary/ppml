# -*- ruby -*-

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
    `antlr4ruby -make #{f}`
    name = f.match(/lib\/grammar\/(.*).g/)[1]
    `mv lib/grammar/#{name}*.rb lib/parser/`
    `mv #{name}.tokens lib/parser/`
  end
end

desc "Run Cucumber tests."
task :cuke do
  puts `cucumber -c features`
end

desc "Run RSpec tests."
task :spec do
  puts `rspec -c -fd`
end
