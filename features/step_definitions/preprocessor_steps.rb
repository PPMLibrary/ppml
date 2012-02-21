Given /^the input is standard fortran$/ do
  @macros ||= {}
end

Given /^a macro "([^"]*)" is defined as "([^"]*)"$/ do |name, body|
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body)
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as "([^"]*)"$/ do |name, args, body| #"
  @macros ||= {}
  a = args.split(',')
  @macros[name] = CG::Macro.new(name, body, a)
end

When /^I preprocess "([^"]*)"$/ do |input| #"
  p = CG::Preprocessor.new
  p.macros += @macros
  @input = input
  @output = p.process(@input)
end

Then /^it should remain unchanged$/ do
  @output.should == @input
end

Then /^it should expand into "([^"]*)"$/ do |result| #"
  @output.should == result
end
