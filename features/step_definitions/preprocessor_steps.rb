Given /^the input is standard fortran$/ do
  @macros ||= {}
end

When /^I preprocess "([^"]*)"$/ do |input| #"
  p = CG::Preprocessor.new
  p.macros += @macros
  @input = input
  @output = p.process(@input)
end

When /^I preprocess$/ do |input|
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

Then /^it should expand into$/ do |result|
  @output.should == result
end
