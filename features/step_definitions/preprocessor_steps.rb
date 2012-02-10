Given /^a macro "([^"]*)" is defined as "([^"]*)"$/ do |name, body|
  @macros ||= []
  @macros << CG::Macro.new(name, body)
end

Given /^the input is standard fortran$/ do
  @macros ||= []
end

When /^I preprocess "([^"]*)"$/ do |code|
  p = CG::Preprocessor.new
  p.macros += @macros
  @code = code
  @o = p.process(code)
end

Then /^it should remain unchanged$/ do
  @o.should == @code
end

Then /^it should expand into "([^"]*)"$/ do |result|
  @o.should == result
end
