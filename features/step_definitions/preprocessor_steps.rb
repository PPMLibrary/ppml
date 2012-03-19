Given /^the standard macro path is "([^"]*)"$/ do |path| #"
  CG::Preprocessor.standard_macro_path = path
end

Given /^user defined macros in "([^"]*)"$/ do |macro_dir| #"
  @macro_dir = macro_dir
end

Given /^the project path is "([^"]*)"$/ do |path| #"
  CG::Preprocessor.instance.cwd path
end

Given /^the input is standard fortran$/ do
  @macros ||= {}
end

When /^the preprocessor object is created$/ do
  @p = CG::Preprocessor.instance
end

When /^I preprocess$/ do |input|
  @macros ||= {}
  p = CG::Preprocessor.instance
  p.macros += @macros
  @input = input
  @output = p.process(@input)
end

When /^I preprocess "([^"]*)"$/ do |input| #"
  step "I preprocess", input
end

Then /^it should remain unchanged$/ do
  @output.should == @input
end

Then /^it should expand into$/ do |result|
  @output.should == result
end

Then /^it should expand into "([^"]*)"$/ do |result| #"
  step "it should expand into", result
end

Then /^it includes all macros from the standard path$/ do
  @p.macros.keys.should include "simple"
  @p.macros.keys.should include "fail"
  @p.macros.keys.should include "override"
  @p.macros.keys.length.should == 3
end

Then /^it includes all user defined macros$/ do
  @p.macros.keys.should include "one"
  @p.macros.keys.should include "two"
  @p.macros.keys.should include "three"
  @p.macros.keys.should include "override"
  @p.macros.keys.length.should == 6
end

Then /^predefined macros with the same names are replaced$/ do
  @p.macros['override'].body.should == "new body\n"
end
