Given /^a macro "([^"]*)" is defined as "([^"]*)"$/ do |name, body|
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body)
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as "([^"]*)"$/ do |name, args, body| #"
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body, args)
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as$/ do |name, args, body| #"
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body, args)
end

Given /^a file with the following structure$/ do |string|
  @file = string
end

When /^the file is loaded$/ do
  @macros = CG::Macro.load_file(StringIO.new(@file))
end

Then /^a macro named "([^"]*)" should be created$/ do |name|
  @name = name
  @macros[name].should be_a(CG::Macro)
  @macros[name].name.should == name
end

Then /^the macro should have arguments "([^"]*)"$/ do |args|
  al = CG::Macro.parse_arglist args
  @macros[@name].args.should == al
end

Then /^the body of the macro should be$/ do |body|
  @macros[@name].body.should == body
end

Given /^the standard macro path is "([^"]*)"$/ do |path|
  CG::Preprocessor.standard_macro_path = path
end

When /^the preprocessor object is created$/ do
  @p = CG::Preprocessor.new(@project?@project:nil)
end

Then /^it includes all macros from the standard path$/ do
  @p.macros.keys.should include "simple"
  @p.macros.keys.should include "with_ruby"
  @p.macros.keys.should include "override"
  @p.macros.keys.length.should == 3
end

Given /^the project path is "([^"]*)"$/ do |path|
  @project = path
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
