########################
# Function call macros #
########################

Given /^a macro "([^"]*)" is defined as$/ do |name, body| #"
  @macros ||= {}
  @macros[name] = CG::FunctionMacro.new(name, body)
end

Given /^a macro "([^"]*)" is defined as "([^"]*)"$/ do |name, body|
  step "a macro \"#{name}\" is defined as", body
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as$/ do |name, args, body|
  @macros ||= {}
  @macros[name] = CG::FunctionMacro.new(name, body, args)
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as "([^"]*)"$/ do |name, args, body| #"
  step "a macro \"#{name}\" with argument list (\"#{args}\") is defined as", body
end

#################
# General steps #
#################

Given /^a file with the following structure$/ do |string|
  @file = string
end

When /^the file is loaded$/ do
  @macros = CG::Macro.load_file(StringIO.new(@file))
end

Then /^a macro named "([^"]*)" should be created$/ do |name| #"
  @name = name
  @macros[name].should be_a(CG::FunctionMacro)
  @macros[name].name.should == name
end

Then /^the macro should have arguments "([^"]*)"$/ do |args| #"
  al = CG::Macro.parse_arglist args
  @macros[@name].args.should == al
end

Then /^the body of the macro should be$/ do |body|
  @macros[@name].body.should == body
end

##################
# Foreach macros #
##################

Given /^a foreach macro named "([^"]*)"$/ do |name| #"
  @macros ||= {}
  @modifiers ||= {}
  @name = name
end

Given /^argument list \(([^\)]*)\)$/ do |args|
  @args = args
end

Given /^modifier "([^"]*)" with argument list \(([^\)]*)\)$/ do |name,args| #"
  @modifiers[name] = args
end

Given /^body$/ do |body|
  @macros[@name] = CG::ForeachMacro.new @name, body, @args, @modifiers
end
