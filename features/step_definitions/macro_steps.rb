########################
# Function call macros #
########################

Given /^a macro "([^"]*)" is defined as$/ do |name, body| #"
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body)
end

Given /^a macro "([^"]*)" is defined as "([^"]*)"$/ do |name, body|
  step "a macro \"#{name}\" is defined as", body
end

Given /^a macro "([^"]*)" with argument list \("([^"]*)"\) is defined as$/ do |name, args, body|
  @macros ||= {}
  @macros[name] = CG::Macro.new(name, body, args)
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
  @macros[name].should be_a(CG::Macro)
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
  @temp_name
end

Given /^argument list \(([^\)]*)\) and body$/ do |args, body|
  @macros[@temp_name] = CG::ForeachMacro.new @temp_name, args, body
end
