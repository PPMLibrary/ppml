Given /^a rhs file with the following structure$/ do |string|
  @file = string
end

Then /^a rhs named "([^"]*)" should be created$/ do |name|
  @rhs = CG::Rhs.load_file(StringIO.new(@file))
  @rhs.name.should == name
end

Then /^the source of the rhs should be$/ do |string|
  @rhs.rhs_func.to_s.should == string
end

