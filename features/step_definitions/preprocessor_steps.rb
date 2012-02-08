Given /^the input is standard fortran$/ do
end

When /^I pass "([^"]*)"$/ do |code|
  p = CG::Preprocessor.new
  @code = code
  @o = p.process(code)
end

Then /^it should remain unchanged$/ do
  @o.should == @code
end
