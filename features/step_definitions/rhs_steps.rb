Given /^there is a rhs call to "([^"]*)"$/ do |rhs| #"
  @rhs_name = rhs
end

Given /^rhs call args are \(([^)]*)\)$/ do |args|
  @rhs_args = args.split(",").map { |e| e.strip[1..e.length].to_sym }
end

Given /^rhs call results are \(([^)]*)\)$/ do |results|
  r = results.split(",").map { |e| e.strip[1..e.length].to_sym }
  CG::RHSModule.instance.call_to(@rhs_name, @rhs_args, r)
end

Then /^the rhs module outputs$/ do |body|
  CG::Preprocessor.instance.process(CG::RHSModule.instance.to_s).should == body
end
