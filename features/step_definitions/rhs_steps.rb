Given /^there is a rhs call to "([^"]*)"$/ do |rhs| #"
  @rhs_name = rhs
end

Given /^rhs call args are \(([^)]*)\) \(([^)]*)\)$/ do |argtypes, disctypes|
  at = argtypes.split(",").map { |e| e.strip[1..e.length].to_sym }
  dt = disctypes.split(",").map { |e| e.strip[1..e.length].to_sym }
  @rhs_args = at.zip(dt)
end

Given /^rhs call results are \(([^)]*)\) \(([^)]*)\)$/ do |resulttypes,disctypes|
  rt = resulttypes.split(",").map { |e| e.strip[1..e.length].to_sym }
  dt = disctypes.split(",").map { |e| e.strip[1..e.length].to_sym }
  r = rt.zip(dt)
  CG::RHSModule.instance.call_to(@rhs_name, @rhs_args, r)
end

Then /^the rhs module outputs$/ do |body|
  CG::Preprocessor.instance.process(CG::RHSModule.instance.to_s).should == body
end
