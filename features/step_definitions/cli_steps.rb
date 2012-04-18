Given /^the cwd is "([^"]*)"$/ do |path| #"
  Dir.chdir path
end

When /^I run "([^"]*)"$/ do |cmd| #"
  @output = `#{cmd}`
end

Then /^the result file is equal to "([^"]*)"$/ do |target| #"
  FileUtils.cmp('output/preprocessed.f',target).should == true
end

Then /^I get an executable simulation$/ do
  pending # express the regexp above with the code you wish you had
end

Then /^the simulation is executed$/ do
  pending # express the regexp above with the code you wish you had
end

Then /^the build directory is deleted$/ do
  pending # express the regexp above with the code you wish you had
end

Given /^the config option "([^"]*)" is set to "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Then /^the command outputs "([^"]*)"$/ do |arg1| #"
  pending # express the regexp above with the code you wish you had
end

Then /^I get a prompt to enter the missing configuration options$/ do
  pending # express the regexp above with the code you wish you had
end

Then /^the project skeleton is created in "([^"]*)"$/ do |target| #"
  project = File.basename(target)
  Dir.exists?(project).should == true
  Dir.exists?("#{project}/macros").should == true
  Dir.exists?("#{project}/gen").should == true
  Dir.exists?("#{project}/run").should == true
  File.exists?("#{project}/#{project}.ppm").should == true
  File.exists?("#{project}/#{project}.rhs").should == true
  File.exists?("#{project}/#{configuration_file_name}").should == true
  FileUtils.rm_rf project # cleanup
end

Given /^the cwd is "([^"]*)" and the system is "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Then /^the subdirectory "([^"]*)" is created and the compiled simulation is copied from "([^"]*)" to "([^"]*)"$/ do |arg1, arg2, arg3|
  pending # express the regexp above with the code you wish you had
end

And /^the template "([^"]*)" is copied to "([^"]*)" and executed$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

And /^plots are created for the parallel scaling and wallclock time$/ do
  pending # express the regexp above with the code you wish you had
end
