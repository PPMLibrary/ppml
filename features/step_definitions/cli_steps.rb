Given /^the cwd is "([^"]*)"$/ do |path|
  Dir.chdir path
end

Given /^user defined macros in "([^"]*)"$/ do |macro_dir|
  @macro_dir = macro_dir
end

When /^I run "([^"]*)"$/ do |cmd|
  @output = `#{cmd}`
end

Then /^the result file is equal to "([^"]*)"$/ do |target|
  s = `cat output/preprocessed.f | md5`
  $?.exitstatus.should == 0
  t = `cat #{target} | md5`
  $?.exitstatus.should == 0
  s.should == t
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

Then /^the configuration option "([^"]*)" is set to "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Given /^the config option "([^"]*)" is set to "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Then /^the command outputs "([^"]*)"$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end

Then /^I get a prompt to enter the missing configuration options$/ do
  pending # express the regexp above with the code you wish you had
end

Then /^the project skeleton is created in "([^"]*)"$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end
