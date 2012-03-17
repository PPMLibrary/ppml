require 'configatron'

Given /^setting (\w*) is (on|off)$/ do
  |name, value|
  value = value == 'on'
  configatron.configure_from_hash({ name.to_sym => value })
end
