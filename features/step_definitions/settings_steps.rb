require 'configatron'

Given /^setting ((?:\w|\.)*) is (on|off|real|integer|\d+)$/ do
  |name, value|
  if value == 'on'
    value = true
  elsif value == 'off'
    value = false
  elsif value == 'real'
    value = :real
  elsif value == 'integer'
    value = :integer
  else
    value = value.to_i
  end
  val = value
  name.split(".").reverse.each do
    |part|
    val = {part.to_sym => val}
  end
  configatron.configure_from_hash(val)
end
