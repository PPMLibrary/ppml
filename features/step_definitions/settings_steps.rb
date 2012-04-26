require 'configatron'

Given /^setting ((?:\w|\.)*) is (on|off|\d+)$/ do
  |name, value|
  if value == 'on'
    value = true
  elsif value == 'off'
    value = false
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
