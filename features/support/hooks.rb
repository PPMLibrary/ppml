require 'configatron'

Before do
  @old_pwd = Dir.pwd
  configatron.comment_mode = false
end

After do
  Dir.chdir @old_pwd
  configatron.comment_mode = false
end
