ENV['PATH'] = ENV['PATH'] + ":#{Dir.pwd}/bin"

Before do
  @old_pwd = Dir.pwd
end

After do
  Dir.chdir @old_pwd
end
