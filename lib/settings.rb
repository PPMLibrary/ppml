
require 'configatron'

CONFIG_FILE_NAME = 'settings'

def configuration_file_name
  CONFIG_FILE_NAME
end

def read_configuration
  eval File.open(CONFIG_FILE_NAME).read
end

def conf
  configatron
end