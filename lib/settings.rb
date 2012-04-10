
require 'configatron'

CONFIG_FILE_NAME = 'settings'

def configuration_file_name
  CONFIG_FILE_NAME
end

def read_configuration
  if File.exists? CONFIG_FILE_NAME
    eval File.open(CONFIG_FILE_NAME).read
    if not conf.ppm.base_path.nil? and File.exists? "#{conf.ppm.base_path}/#{CONFIG_FILE_NAME}"
      eval File.open("#{conf.ppm.base_path}/#{CONFIG_FILE_NAME}").read
      eval File.open(CONFIG_FILE_NAME).read
    end
  end
end

def conf
  configatron
end
