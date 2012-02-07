Gem::Specification.new do |s|
  s.name = "ppmcg"
  s.summary = "Easily create high performance simulations with the PPM library."
  s.description = File.read(File.join(File.dirname(__FILE__), 'README'))
  s.requirements = [ 'PPM Library has to be installed' ]
  s.version = "0.0.1"
  s.author = "MOSAIC Group"
  s.email = "team@ppm-library.com"
  s.homepage = "http://ppm-library.org"
  s.platform = Gem::Platform::RUBY
  s.required_ruby_version = '~>1.9'
  s.files = Dir['**/**']
  s.executables = [ 'cg' ]
  s.test_files = Dir["test/test*.rb"]
  s.has_rdoc = false
end
