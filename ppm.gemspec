Gem::Specification.new do |s|
  s.name        = 'ppm'
  s.version     = '0.1.0'
  s.executables << 'ppm'
  s.date        = '2012-06-15'
  s.summary     = "Parallel Particle Mesh library command line utility."
  s.description = "A DSL extension of the PPM library."
  s.authors     = ["Milan Mitrovic", "Omar Awile"]
  s.email       = ''
  s.files       = Dir['lib/**/*.rb'] + Dir['lib/templates/*.tt'] + ['lib/parser/CG.tokens']
  s.files       = s.files + Dir['lib/macros/defs/*.mac']
  s.homepage    = 'http://www.ppm-library.org'
  s.add_runtime_dependency 'antlr3',      '~> 1.8.12'
  s.add_runtime_dependency 'thor',        '~> 0.15.2'
  s.add_runtime_dependency 'configatron', '~> 2.9.1'
  s.add_development_dependency 'cucumber', '~> 1.2.1'
  s.add_development_dependency 'rspec',    '~> 2.10.0'
  s.add_development_dependency 'autotest', '~> 4.4.6'
end
