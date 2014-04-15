# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'servo/version'

Gem::Specification.new do |spec|
  spec.name          = "servo"
  spec.version       = Servo::VERSION
  spec.authors       = ["HIROE Shin"]
  spec.email         = ["hiroe.orz@gmail.com"]
  spec.summary       = "MarioNet MQTT Client Device Application"
  spec.description   = "MarioNet is MQTT Client application."
  spec.homepage      = ""
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.6"
  spec.add_development_dependency "rake"

  spec.add_dependency "msgpack"
  spec.add_dependency "uuidtools"
  spec.add_dependency "em-zeromq"
end
