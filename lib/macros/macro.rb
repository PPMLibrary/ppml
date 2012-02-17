require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body
    NAME = "(?:[a-zA-Z_][a-zA-Z_0-9]*)"
    ARGS = "(?: *#{NAME} *(?:, *#{NAME} *)*)?"
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/
    MACRO_STOP = /^ *end +macro *$/

    def initialize(name, body, args=nil)
      @name, @body, @args = name, body, args
    end

    def expand(result=nil, args=nil)
      map = {}
      if args and @args
        [args.size, @args.size].min.times do |i|
          map[@args[i]] = args[i]
        end
      end
      data = OpenStruct.new(map)
      def data.get_binding; binding end
      template = ERB.new @body
      template.result data.get_binding
    end

    private

    class << self
      def load(path)
        load_file File.open(path)
      end

      def load_file(file)
        macros = {}
        while l=file.gets
          if l =~ MACRO_START
            name = $~[:name]
            args = $~[:args].split(',').map(&:strip) if $~[:args]
            body = read_body(file)
            macros[name] = Macro.new(name, body, args)
          end
        end
        macros
      end

      private

      def read_body(file)
        body = ''
        while l=file.gets
          raise "Nested macro definitions are not allowed" if l =~ MACRO_START
          return body if l =~ MACRO_STOP
          body << l
        end
        raise "EOF while reading body!"
      end
    end
  end
end
