require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body

    NAME = '(?:[a-zA-Z_][a-zA-Z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *(?<!%)= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*)?"
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/
    MACRO_STOP = /^ *end +macro *$/
    REGMAGIC = /(?:^|,) *(?<name>([^=,]|(?<=%)=)*?)(?: *(?<!%)= *(?<value>".*?(?<!\\)"|[^,]*))? *(?=,|$)/

    def initialize(name, body, args=nil)
      @name, @body = name, body
      if args
        @args = Macro.parse_arglist args
      end
    end

    def expand(result=nil, args=nil, named=nil)
      if @args
        map = @args.clone
        [args.size, @args.keys.size].min.times do |i|
          map[map.keys[i]] = args[i]
        end if args
        map.merge! named if named
      end
      ERB.new(@body).result(Macro.binding_from_map(map))
    end

    def expand_recursive_calls macros
      @body.gsub!(/^(?<indent>\s*)\$(?<name>#{NAME})\((?<args>.*)\)/) do
        indent = $~[:indent]
        name = $~[:name]
        args = Macro.parse_arglist($~[:args])
        positional = []
        named = {}
        args.each do
          |name, value|
          if value.nil?
            positional.push name
          else
            named[name] = value
          end
        end
        macros[name].expand(nil,positional,named).gsub!(/^(.*)$/) do
          |m|
          indent + m
        end if macros.has_key? name
      end
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
            body = read_body(file)
            macros[name] = Macro.new(name, body, $~[:args])
          end
        end
        macros
      end

      def binding_from_map map
        data = OpenStruct.new(map)
        def data.get_binding; binding end
        data.get_binding
      end

      def parse_arglist args
        result = {}
        args.scan(REGMAGIC) do
          |name, value|
          result[name] = (value and value != 'nil') ? value : nil
        end
        result
      end

      private

      def read_body file
        body = ''
        while l=file.gets
          raise "Nested macro definitions are not allowed" if l =~ MACRO_START
          return body if l =~ MACRO_STOP
          body << l
        end
        raise "EOF while reading body!"
      end

    end # class << self

  end # class Macro
end
