require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body
    NAME = '(?:[a-zA-Z_][a-zA-Z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*)?"
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/
    MACRO_STOP = /^ *end +macro *$/
    REGMAGIC = /(?:^|,) *(?<name>[^=,]*)(?: *(?:=) *(?<value>".*?(?<!\\)"|[^,]*))? */

    def initialize(name, body, args=nil)
      @name, @body = name, body
      if args
        @args = parse_arglist args
      end
    end

    def expand(result=nil, args=nil, namedargs=nil, namedvalues=nil)
      if @args
        map = @args.clone
        if args
          [args.size, @args.keys.size].min.times do |i|
            map[map.keys[i]] = args[i]
          end
        end
        if namedargs
          namedargs.each_with_index do
            |name,i|
            if map.has_key? name
              map[name] = namedvalues[i]
            end
          end
        end
      end
      data = OpenStruct.new(map)
      def data.get_binding; binding end
      template = ERB.new @body
      template.result data.get_binding
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
    end
  end
end
