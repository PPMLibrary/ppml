require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body

    NAME = '(?:[a-zA-Z_][a-zA-Z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *(?<!%)= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*)?"
    MACRO_START = //
    MACRO_STOP = //
    REGMAGIC = /(?:^|,) *(?<name>([^=,]|(?<=%)=)*?)(?: *(?<!%)= *(?<value>".*?(?<!\\)"|[^,]*))? *(?=,|$)/

    def initialize(name, body_or_file, args=nil)
      if body_or_file.is_a? StringIO or body_or_file.is_a? File
        body = read_body body_or_file
      else
        body = body_or_file
      end
      @name, @body = name, body
      if args
        @args = Macro.parse_arglist args
      end
      @recursive_expanded = false
    end

    def expand(scope, result=nil, args=nil, named=nil, dotarg=nil)
      map = {}
      args.insert 0,dotarg if !dotarg.nil?
      if @args
        map = @args.clone
        [args.size, map.keys.size].min.times do |i|
          map[map.keys[i]] = args[i]
        end if args
        map.merge! named if named
      end
      map["scope"] = scope
      expand_recursive_calls(scope) if !@recursive_expand
      erb = ERB.new @body, nil, "%"
      erb.result Macro.binding_from_map(map)
    end

    def expand_recursive_calls(scope)
      m = Preprocessor.instance.macros
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
        m[name].expand(scope,nil,positional,named).gsub!(/^(.*)$/) do
          |line|
          indent + line
        end if m.has_key? name
      end
      @recursive_expanded = true
    end

    protected

    def read_body file
      body = ''
      while l=file.gets
        raise "Nested macro definitions are not allowed" if l =~ self.class::MACRO_START
        return body if l =~ self.class::MACRO_STOP
        body << l
      end
      raise "EOF while reading body!"
    end

    protected

    class << self
      def load(path)
        load_file File.open(path)
      end

      def load_file(file)
        macros = {}
        while l=file.gets
          if l =~ FunctionMacro::MACRO_START
            name = $~[:name]
            macros[name] = FunctionMacro.new(name, file, $~[:args])
          elsif l =~ IncludeMacro::MACRO_START
            name = $~[:name]
            macros[name] = IncludeMacro.new(name, file, $~[:args])
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



    end # class << self

  end # Macro

  class FunctionMacro < Macro
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/
    MACRO_STOP = /^ *end +macro *$/
  end # FunctionMacro

  class ForeachMacro < Macro
  end # ForeachMacro

  class IncludeMacro < Macro
    MACRO_START = /^ *include +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/
    MACRO_STOP = /^ *end +include *$/

    def expand(scope, args=nil, named=nil)
      super(scope, result=nil, args=args, named=named, dotarg=nil)
    end

  end #IncludeMacro

end
