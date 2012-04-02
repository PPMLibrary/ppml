require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body

    NAME = '(?:[a-z_][a-z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *(?<!%)= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*)?"
    MACRO_START = /^ *(?<foreach>foreach)? *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
    MACRO_STOP = /^ *end +macro *$/i
    REGMAGIC = /(?:^|,) *(?<name>([^=,]|(?<=%)=)*?)(?: *(?<!%)= *(?<value>".*?(?<!\\)"|[^,]*))? *(?=,|$)/

    def initialize name, body, args=nil
      @name, @body = name, body
      if args
        @args = Macro.parse_arglist args
      end
      @recursive_expanded = false
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
            body = read_body file
            if $~[:foreach]
              macros[name] = ForeachMacro.new(name, body, $~[:args])
            else
              macros[name] = FunctionMacro.new(name, body, $~[:args])
            end
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

      def resolve_args default, positional, named
        result = {}
        if default
          result = default.clone
          [positional.size, result.keys.size].min.times do |i|
            result[result.keys[i]] = positional[i]
          end if positional
          result.merge! named if named
        end
        return result
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

  end # Macro

  class FunctionMacro < Macro
    def expand(scope, result=nil, args=nil, named=nil)
      map = Macro.resolve_args @args, args, named
      map["scope"] = scope
      expand_recursive_calls(scope) unless @recursive_expand
      erb = ERB.new @body, nil, "%-"
      erb.result Macro.binding_from_map(map)
    end
  end # FunctionMacro

  class ForeachMacro < Macro
    def initialize name, body, args=nil
      super name, body, args
      # STDERR.puts "FOREACH MACRO : #{name}\n#{body}\nDONE"
    end

    def expand context, iter, args, named, mods, modargs, bodies
      # STDERR.puts "\nexpand called!\n\n"
      # STDERR.puts "      iter : #{iter}"
      # STDERR.puts "      args : #{args}"
      # STDERR.puts "named args : #{named}"
      # STDERR.puts "      mods : #{mods}"
      # STDERR.puts "  mod args : #{modargs}"
      # STDERR.puts "    bodies : #{bodies}"
      map = Macro.resolve_args @args, args, named
      map["scope"] = context
      map["body"] = bodies[0]
      map["iter"] = iter
      # expand_recursive_calls(scope) unless @recursive_expand
      erb = ERB.new @body, nil, "%-"
      r = erb.result Macro.binding_from_map(map)
      # STDERR.puts "RESULT\n#{r}\nDONE"
      r
    end
  end # ForeachMacro

end
