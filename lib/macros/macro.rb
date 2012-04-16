require 'ostruct'

module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body

    NAME = '(?:[a-z_][a-z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *(?<!%)= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*)?"
    MACRO_START = //
    MACRO_STOP = /^ *end +macro *$/i
    REGMAGIC = /(?:^|,) *(?<name>([^=,]|(?<=%)=)+?)(?: *(?<!%)= *(?<value>".*?(?<!\\)"|[^,]*))? *(?=,|$)/i

    def initialize name, body_or_file, args=nil
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

    def expand(scope, result=nil, pos=nil, named=nil, dotarg=nil)
      pos.insert 0, dotarg if !dotarg.nil?
      map = Macro.resolve_args @args, pos, named
      map["scope"] = scope
      map["result"] = result.to_s
      expand_recursive_calls(scope) unless @recursive_expand
      erb = ERB.new @body, nil, "%-"
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
          if value == :required
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

    class << self
      def load path
        load_file File.open(path)
      end

      def load_file file
        macros = {}
        while l=file.gets
          if l =~ FunctionMacro::MACRO_START
            name = $~[:name]
            macros[name] = FunctionMacro.new name, file, $~[:args]
          elsif l =~ ForeachMacro::MACRO_START
            name = $~[:name]
            macros[name] = ForeachMacro.new name, file, $~[:args]
          elsif l =~ IncludeMacro::MACRO_START
            name = $~[:name]
            macros[name] = IncludeMacro.new name, file, $~[:args]
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
          result[name] = value ? (value != 'nil' ? value : nil) : :required
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
        raise ArgumentError if result.values.include? :required
        return result
      end

    end # class << self

  end # Macro

  class FunctionMacro < Macro
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
  end # FunctionMacro

  class ForeachMacro < Macro
    MACRO_START = /^ *foreach +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

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

  class IncludeMacro < Macro
    MACRO_START = /^ *include +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

    def expand(scope, args=nil, named=nil)
      super(scope, result=nil, args=args, named=named, dotarg=nil)
    end
  end #IncludeMacro
end
