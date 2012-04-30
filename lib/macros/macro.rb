require 'ostruct'

def indent body, amount
  prefix = " " * amount
  unless body.empty?
    lines = body.split("\n")
    lines = [''] if lines.empty?
    lines.map! { |l| prefix + l }
    lines.join("\n") + "\n"
  end
end


module CG
  class Macro
    attr_accessor :name
    attr_reader :args, :body

    NAME = '(?:[a-z_][a-z_0-9]*)'
    VAL = '(?:"[^"]*"|[^,]*?)'
    ARG = "#{NAME}(?: *(?<!%)= *#{VAL})?"
    ARGS = "(?: *#{ARG} *(?:, *#{ARG} *)*(?:, *\\*#{NAME} *)?|(?: *\\*#{NAME} *))?"
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
      expand_recursive_calls(scope) unless @recursive_expanded
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
          elsif l =~ TimeLoopMacro::MACRO_START
            macros["timeloop"] = TimeLoopMacro.new file
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
          if name[0] == '*'
            result[:splat] = name[1..name.length]
            break
          else
            result[name] = value ? (value != 'nil' ? value : nil) : :required
          end
        end
        result[:splat] = nil unless result[:splat]
        result
      end

      def resolve_args default, positional, named
        result = {}
        if default
          result = default.clone
          if positional
            n = [positional.size, result.keys.size-1].min
            n.times do |i|
              if i < positional.size
                result[result.keys[i]] = positional[i]
              end
              result[result.keys[i]] = positional[i]
            end
            if positional.size > n
              unless result[:splat]
                raise "Too many arguments! Expecting #{result.keys.size-1} got #{@positional.size}"
              end
              result[result[:splat]] = []
              n.upto(positional.size-1).each do |i|
                result[result[:splat]] << positional[i]
              end
              result.delete(:splat)
            end
          end
          result.merge! named if named
        end
        raise ArgumentError if result.values.include? :required
        result
      end

    end # class << self

  end # Macro

  class FunctionMacro < Macro
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
  end # FunctionMacro

  class ForeachMacro < Macro
    MACRO_START = /^ *foreach +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

    def initialize name, body, args, mods=nil
      super name, body, args
      @mods = ForeachMacro.parse_mods mods
    end

    def expand context, iter, args, named, mods, ma, ma_named, bodies
      map = Macro.resolve_args @args, args, named
      map.merge! ForeachMacro.resolve_mods(@mods, mods, ma, ma_named)
      map["body"] = bodies[0]
      map["scope"] = context
      map["iter"] = iter
      # expand_recursive_calls(scope) unless @recursive_expanded
      erb = ERB.new @body, nil, "%-"
      erb.result Macro.binding_from_map(map)
    end

    def self.parse_mods mods
      result = {}
      mods.each do
        |name, arglist|
        result[name] = Macro.parse_arglist arglist
      end if mods
      result
    end

    def self.resolve_mods defn, mods, pos, named
      result = {}
      # copy default values in
      defn.each do
        |n, args|
        args.each do
          |name, value|
          result[name] = value unless name == :splat
        end
      end
      # merge supplied args (override defaults)
      mods.zip(pos,named).each do
        |m, p, n|
        result.merge! Macro.resolve_args defn[m], p, n
      end
      result
    end
  end # ForeachMacro

  class TimeLoopMacro < Macro
    MACRO_START = /^ *timeloop +macro *\(body\) *$/i

    def initialize body
      super "timeloop", body, "body"
    end

    def expand context, body
      map = {"scope" => context, "body" => body.to_s}
      expand_recursive_calls(context) unless @recursive_expanded
      erb = ERB.new @body, nil, "%-"
      erb.result Macro.binding_from_map(map)
    end
  end # TimeLoopMacro

  class IncludeMacro < Macro
    MACRO_START = /^ *include +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

    def expand(scope, args=nil, named=nil)
      super(scope, result=nil, args=args, named=named, dotarg=nil)
    end
  end #IncludeMacro
end
