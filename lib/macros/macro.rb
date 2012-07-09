require 'ostruct'
require_relative 'transform'

class String

  def indent amount
    if amount.is_a? String
      prefix = amount
    else
      prefix = " " * amount
    end
    unless empty?
      lines = split("\n")
      lines = [''] if lines.empty?
      lines.map! { |l| prefix + l }
      lines.join("\n") + "\n"
    end
  end

  def indent! amount
    replace(indent amount)
  end

  def transform pattern, replacement
    t = CG::Transform.new pattern, replacement
    t.transform self
  end

  def transform! pattern, replacement
    replace(transform pattern, replacement)
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
        @args = Macro.build_arglist args
      end
    end

    def expand(scope, result=nil, pos=nil, named=nil, recursive=false)
      map = Macro.resolve_args @args, pos, named
      map["scope"] = scope
      if result.length > 1
        map["result"] = result.map(&:to_s)
      elsif result.length == 1
        map["result"] = result[0].to_s
      end unless result.nil?
      binding = Macro.binding_from_map map
      erb = ERB.new @body, nil, "%-"
      expanded = erb.result binding
      expanded = scope.mangle expanded unless scope.nil?
      expanded = Preprocessor.instance.process expanded+"\n", scope
      expanded = expanded[0...-1]
      expanded
    end

    def self.eval_args args, pos, named, binding
      args.each do
        |name, value|
        if value == :required
          pos.push eval(name, binding).to_s
        else
          named[name] = eval(value, binding).to_s
        end
      end
    end

    protected

    def read_body file
      body = ''
      while l=file.gets
        raise "Fatal Error: Nested macro definitions are not allowed" if l =~ self.class::MACRO_START
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
            macros["timeloop"] = TimeLoopMacro.new file, $~[:args]
          elsif l =~ IncludeMacro::MACRO_START
            name = $~[:name]
            macros[name] = IncludeMacro.new name, file, $~[:args]
          end
        end
        macros
      end

      def build_struct arguments
        if arguments.is_a? Hash
          data = OpenStruct.new(Hash[arguments.map {|k,v| [k,build_struct( v)]}])
        else
          arguments
        end
      end

      def binding_from_map arguments
        data = build_struct arguments
        def data.get_binding; binding end
        data.get_binding
      end

      def build_arglist args
        result = parse_arglist args
        result[:splat] = nil unless result[:splat]
        result
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
                raise "Too many arguments! Expecting #{result.keys.size-1} got #{positional.size}"
              end
              result[result[:splat]] = []
              n.upto(positional.size-1).each do |i|
                result[result[:splat]] << positional[i]
              end
            end
          end
          if named
            named.each_pair do |name,value|
              unless result[name].nil?
                result[name] = value
              else
                unless result[:splat]
                  raise "Unexpected named argument! #{name} was not defined"
                else
                  result[result[:splat]] = [] if result[result[:splat]].nil?
                  result[result[:splat]] << [name,value]
                end
              end
            end
          end
          result.delete(:splat)
        end
        raise ArgumentError if result.values.include? :required
        result.each_pair do |k,v|
          if (v.to_s.start_with?("'") and v.to_s.end_with?("'")) or
             (v.to_s.start_with?('"') and v.to_s.end_with?('"'))
            result[k] = v.to_s[1...-1]
          end
        end
        result
      end

    end # class << self

  end # Macro

  class FunctionMacro < Macro
    MACRO_START = /^ *macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
  end # FunctionMacro

  class ForeachMacro < Macro
    MACRO_START = /^ *foreach +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i
    MODIFIER = /^ *modifier +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

    def initialize name, body, args
      super name, body, args
      @body, @modifiers = parse_modifiers @body
    end

    def expand scope, iter, args, named, mods, ma, ma_named, bodies, recursive=false
      map = Macro.resolve_args @args, args, named
      map.merge! resolve_modifiers(mods, ma, ma_named)
      map["body"]   = bodies[:default] if bodies[:default]
      map["bodies"] = bodies
      map["scope"]  = scope
      map["iter"]   = iter
      binding = Macro.binding_from_map map
      erb = ERB.new @body, nil, "%-"
      expanded = erb.result binding
      expanded = scope.mangle expanded unless scope.nil?
      expanded = Preprocessor.instance.process expanded+"\n", scope
      expanded = expanded[0...-1]
      expanded
    end

    def parse_modifiers body
      modifiers = {}
      rest = []
      body.split("\n").each do |line|
          if line =~ MODIFIER
            modifiers[$~[:name]] = Macro.build_arglist $~[:args]
          else
            rest << line
          end
      end
      body = rest.join("\n")
      [body,modifiers]
    end

    def resolve_modifiers mods, pos, named
      result = {}
      # copy default values in
      @modifiers.each do
        |n, args|
        args.each do
          |name, value|
          result[name] = value unless name == :splat
        end
      end
      # merge supplied args (override defaults)
      mods.zip(pos,named).each do
        |m, p, n|
        result.merge! Macro.resolve_args @modifiers[m], p, n
      end
      result
    end
  end # ForeachMacro

  class TimeLoopMacro < Macro
    MACRO_START = /^ *timeloop +macro *\((?<args>#{ARGS})\) *$/i

    def initialize body, tparam_args
      super "timeloop", body, tparam_args
    end

    def expand scope, time, tparams, tparams_named, body, recursive=false
      map = Macro.resolve_args @args, tparams, tparams_named
      map["scope"] = scope
      map["time"] = time.to_s
      map["body"] = body.to_s
      binding = Macro.binding_from_map map
      erb = ERB.new @body, nil, "%-"
      expanded = erb.result binding
      expanded = scope.mangle expanded unless scope.nil?
      expanded = Preprocessor.instance.process expanded+"\n", scope
      expanded = expanded[0...-1]
      expanded
    end
  end # TimeLoopMacro

  class IncludeMacro < Macro
    MACRO_START = /^ *include +macro +(?<name>#{NAME}) *\((?<args>#{ARGS})\) *$/i

    def expand(scope, args=nil, named=nil, recursive=false)
      super(scope, result=nil, args=args, named=named)
    end
  end #IncludeMacro
end
