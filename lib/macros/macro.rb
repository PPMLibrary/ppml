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


def transform body, pattern, replacement
  # check if arguments are used in the replacement
  replacement = replacement.split /(\$(?:\d+|\*))/
  max = 0
  splat = false
  replacement.map! do
    |piece|
    if piece == "$*"
      splat = true
      :splat
    elsif piece =~ /^\$(\d+)$/
      max = $~[1].to_i if $~[1].to_i > max
      $~[1].to_i
    else
      piece
    end
  end
  # turn pattern into propper regexp
  pattern = "(?<![a-z_0-9])" + pattern + "(?![a-z_0-9])"
  if splat or max > 0
    pattern << "\\((.*?)\\)"
  end
  # replace
  body.gsub! /#{pattern}/ do
    |match|
    rep = replacement
    if splat or max > 0
      args = []
      $~[1].scan /"[^"]*"|[^,]+/ do
        |arg|
        args << arg
      end
      rep.map! do
        |piece|
        if piece.is_a? Integer
          args[piece-1]
        elsif piece == :splat
          args[max..args.length].join "," unless max > args.length
        else
          piece
        end
      end
    end
    rep.join ''
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

    def expand context, iter, args, named, mods, ma, ma_named, bodies
      map = Macro.resolve_args @args, args, named
      map.merge! resolve_modifiers(mods, ma, ma_named)
      map["body"] = bodies[0]
      map["scope"] = context
      map["iter"] = iter
      # expand_recursive_calls(scope) unless @recursive_expanded
      erb = ERB.new @body, nil, "%-"
      erb.result Macro.binding_from_map(map)
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
