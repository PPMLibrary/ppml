require 'antlr3'
require_relative '../parser'

module CG
  class Preprocessor
    attr_accessor :macros
    @@standard_macro_path = File.dirname(__FILE__)+'/defs/'
    @@user_macro_path = 'macros/'

    def self.standard_macro_path=(p)
      @@standard_macro_path = p
    end
    def self.standard_macro_path
      @@standard_macro_path
    end

    def initialize(pwd=nil)
      @macros = {}
      def @macros.<<(m)
        m.expand_recursive_calls(self)
        self[m.name] = m
      end
      def @macros.+(m)
        m.each do |name,v|
          v.expand_recursive_calls(self)
          self[name] = v
        end
        self
      end
      Dir["#{@@standard_macro_path}/*.mac"].each do |f|
        @macros += Macro.load f
      end
      Dir["#{pwd+'/'+@@user_macro_path}/*.mac"].each do |f|
        @macros += Macro.load f
      end if pwd

      @macros.each { |name,m| m.expand_recursive_calls(@macros) }

      @templates = ANTLR3::Template::Group.new do
        define_template( :prog,        "<%= @lines.join(\"\n\") %>\n")
        define_template( :line,        "<%= @in %>")
        define_template( :fortran,     "<%= @in %>")
        define_template( :fcall_macro, "<%= @p.expand(@name, @result, @args, Hash[*@namedargs.zip(@namedvalues).flatten]) %>" )
      end
    end

    def process(input)
      parser = parser_for_string(input)
      parser.prog.template.to_s
    end

    def expand(name, *args)
      @macros[name.to_s].expand(*args) if @macros.has_key? name.to_s
    end

    private

    def print_lexer_tokens string
      STDERR.puts "\n\nPrinting Lexer Tokens:\n\n"
      Lexer.new(string).each_with_index { |t,i| STDERR.puts "\t#{i} : #{t.inspect}" }
    end

    def print_tree_tokens string
      STDERR.puts "\n\nPrinting Tree Tokens\n\n"
      p = Parser.new Lexer.new(string)
      p.preprocessor = self
      s = ANTLR3::AST::CommonTreeNodeStream.new p.prog.tree
      s.each { |t| STDERR.puts "#{t.name}\t| #{t.start_index}\t| #{t.stop_index}\t| inspect:\t#{t.inspect}" }
    end

    def parser_for_string(string)
      # STDERR.puts "Parsing String\n\n#{string}"
      # print_lexer_tokens string
      # print_tree_tokens string
      l = Lexer.new string
      token_stream = ANTLR3::CommonTokenStream.new l
      p = Parser.new token_stream
      p.preprocessor = self
      s = ANTLR3::AST::CommonTreeNodeStream.new p.prog.tree
      t = TreeParser.new s, {templates: @templates}
      t.preprocessor = self
      # t.stream = token_stream
      t
    end
  end
end
