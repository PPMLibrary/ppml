require 'antlr3'
require 'singleton'
require_relative '../parser'
require_relative 'templates'

module CG
  class Preprocessor
    include Singleton

    attr_accessor :macros, :lexer, :tokens, :parser, :tree_tokens, :tree_parser

    @@standard_macro_path = File.dirname(__FILE__)+'/defs/'
    @@user_macro_path = 'macros/'

    def self.standard_macro_path= p
      @@standard_macro_path = p
      Preprocessor.instance.reload_macros
    end

    def self.standard_macro_path
      @@standard_macro_path
    end

    def initialize
      initialize_macros_hash

      @working_dir = Dir.pwd
      reload_macros

      @templates = Templates.get_all
    end

    def cwd path
      @working_dir = path
      reload_macros
    end

    def reload_macros
      @macros.clear
      load_macros_from_glob "#{@@standard_macro_path}/*.mac"
      load_macros_from_glob "#{@working_dir+'/'+@@user_macro_path}/*.mac"
      # @macros.each { |name,m| m.expand_recursive_calls }
    end

    def process string
      # STDERR.puts "Parsing String\n\n#{string}"
      # print_lexer_tokens string
      # print_tree_tokens string
      @lexer = Lexer.new string
      @tokens = ANTLR3::CommonTokenStream.new @lexer
      unless all_hidden? @tokens
        @parser = Parser.new @tokens
        @tree_tokens = ANTLR3::AST::CommonTreeNodeStream.new @parser.prog.tree
        @tree_parser = TreeParser.new @tree_tokens, {templates: @templates}
        @tree_parser.prog.template.to_s
      else
        @tokens.map(&:text).join ''
      end
    end

    def expand(name, *args)
      @macros[name.to_s].expand(*args) if @macros.has_key? name.to_s
    end

    private

    def initialize_macros_hash
      @macros = {}
      @macros.instance_variable_set :@preprocessor, self
      def @macros.<<(m)
        # m.expand_recursive_calls
        self[m.name] = m
      end
      def @macros.+(m)
        m.each do |name,v|
          # v.expand_recursive_calls
          self[name] = v
        end
        self
      end
    end

    def load_macros_from_glob p
      Dir[p].each do |f|
        @macros += Macro.load f
      end
    end

    def all_hidden? toks
      ah = true
      toks.each { |t| ah = false unless t.channel == :hidden }
      return ah
    end

    def print_lexer_tokens string
      STDERR.puts "\n\nPrinting Lexer Tokens:\n\n"
      Lexer.new(string).each_with_index { |t,i| STDERR.puts "\t#{i} : #{t.inspect}" }
    end

    def print_tree_tokens string
      STDERR.puts "\n\nPrinting Tree Tokens\n\n"
      p = Parser.new Lexer.new(string)
      s = ANTLR3::AST::CommonTreeNodeStream.new p.prog.tree
      s.each { |t| STDERR.puts "#{t.name}\t| #{t.start_index}\t| #{t.stop_index}\t| inspect:\t#{t.inspect}" }
    end

  end
end
