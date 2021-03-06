require 'antlr3'
require 'singleton'
require_relative '../parser'
require_relative 'templates'

module CG
  class Preprocessor
    include Singleton

    attr_accessor :macros, :lexer, :tokens, :parser, :tree_tokens, :tree_parser

    @@standard_macro_path = File.dirname(__FILE__)+'/defs/'
    @@user_macro_paths = ['macros/']

    def self.standard_macro_path= p
      @@standard_macro_path = p
      Preprocessor.instance.reload_macros
    end

    def self.standard_macro_path
      @@standard_macro_path
    end

    def self.user_macro_paths= p
      @@user_macro_paths = p
      Preprocessor.instance.reload_macros
    end

    def self.user_macro_paths
      @@user_macro_paths
    end

    def initialize
      initialize_macros_hash

      @working_dir = Dir.pwd
      reload_macros

      @templates = Templates.get_all

      @state_stack = []
    end

    def cwd path
      @working_dir = path
      reload_macros
    end

    def reload_macros
      @macros.clear
      load_macros_from_glob "#{@@standard_macro_path}/*.mac"
      @@user_macro_paths.each do | user_macro_path |
        unless user_macro_path.start_with? '/'
          load_macros_from_glob "#{@working_dir+'/'+user_macro_path}/*.mac"
        else
          load_macros_from_glob "#{user_macro_path}/*.mac"
        end
      end
      # @macros.each { |name,m| m.expand_recursive_calls }
    end

    def imacros
      @macros.reject {|n,m| !(m.is_a? IncludeMacro)}
    end

    def fmacros
      @macros.reject {|n,m| !(m.is_a? FunctionMacro)}
    end



    def process string, scope=nil
      # STDERR.puts "Parsing String\n\n#{string}"
      # print_lexer_tokens string
      # print_tree_tokens string

      saveState

      @lexer = Lexer.new string
      @tokens = ANTLR3::CommonTokenStream.new @lexer
      unless all_hidden? @tokens
        @parser = Parser.new @tokens
        @tree_tokens = ANTLR3::AST::CommonTreeNodeStream.new @parser.prog.tree
        @tree_parser = TreeParser.new @tree_tokens, {templates: @templates}, scope
        result = @tree_parser.prog.template.to_s
      else
        result = @tokens.map(&:text).join ''
      end

      restoreState

      result
    rescue
      STDERR.puts "An exception occured, here is some relevant output:"
      STDERR.puts "You were parsing:\n\n#{string}"
      print_lexer_tokens string
      print_tree_tokens string
      raise
    end

    def expand(name, *args)
      @macros[name.to_s].expand(*args) if @macros.has_key? name.to_s
    rescue ArgumentError => e
      STDERR.print "Fatal Error: Called macro #{name}"
      STDERR.print " in scope #{args[0].name}" if args[0]
      STDERR.puts " with wrong number of arguments"
      raise
    rescue SyntaxError => e
      STDERR.print "Fatal Error: Macro #{name}"
      STDERR.puts " contains a syntactical error"
      raise
    rescue NoMethodError => e
      STDERR.print "Fatal Error: Macro #{name}"
      STDERR.puts " calls an undefined method"
      raise
    end

    private

    class PreprocessorState

      attr_accessor :lexer, :tokens, :parser, :tree_tokens, :tree_parser

    end

    def saveState
      newState = PreprocessorState.new
      newState.lexer = @lexer
      newState.tokens = @tokens
      newState.parser = @parser
      newState.tree_tokens = @tree_tokens
      newState.tree_parser = @tree_parser

      @state_stack.push newState
    end

    def restoreState
      oldState = @state_stack.pop
      @lexer = oldState.lexer
      @tokens = oldState.tokens
      @parser = oldState.parser
      @tree_tokens = oldState.tree_tokens
      @tree_parser = oldState.tree_parser
    end

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
