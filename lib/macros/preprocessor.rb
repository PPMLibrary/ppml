module CG
  class Preprocessor
    attr_accessor :macros

    def initialize
      @macros = {}
      def @macros.<<(m)
        self[m.name] = m
      end
      def @macros.+(m)
        self.merge(m)
      end

      @templates = ANTLR3::Template::Group.new do
        define_template( :prog,        "<%= @lines.join(\"\n\") %>")
        define_template( :line,        "<%= @in %>")
        define_template( :fortran,     "<%= @in %>")
        define_template( :fcall_macro, "<%= @p.expand(@name, @result, @args) %>" )
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

    def print_lexer_tokens(string)
      puts ''
      puts ''
      Lexer.new(string).each { |t| pp t if !t.hidden? }
      puts ''
      puts ''
    end

    def parser_for_string(string)
      # print_lexer_tokens(string)
      # STDOUT.flush
      p = Parser.new(Lexer.new(string))
      p.preprocessor = self
      t = TreeParser.new(ANTLR3::AST::CommonTreeNodeStream.new(p.prog.tree),
                         {templates: @templates})
      t.preprocessor = self
      t

    end
  end
end
