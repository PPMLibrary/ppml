module CG
  class Preprocessor
    attr_accessor :macros

    def initialize
      @macros = []
    end

    def process(input)
      parser = Parser.new(Lexer.new(input))
      input
    end

  end
end
