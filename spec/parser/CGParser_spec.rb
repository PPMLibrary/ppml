require_relative '../spec_helper'

module CG
  describe Parser do
    def parser_for_string(string)
      l = Lexer.new(string)
      p = Parser.new(l)
    end

    program_definition = "PROGRAM name\nend program\n"

    context "recognize standard fortran" do
      it "recognizes all fortran code as an fblock" do
        p = parser_for_string(program_definition)
        p.prog
      end
    end
  end
end
