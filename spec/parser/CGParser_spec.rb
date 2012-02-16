require_relative '../spec_helper'

module CG
  describe Parser do
    def parser_for_string(string)
      p = Parser.new(Lexer.new(string))
      t = TreeParser.new(ANTLR3::AST::CommonTreeNodeStream.new(p.prog.tree),
                         {templates: get_templates})
    end

    def print_lexer_tokens(string)
      Lexer.new(string).exhaust.each { |t| pp t if !t.hidden? }
    end

    def get_templates
      ANTLR3::Template::Group.new do
        define_template( :fcall_macro, <<-'END'.strip )
        [<% puts "Hello, world!" %>]
        END
      end
    end

    fortran_program = <<HDC
PROGRAM name
  integer i
  bla bla bla
  macro_name()
  some more shit
end program
HDC

    context "recognize standard fortran" do
      it "recognizes all fortran code as an fline" do
        t = parser_for_string(fortran_program)
        # g = ANTLR3::Template::Group.new(templates)
        # c = t.create_template(templates)
        puts t.prog
        pp t.prog.template
      end
    end
  end
end
