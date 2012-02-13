require_relative("../spec_helper")

module CG
  describe Preprocessor do
    context "simple macro substitution" do
      it "substitutes simple function call macros" do
        pending "plugging in the parser"
        p = Preprocessor.new
        m = Macro.new('simple', 'body')
        p.macros << m
        p.process("simple()").should == 'body'
      end
    end
  end
end
