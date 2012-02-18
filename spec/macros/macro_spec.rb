require_relative("../spec_helper")

module CG
  describe Macro do
    it "takes name, body and args (optional) as arguments" do
    end

    context "macro expansion" do
      it "returns the body with arguments replaced" do
        m = Macro.new("test",<<-'BODY',['a','b','c'])
        plain text line
        substituting <%= a %>, <%= b %> and <%= c %>
        more text
        BODY
        m.expand(nil,['e','f','g']).should == <<-REPLACEMENT
        plain text line
        substituting e, f and g
        more text
        REPLACEMENT
      end
    end
    
    context "macro loading" do
      it "should create Macro instances for all macro defintions in a file" do
        path = "examples/testdata/macros/function_call.mac"
        macros = Macro.load(path)
        macros.keys.size.should == 3
        macros.keys.should include "simple"
        macros.keys.should include "with_ruby"
        macros.keys.should include "override"
      end
      it "should create Macro instances for all macro defintions in a string" do
        path = "examples/testdata/macros/function_call.mac"
        macros = Macro.load_file File.open(path)
        macros.keys.size.should == 3
        macros.keys.should include "simple"
        macros.keys.should include "with_ruby"
        macros.keys.should include "override"
      end
    end
  end
end
