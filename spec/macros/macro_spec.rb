require_relative "../spec_helper"

module CG
  describe Macro do
    context "utilities" do
      describe "#binding_from_map" do
        it "transforms hash key-value pairs into variables in a binding" do
          b = Macro.binding_from_map({var: "value"})
          eval("var",b).should == "value"
        end
      end
      describe "#parse_arglist" do
        it "turns an arguments string into a map" do
          Macro.parse_arglist("a,b,c").should == {'a'=>:required, 'b'=>:required, 'c'=>:required, :splat => nil}
          Macro.parse_arglist("a=b,c=d,e").should == {'a'=>'b', 'c'=>'d', 'e'=>:required, :splat => nil}
        end
      end
    end

    context "macro loading" do

      describe "#load" do
        it "creates Macro instances for all macro defintions in a file path" do
          path = "examples/testdata/macros/function_call.mac"
          macros = Macro.load(path)
          macros.keys.size.should == 3
          macros.keys.should include "simple"
          macros.keys.should include "fail"
          macros.keys.should include "override"
        end
      end

      describe "#load_file" do
        it "creates Macro instances for all macro defintions in an IO object" do
          path = "examples/testdata/macros/function_call.mac"
          macros = Macro.load_file File.open(path)
          macros.keys.size.should == 3
          macros.keys.should include "simple"
          macros.keys.should include "fail"
          macros.keys.should include "override"
        end
      end

      describe "mixed types" do
        it "creates subclasses of Macro depending on definition" do
          path = "examples/testdata/macros/mixed.mac"
          macros = Macro.load_file File.open(path)
          macros.keys.size.should == 2
          macros.keys.should include "function_call"
          macros.keys.should include "looping"
          macros['function_call'].should be_a FunctionMacro
          macros['looping'].should be_a ForeachMacro
        end
      end
    end
  end

  describe FunctionMacro do
    it "takes name, body and args (optional) as arguments" do
    end

    context "macro expansion" do
      it "returns the body with arguments replaced" do
        m = FunctionMacro.new("test",<<-'BODY','a,b,c')
        plain text line
        substituting <%= a %>, <%= b %> and <%= c %>
        more text
        BODY
        m.expand(nil,nil,['e','"string, args"','g'],nil).should == <<-REPLACEMENT
        plain text line
        substituting e, "string, args" and g
        more text
        REPLACEMENT
      end
    end
  end

  describe ForeachMacro do
    it "takes name, body, args, modifiers and bodies as arguments" do
    end
  end
end
