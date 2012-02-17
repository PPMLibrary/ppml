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
  end
end
