require_relative '../spec_helper'

module CG
  describe Transform do
    it "transforms variable names" do
      t = Transform.new "a", "b"
      t.transform("a = 3*a + c").should == "b = 3*b + c"
    end

    it "recognizes and replaces arg references" do
      t = Transform.new "a", "b(t,$1)"
      t.transform("a(x) = 3*a(y) + c").should == "b(t,x) = 3*b(t,y) + c"
      t = Transform.new "a", "b($3)"
      t.transform("a(x,y,z)").should == "b(z)"
    end

    it "accepts splat arguments" do
      t = Transform.new "a", "b(x, $*)"
      t.transform("a(x,y,z)").should == "b(x, x,y,z)"
      t = Transform.new "a", "b($3, $*)"
      t.transform("a(a,b,c,x,y,z)").should == "b(c, x,y,z)"
    end

    context "#arg_split" do
      it "works like String.split ',' but skips quoted commas" do
        Transform.arg_split('a,"b,\"c,d",e').should == ["a", '"b,\"c,d"', "e"]
      end
    end
  end
end
