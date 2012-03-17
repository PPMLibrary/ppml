require_relative '../spec_helper'

module CG
  describe Scope do

    describe "creating scopes" do
      it "creates nested scopes" do
        prog = Scope.new "program"
        s1 = Scope.new("subroutine1",prog)
        s2 = Scope.new("subsub",s1)
        s2.parent.should == s1
        s2.parent.parent.should == prog
        s1.parent.should == prog
        prog.child.should == s1
        prog.child.child.should == s2
        s1.child.should == s2
        prog.parent.should == nil
        s2.child.should == nil
      end
    end


    describe "Scope symbol tables" do
      before :each do
        @s = Scope.new "name"
      end

      it "adds use statements with #use" do
        @s.use 'some_module'
        @s.use 'other_module, only: variable'
        @s.use_statements.should == ['use some_module', 'use other_module, only: variable']
      end

      it "adds variables with #var" do
        @s.var("aVariable" => "TYPE(SomeType), DIMENSION(:)  :: aVariable")
        @s.var("i" => "INTEGER :: i")
        @s.variables.should == {"aVariable" => "TYPE(SomeType), DIMENSION(:)  :: aVariable","i" => "INTEGER :: i"}
      end
    end
  end
end
