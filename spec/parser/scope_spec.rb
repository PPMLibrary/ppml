require_relative '../spec_helper'

module CG
  describe Scope do

    describe "creating scopes" do
      it "creates nested scopes" do
        prog = Scope.new :program, "program"
        s1 = Scope.new(:subroutine, "subroutine1",prog)
        s2 = Scope.new(:subroutine, "subsub",s1)
        s2.parent.should == s1
        s2.parent.parent.should == prog
        s1.parent.should == prog
        prog.kind.should == :program
        s1.parent.kind.should == :program
        s2.parent.kind.should == :subroutine
        prog.child.should == s1
        prog.child.child.should == s2
        s1.child.should == s2
        prog.parent.should == nil
        s2.child.should == nil
      end
    end


    describe "scope modification" do
      before :each do
        @s = Scope.new :subroutine, "name"
      end

      it "adds use statements with #use" do
        @s.use :some_module
        @s.use :other_module, 'use other_module, only: variable'
        @s.use_statements.should == {some_module:'use some_module', other_module: 'use other_module, only: variable'}
      end

      it "adds variables with #raw_var" do
        @s.raw_var(aVariable: "TYPE(SomeType), DIMENSION(:)  :: aVariable")
        @s.raw_var(i: "INTEGER :: i")
        @s.variables.should == {aVariable: "TYPE(SomeType), DIMENSION(:)  :: aVariable", i: "INTEGER :: i"}
      end

      it "allows the type of a symbol to be set as second argument to #var" do
        @s.raw_var({:c => "type(something) :: c"}, :particles)
        @s.variables.should == {:c => "type(something) :: c"}
        @s.type_of(:c).should == :particles
      end
    end
  end
end
