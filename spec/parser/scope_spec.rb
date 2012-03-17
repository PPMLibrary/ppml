require_relative '../spec_helper'

module CG
  describe Scope do
    describe "#program" do
      it "creates a program scope" do
        s = Scope.program "name"
        s.should be_an_instance_of ProgramScope
        s.name.should == "name"
      end
    end

    describe "use statements" do
      s = Scope.new "name"
      s.use 'some_module'
      s.use 'other_module, only: variable'
      s.use_statements.should == ['use some_module', 'use other_module, only: variable']
    end
  end
end
