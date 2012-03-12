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
  end
end
