require_relative "../spec_helper"

module CG
  describe Rhs do

    context "rhs loading" do

      describe "#load" do
        it "creates an Rhs instance for a file path" do
          path = "examples/testdata/test.rhs"
          rhs = Rhs.load(path)
          rhs.rhs_func.to_s.should == <<-eos
function test(t, pset1, wp, dwp, idata, ldata, rdata)
  implicit none
  integer :: test
  integer, intent(in) :: t
  real(mk), dimension(:,:), pointer :: pset1
  real(mk), dimension(:,:), pointer :: wp
  real(mk), dimension(:,:), pointer :: dwp
  integer, dimension(:,:), intent(in), optional :: idata
  logical, dimension(:,:), intent(in), optional :: ldata
  real(mk), dimension(:,:), intent(in), optional :: rdata
  ! arguments cannot be changed but erb templates are allowed
  print *,t, <%= conf.ppm.dim %>
  
end function test
eos
        
        end
      end

#      describe "#load_file" do
#        it "creates Macro instances for all macro defintions in an IO object" do
#          path = "examples/testdata/macros/function_call.mac"
#          macros = Macro.load_file File.open(path)
#          macros.keys.size.should == 3
#          macros.keys.should include "simple"
#          macros.keys.should include "fail"
#          macros.keys.should include "override"
#        end
#      end

#      describe "mixed types" do
#        it "creates subclasses of Macro depending on definition" do
#          path = "examples/testdata/macros/mixed.mac"
#          macros = Macro.load_file File.open(path)
#          macros.keys.size.should == 2
#          macros.keys.should include "function_call"
#          macros.keys.should include "looping"
#          macros['function_call'].should be_a FunctionMacro
#          macros['looping'].should be_a ForeachMacro
#        end
#      end
    end
  end

#  describe FunctionMacro do
#    it "takes name, body and args (optional) as arguments" do
#    end
#
#    context "macro expansion" do
#      it "returns the body with arguments replaced" do
#        m = FunctionMacro.new("test",<<-'BODY','a,b,c')
#        plain text line
#        substituting <%= a %>, <%= b %> and <%= c %>
#        more text
#        BODY
#        m.expand(nil,nil,['e','"string, args"','g'],nil).should == <<-REPLACEMENT
#        plain text line
#        substituting e, "string, args" and g
#        more text
#        REPLACEMENT
#      end
#    end
#  end
#
#  describe ForeachMacro do
#    it "takes name, body, args, modifiers and bodies as arguments" do
#    end
#  end
end
