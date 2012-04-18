module CG
  describe FortranFunction do

    before :each do
      @f = FortranFunction.new "name","integer","resval"
    end

    context "#use" do
      it "adds a use statement" do
        @f.use "module_ctrl"
        @f.to_s.to_s.should == <<EOF
function name result(resval)
  use module_ctrl
  implicit none
  integer :: resval
end function name
EOF
      end
    end

    context "#add" do
      it "adds new lines of code" do
        @f.add "name = 2 + 3"
        @f.to_s.should == <<EOF
function name result(resval)
  implicit none
  integer :: resval
  name = 2 + 3
end function name
EOF
      end
    end

    context "#args" do
      it "defines the argument list" do
        @f.args x: "integer :: x", info: "integer :: info"
        @f.to_s.should == <<EOF
function name(x, info) result(resval)
  implicit none
  integer :: resval
  integer :: x
  integer :: info
end function name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @f.var :ndim, "integer :: ndim"
        @f.to_s.should == <<EOF
function name result(resval)
  implicit none
  integer :: resval
  integer :: ndim
end function name
EOF
      end
    end
  end
end
