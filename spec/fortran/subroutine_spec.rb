module CG
  describe FortranSubroutine do

    before :each do
      @s = FortranSubroutine.new "name"
    end

    context "#use" do
      it "adds a use statement" do
        @s.use "module_ctrl"
        @s.to_s.to_s.should == <<EOF
subroutine name
  use module_ctrl
  implicit none
end subroutine name
EOF
      end
    end

    context "#add" do
      it "adds new lines of code" do
        @s.add "2 + 3"
        @s.to_s.should == <<EOF
subroutine name
  implicit none
  2 + 3
end subroutine name
EOF
      end
    end

    context "#args" do
      it "defines the argument list" do
        @s.args x: "integer :: x", info: "integer :: info"
        @s.to_s.should == <<EOF
subroutine name(x, info)
  implicit none
  integer :: x
  integer :: info
end subroutine name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @s.var :ndim, "integer :: ndim"
        @s.to_s.should == <<EOF
subroutine name
  implicit none
  integer :: ndim
end subroutine name
EOF
      end
    end
  end
end
