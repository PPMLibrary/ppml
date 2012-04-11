module CG
  describe FortranModule do

    before :each do
      @m = FortranModule.new "name"
    end

    context "#to_s" do
      it "outputs the correct fortran code for the module" do
        @m.to_s.should == <<EOF
module name
  implicit none
end module name
EOF
      end
    end

    context "#use" do
      it "adds a use statement" do
        @m.use "module_ctrl"
        @m.to_s.should == <<EOF
module name
  use module_ctrl
  implicit none
end module name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @m.var :ndim, "integer :: ndim"
        @m.to_s.should == <<EOF
module name
  implicit none
  integer :: ndim
end module name
EOF
      end
    end

    context "#subroutine" do
      it "adds a subroutine definition" do
        @m.subroutine FortranSubroutine.new "subname"
        @m.to_s.should == <<EOF
module name
  implicit none
  contains
  subroutine subname
    implicit none
  end subroutine subname
  
end module name
EOF
      end
    end
  end
end
