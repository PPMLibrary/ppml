module CG
  describe FortranModule do

    before :each do
      @m = FortranModule.new "name"
    end

    context "#to_s" do
      it "outputs the correct fortran code for the module" do
        @m.to_s.should == <<EOF
MODULE name
  IMPLICIT NONE
END MODULE name
EOF
      end
    end

    context "#use" do
      it "adds a use statement" do
        @m.use "module_ctrl"
        @m.to_s.should == <<EOF
MODULE name
  USE module_ctrl
  IMPLICIT NONE
END MODULE name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @m.var :ndim, "INTEGER :: ndim"
        @m.to_s.should == <<EOF
MODULE name
  IMPLICIT NONE
  INTEGER :: ndim
END MODULE name
EOF
      end
    end

    context "#subroutine" do
      it "adds a subroutine definition" do
        @m.subroutine FortranSubroutine.new "subname"
        @m.to_s.should == <<EOF
MODULE name
  IMPLICIT NONE
CONTAINS
  SUBROUTINE subname
    IMPLICIT NONE
  END SUBROUTINE subname
END MODULE name
EOF
      end
    end
  end
end
