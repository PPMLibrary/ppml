module CG
  describe FortranSubroutine do

    before :each do
      @s = FortranSubroutine.new "name"
    end

    context "#use" do
      it "adds a use statement" do
        @s.use "module_ctrl"
        @s.to_s.to_s.should == <<EOF
SUBROUTINE name
  USE module_ctrl
  IMPLICIT NONE
END SUBROUTINE name
EOF
      end
    end

    context "#add" do
      it "adds new lines of code" do
        @s.add "2 + 3"
        @s.to_s.should == <<EOF
SUBROUTINE name
  IMPLICIT NONE
  2 + 3
END SUBROUTINE name
EOF
      end
    end

    context "#args" do
      it "defines the argument list" do
        @s.args x: "INTEGER :: x", info: "INTEGER :: info"
        @s.to_s.should == <<EOF
SUBROUTINE name(x, info)
  IMPLICIT NONE
  INTEGER :: x
  INTEGER :: info
END SUBROUTINE name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @s.var :ndim, "INTEGER :: ndim"
        @s.to_s.should == <<EOF
SUBROUTINE name
  IMPLICIT NONE
  INTEGER :: ndim
END SUBROUTINE name
EOF
      end
    end
  end
end
