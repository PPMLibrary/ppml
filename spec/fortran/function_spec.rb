module CG
  describe FortranFunction do

    before :each do
      @f = FortranFunction.new "name", "INTEGER", "resval"
    end

    context "#use" do
      it "adds a use statement" do
        @f.use "module_ctrl"
        @f.to_s.to_s.should == <<EOF
FUNCTION name result(resval)
  USE module_ctrl
  IMPLICIT NONE
  INTEGER :: resval
END FUNCTION name
EOF
      end
    end

    context "#add" do
      it "adds new lines of code" do
        @f.add "name = 2 + 3"
        @f.to_s.should == <<EOF
FUNCTION name result(resval)
  IMPLICIT NONE
  INTEGER :: resval
  name = 2 + 3
END FUNCTION name
EOF
      end
    end

    context "#args" do
      it "defines the argument list" do
        @f.args x: "INTEGER :: x", info: "INTEGER :: info"
        @f.to_s.should == <<EOF
FUNCTION name(x, info) result(resval)
  IMPLICIT NONE
  INTEGER :: resval
  INTEGER :: x
  INTEGER :: info
END FUNCTION name
EOF
      end
    end

    context "#var" do
      it "adds a variable definition" do
        @f.var :ndim, "INTEGER :: ndim"
        @f.to_s.should == <<EOF
FUNCTION name result(resval)
  IMPLICIT NONE
  INTEGER :: resval
  INTEGER :: ndim
END FUNCTION name
EOF
      end
    end
  end
end
