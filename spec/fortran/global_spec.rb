require_relative '../spec_helper'

module CG
  describe GlobalModule do
    before :all do
      read_configuration
    end

    before :each do
      @g = GlobalModule.instance
    end

    context "#to_s" do
      it "outputs the global parameter module with define_args function" do
        @g.to_s.should == <<EOF
MODULE ppm_autogenerated_global
  USE ppm_module_core
  USE ppm_module_user_numerics
  IMPLICIT NONE
  INTEGER, PARAMETER :: MK = ppm_kind_single

CONTAINS

  SUBROUTINE define_args
    IMPLICIT NONE
  END SUBROUTINE define_args

END MODULE ppm_autogenerated_global
EOF
      end
    end

    context "#arg" do
      it "adds an arg definition" do
        @g.arg name: :i, type: 'integer', default: 1, min: 0, max: 10, help_text: "'hello world'"
        @g.to_s.should == <<EOF
MODULE ppm_autogenerated_global
  USE ppm_module_core
  USE ppm_module_user_numerics
  IMPLICIT NONE
  INTEGER, PARAMETER :: MK = ppm_kind_single
  INTEGER :: i

CONTAINS

  SUBROUTINE define_args
    IMPLICIT NONE
    CALL arg(i, 'i', &
             default = 1, &
             min = 0, &
             max = 10, &
             help_text = 'hello world')
  END SUBROUTINE define_args

END MODULE ppm_autogenerated_global
EOF
      end
    end
  end
end
