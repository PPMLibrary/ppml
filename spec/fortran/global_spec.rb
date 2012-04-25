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
module global
  use ppm_module_ctrl
  use ppm_module_data
  implicit none
  contains
  subroutine define_args
    implicit none
  end subroutine define_args
  
end module global
EOF
      end
    end

    context "#arg" do
      it "adds an arg definition" do
        @g.arg name: :i, type: 'integer', default: 1, min: 0, max: 10, help_text: "'hello world'"
        @g.to_s.should == <<EOF
module global
  use ppm_module_ctrl
  use ppm_module_data
  implicit none
  integer :: i
  contains
  subroutine define_args
    implicit none
    call arg(i, 'i', &
             default = 1, &
             min = 0, &
             max = 10, &
             help_text = 'hello world')
  end subroutine define_args
  
end module global
EOF
      end
    end
  end
end
