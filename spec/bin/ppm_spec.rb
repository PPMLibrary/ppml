require_relative '../spec_helper.rb'
require 'fileutils'
ENV['PATH'] += ":#{Dir.pwd}/bin"

describe "ppm" do
  describe "pp" do
    it "preprocesses STDIN and prints to STDOUT" do
      `echo "Hello World" | ./bin/ppm pp`.should == "Hello World\n"
    end
    it "takes a file argument and puts result to STDOUT" do
      `./bin/ppm pp examples/testdata/fortran.f`.should == <<-HEREDOC
PROGRAM test
  IMPLICIT NONE
  INTEGER, DIMENSION(50) :: a,b,c
  INTEGER :: x,i=2,j=49
  ! Int. J. Numer. Meth. Engng 2003; 56:935-960.
  FORALL (x=i:j)
    a(x)=b(x)+c(x) ! All these assignments are performed after the
    c(x)=b(x)-a(x) ! assignments in the preceding statement
  END FORALL
END PROGRAM
HEREDOC
    end
    it "takes '-o file' and puts result to 'file'" do
      `./bin/ppm pp examples/testdata/fortran.f -o examples/testdata/output/preprocessed.f`
      FileUtils.cmp('examples/testdata/output/preprocessed.f','examples/testdata/fortran_gold.f').should == true
    end
  end
end
