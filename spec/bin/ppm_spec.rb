require_relative '../spec_helper.rb'

ENV['PATH'] += ":#{Dir.pwd}/bin"

describe "ppm" do
  describe "pp" do
    it "preprocesses STDIN and prints to STDOUT" do
      `echo "Hello World" | ./bin/ppm pp`.should == "Hello World\n"
    end
    it "takes a file argument and puts result to STDOUT" do
      `./bin/ppm pp examples/testdata/fortran.f`.should == <<-HEREDOC
program test
  integer, dimension(50) :: a,b,c
  integer :: x,i=2,j=49
  ! omar will be happy
  forall (x=i:j)
    a(x)=b(x)+c(x) ! All these assignments are performed after the
    c(x)=b(x)-a(x) ! assignments in the preceding statement
  end forall
end program
HEREDOC
    end
    it "takes '-o file' and puts result to 'file'" do
      `./bin/ppm pp examples/testdata/fortran.f -o examples/testdata/preprocessed.f`
      o = `cat examples/testdata/preprocessed.f | md5`
      g = `cat examples/testdata/preprocessed_gold.f | md5`
      o.should == g
    end
  end
end
