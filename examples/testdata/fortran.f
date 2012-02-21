program test
  integer, dimension(50) :: a,b,c
  integer :: x,i=2,j=49
  ! omar will be happy
  forall (x=i:j)
    a(x)=b(x)+c(x) ! All these assignments are performed after the
    c(x)=b(x)-a(x) ! assignments in the preceding statement
  end forall
end program
