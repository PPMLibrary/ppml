Feature: Templating for fortran

  To cut down the code duplication in ppm we should be able to
  template subroutines and types and automatically create appropriate
  interfaces.

  Scenario: basic templates
    When I preprocess
    """
    module scope

    contains

      template<T:[integer, real]>
      subroutine first(x,y,info)
        T :: x
        T :: y
        integer :: info
        y = x
      end subroutine first

      template<T:[complex]>
      subroutine first(x,y,info)
        T :: x
        T :: y
        integer :: info
        y%real = x%real
        y%imag = x%imag
      end subroutine first

    end module scope

    """
    Then it should expand into
    """
    module scope
    implicit none

      interface first
        module procedure first_integer
        module procedure first_real
        module procedure first_complex
      end interface first

    contains

      subroutine first_integer(x,y,info)
        implicit none
        integer :: x
        integer :: y
        integer :: info
        y = x
      end subroutine first_integer

      subroutine first_real(x,y,info)
        implicit none
        real :: x
        real :: y
        integer :: info
        y = x
      end subroutine first_real

      subroutine first_complex(x,y,info)
        implicit none
        complex :: x
        complex :: y
        integer :: info
        y%real = x%real
        y%imag = x%imag
      end subroutine first_complex

    end module scope

    """
