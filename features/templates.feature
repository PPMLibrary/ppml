Feature: Templating for fortran

  To cut down the code duplication in ppm we should be able to
  template subroutines and types and automatically create appropriate
  interfaces.

  Scenario: basic templates
    When I preprocess
    """
    module scope

    contains

      template <T:[integer, real]>
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

  Scenario: multiple variables
    When I preprocess
    """
    module scope

    contains

      template <T:[integer, real], U:[real, integer]>
      subroutine multi(x,y,info)
        T :: x
        U :: y
        integer :: info
        y = x
      end subroutine multi

      template * <T:[logical], U:[integer, real]>
      subroutine multi(x,y,info)
        T :: x
        U :: y
        integer :: info
        y = x
      end subroutine multi

    end module scope

    """
    Then it should expand into
    """
    module scope
    implicit none

      interface multi
        module procedure multi_integer_real
        module procedure multi_real_integer
        module procedure multi_logical_integer
        module procedure multi_logical_real
      end interface multi

    contains

      subroutine multi_integer_real(x,y,info)
        implicit none
        integer :: x
        real :: y
        integer :: info
        y = x
      end subroutine multi_integer_real

      subroutine multi_real_integer(x,y,info)
        implicit none
        real :: x
        integer :: y
        integer :: info
        y = x
      end subroutine multi_real_integer

      subroutine multi_logical_integer(x,y,info)
        implicit none
        logical :: x
        integer :: y
        integer :: info
        y = x
      end subroutine multi_logical_integer

      subroutine multi_logical_real(x,y,info)
        implicit none
        logical :: x
        real :: y
        integer :: info
        y = x
      end subroutine multi_logical_real

    end module scope

    """

