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
  
  Scenario: basic templates using suffixes
    When I preprocess
    """
    module scope

    contains

      template <T:[integer, real]> suffixes [i,r]
      subroutine first(x,y,info)
        T :: x
        T :: y
        integer :: info
        y = x
      end subroutine first

    end module scope

    """
    Then it should expand into
    """
    module scope
    implicit none

      interface first
        module procedure first_i
        module procedure first_r
      end interface first

    contains

      subroutine first_i(x,y,info)
        implicit none
        integer :: x
        integer :: y
        integer :: info
        y = x
      end subroutine first_i

      subroutine first_r(x,y,info)
        implicit none
        real :: x
        real :: y
        integer :: info
        y = x
      end subroutine first_r

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

      template * <T:[class(particle_t)], U:[integer(8), type(mesh_t)]>
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
        module procedure multi_class_particle_t__integer_8_
        module procedure multi_class_particle_t__type_mesh_t_
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

      subroutine multi_class_particle_t__integer_8_(x,y,info)
        implicit none
        class(particle_t) :: x
        integer(8) :: y
        integer :: info
        y = x
      end subroutine multi_class_particle_t__integer_8_

      subroutine multi_class_particle_t__type_mesh_t_(x,y,info)
        implicit none
        class(particle_t) :: x
        type(mesh_t) :: y
        integer :: info
        y = x
      end subroutine multi_class_particle_t__type_mesh_t_

    end module scope

    """

  Scenario: templates and macros
    Given a macro "pmac" with argument list ("type") is defined as
    """
    ! inside pmac
    print *,'<%= type %>'

    """
    When I preprocess
    """
    module scope

    contains

      template <T:[integer, real]>
      subroutine sub(x,info)
        T :: x
        integer :: info
        pmac(T)
      end subroutine sub

    end module scope

    """
    Then it should expand into
    """
    module scope
    implicit none

      interface sub
        module procedure sub_integer
        module procedure sub_real
      end interface sub

    contains

      subroutine sub_integer(x,info)
        implicit none
        integer :: x
        integer :: info
        ! inside pmac
        print *,'integer'
      end subroutine sub_integer

      subroutine sub_real(x,info)
        implicit none
        real :: x
        integer :: info
        ! inside pmac
        print *,'real'
      end subroutine sub_real

    end module scope

    """
