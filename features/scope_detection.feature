Feature: Scope Detection

  To enable insertion of new variables, initialization and cleanup
  code, and automatic interface definitions we need to detect target
  locations and make them available in the calling macro code.

  Background: running in comment mode
    Given setting comment_mode is on

  Scenario: program statement
    When I preprocess
    """
      
      ! leading comment
      
      program sample ! some comment for omar
        ! first inner comment
        use something
        ! after use comment
        implicit none
        ! after implicit comment
        integer :: i
        ! body comment
        i = 42
        ! before end comment
      end program
      ! trailing comment

    """
    Then it should expand into
    """
      
      ! leading comment
      
      program sample ! some comment for omar
        ! first inner comment
        use something
        ! use statements
        ! after use comment
        implicit none
        ! variable definitions
        ! after implicit comment
        integer :: i
        ! body comment
        i = 42
        ! subroutines
        ! before end comment
      end program
      ! trailing comment

    """

  Scenario: module statement
    When I preprocess
    """
      
      ! leading comment
      
      module test ! some comment for omar
        use something
        implicit none
        integer :: i
        i = 42
      end module

    """
    Then it should expand into
    """
      
      ! leading comment
      
      module test ! some comment for omar
        use something
        ! use statements
        implicit none
        ! variable definitions
        integer :: i
        i = 42
        ! subroutines
      end module

    """

  Scenario: subroutine statement
    When I preprocess
    """
      
      ! leading comment
      
      subroutine test(some,args) ! some comment for omar
        use something
        implicit none
        integer :: i
        i = 42
      end subroutine test

    """
    Then it should expand into
    """
      
      ! leading comment
      
      subroutine test(some,args) ! some comment for omar
        use something
        ! use statements
        implicit none
        ! variable definitions
        integer :: i
        i = 42
        ! subroutines
      end subroutine test

    """

  Scenario: nested subroutine
    When I preprocess
    """
      
      ! leading comment
      
      program test ! some comment for omar
        use something
        implicit none
        integer :: i
        i = 42
        contains

      ! leading comment
      
        integer function nested
          use stuff
          integer k
          k = 13
        end function
      
      ! leading comment
      
        subroutine nested
          use stuff
          integer k
          k = 13
        end subroutine
      end program

    """
    Then it should expand into
    """
      
      ! leading comment
      
      program test ! some comment for omar
        use something
        ! use statements
        implicit none
        ! variable definitions
        integer :: i
        i = 42
        contains
        ! subroutines

      ! leading comment
      
        integer function nested
          use stuff
          ! use statements
          implicit none
          ! variable definitions
          integer k
          k = 13
          ! subroutines
        end function
      
      ! leading comment
      
        subroutine nested
          use stuff
          ! use statements
          implicit none
          ! variable definitions
          integer k
          k = 13
          ! subroutines
        end subroutine
      end program

    """

  Scenario: function statements
    When I preprocess
    """
    integer function test(a)
      use something
      implicit none
      integer :: i
      i = 42
    end function test

    """
    Then it should expand into
    """
    integer function test(a)
      use something
      ! use statements
      implicit none
      ! variable definitions
      integer :: i
      i = 42
      ! subroutines
    end function test

    """

  Scenario: type statement
    When I preprocess
    """
      module test
        use something
        implicit none
        integer :: i
        type :: ppm_t_type
          integer :: j
          contains
          procedure :: proc => proc_implementation
        end type ppm_t_type
        i = 42
      end module

    """
    Then it should expand into
    """
      module test
        use something
        ! use statements
        implicit none
        ! variable definitions
        integer :: i
        type :: ppm_t_type
          integer :: j
          contains
          procedure :: proc => proc_implementation
        end type ppm_t_type
        i = 42
        ! subroutines
      end module

    """

    When I preprocess
    """
      module test
        use something
        implicit none
        integer :: i
        type, extends(supertype) :: ppm_t_type
          contains
          procedure :: proc => proc_implementation
        end type ppm_t_type
        i = 42
      end module

    """
    Then it should expand into
    """
      module test
        use something
        ! use statements
        implicit none
        ! variable definitions
        integer :: i
        type, extends(supertype) :: ppm_t_type
          contains
          procedure :: proc => proc_implementation
        end type ppm_t_type
        i = 42
        ! subroutines
      end module

    """

  Scenario: interface statement
    When I preprocess
    """
      module m
        interface  
          subroutine test(arg1,arg2)
          integer, intent(in)  :: arg1
          integer, intent(out) :: arg2
          end subroutine test
        end interface
      end module

    """
    Then it should expand into
    """
      module m
        ! use statements
        implicit none
        ! variable definitions
        interface  
          subroutine test(arg1,arg2)
          ! use statements
          integer, intent(in)  :: arg1
          integer, intent(out) :: arg2
          end subroutine test
        end interface
        ! subroutines
      end module

    """


  Scenario: typebound procedure

  Scenario: nested type
