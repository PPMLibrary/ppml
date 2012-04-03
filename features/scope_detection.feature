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
        ! more comments
        use something
        ! more comments
        implicit none
        ! more comments
        integer :: i
        ! more comments
        i = 42
        ! more comments
      end program
      ! more comments

    """
    Then it should expand into
    """
      
      ! leading comment
      
      program sample ! some comment for omar
        ! more comments
        use something
        ! use statements
        ! more comments
        implicit none
        ! interfaces
        ! variable definitions
        ! more comments
        integer :: i
        ! more comments
        i = 42
        ! subroutines
        ! more comments
      end program
      ! more comments

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
        ! interfaces
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
        ! interfaces
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
        ! interfaces
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
          ! interfaces
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
          ! interfaces
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
      ! interfaces
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
        ! interfaces
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
        ! interfaces
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

  Scenario: typebound procedure

  Scenario: nested type
