Feature: Scope Detection

  To enable insertion of new variables, initialization and cleanup
  code, and automatic interface definitions we need to detect target
  locations and make them available in the calling macro code.

  Background: running in comment mode
    Comment mode makes the scope object print a comment line in all
    target locations.
    Given setting comment_mode is on

  Scenario: program statement
    When I preprocess
    """
      
      ! leading comment
      
      program sample ! some comment for omar
        use something
        implicit none
        integer :: i
        i = 42
      end program

    """
    Then it should expand into
    """
      
      ! leading comment
      
      program sample ! some comment for omar
        use something
        ! use statements
        implicit none
        ! interfaces
        ! variable definitions
        integer :: i
        i = 42
        ! subroutines
      end program

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
      
        subroutine nested
          use stuff
          integer k
          k = 13
        end subroutine
      
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

  Scenario: type statement

  Scenario: typebound procedure

  Scenario: nested type
