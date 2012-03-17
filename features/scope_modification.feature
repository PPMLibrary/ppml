Feature: scope modification

  To allow for non-local modifications of the source code
  a macro programmer should be able to:
  - add variables to the enclosing scope
  - add interface definition
  - add use statements
  - add subroutines
  from any macro invoked anywhere in a scope.
  
  Scenario: Declaring variables
    Given a macro "mpi_init" is defined as
    """
    % scope.var("info" => "INTEGER    :: info")
    call MPI_Init(info)

    """
    When I preprocess
    """
    program test 
      mpi_init()
    end program test

    """
    Then it should expand into
    """
    program test 
      implicit none
      INTEGER    :: info
      call MPI_Init(info)
    end program test
    
    """


