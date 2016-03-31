Feature: scope modification

  To allow for non-local modifications of the source code
  a macro programmer should be able to:
  - add variables to the enclosing scope
  - add interface definition
  - add use statements
  - add subroutines
  from any macro invoked anywhere in a scope.

  Scenario: Declaring variables
    Given setting predictable_mangle_prefix is on
    Given a macro "mpi_init" is defined as
    """
    % scope.var :info, "INTEGER"
    CALL MPI_Init(info)

    """
    When I preprocess
    """
    PROGRAM test
      mpi_init()
    END PROGRAM test

    """
    Then it should expand into
    """
    PROGRAM test
      IMPLICIT NONE
      INTEGER :: mangled_info
      CALL MPI_Init(mangled_info)
    END PROGRAM test

    """

  Scenario: Declaring module uses
    Given a macro "ppm_init" is defined as
    """
    % scope.use "ppm_module_init"
    CALL ppm_init(2,eps,tol,0,info)

    """
    When I preprocess
    """
    PROGRAM test
      ppm_init()
    END PROGRAM test

    """
    Then it should expand into
    """
    PROGRAM test
      USE ppm_module_init
      IMPLICIT NONE
      CALL ppm_init(2,eps,tol,0,info)
    END PROGRAM test

    """


