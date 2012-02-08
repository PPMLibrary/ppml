Feature: preprocessing standard fortran

  Using the client generator as a preprocesor for fortran should not
  interfere with standard fortran code. All the standard fortran
  syntax should be ignored by the macro expansion.

  Scenario Outline: preprocess fortran
    Given the input is standard fortran
    When I pass "<code>"
    Then it should remain unchanged

    Scenarios: top level constructs
      | code                                       |
      | program sample\nend program                |
      | subroutine name(args)\nend subroutine name |

