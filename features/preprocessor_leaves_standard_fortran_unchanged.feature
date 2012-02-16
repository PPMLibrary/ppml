Feature: preprocessing standard fortran

  If the preprocessor is to be used with existing
  fortran code, it has to be generic
  a user should be able to preprocess standard fortran
  which should pass without modification

  Scenario Outline: preprocess standard fortran
    Given the input is standard fortran
    When I preprocess "<code>"
    Then it should remain unchanged

    Examples: top level constructs
      | code                                         |
      | program sample\nend program\n                |
      | subroutine name(args)\nend subroutine name\n |

    Examples: variable definition
      | code                      |
      | integer :: i\n            |
      | real, dimesion(:) array\n |

    Examples: expressions
      | code         |
      | a+2*b\n      |
      | l = log(x)\n |
