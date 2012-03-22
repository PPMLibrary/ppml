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
      | code                                                        |
      | program sample\nimplicit none\nend program\n                |
      | subroutine name(args)\nimplicit none\nend subroutine name\n |
      | module name\nimplicit none\nend module name\n               |

    Examples: variable definition
      | code                      |
      | integer :: i\n            |
      | real, dimesion(:) array\n |
      | type(ptr_t) :: p\n        |

    Examples: expressions
      | code         |
      | a+2*b\n      |
      | l = log(x)\n |

    Examples: loops
      | code                        |
      | do i=1,N\nj = i+1\nend do\n |

    Examples: continued line
      | code                            |
      | i=10 &\n  +2\n                  |
      | j=30 &\n & +i\n                 |
