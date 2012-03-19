Feature: macro substitution

  In order to be able to invoke a macro we have to provide a syntax
  that can be recognized in standard fortran files and it should be
  simple and fortran-like so that it apeals to users.
  A user should be able to:
   - define a macro and give it a name
   - define the replacement body
   - type macro syntax mixed with standard fortran
   - have the preprocessor expand the body

  Scenario Outline: function call syntax with no arguments
    Given a macro "<name>" is defined as "<body>"
    When I preprocess "<input>"
    Then it should expand into "<result>"

    Examples: simple replacement
      | name      | body                   | input           | result               |
      | example   | call example(args)     | example()\n     | call example(args)\n |
      | commented | this will never expand | ! commented()\n | ! commented()\n      |

  Scenario Outline: function call syntax with argument substitution
    Given a macro "<name>" with argument list ("<args>") is defined as "<body>"
    When I preprocess "<input>"
    Then it should expand into "<result>"

    Examples: simple replacement
       | name   | args | body                    | input                        | result                        |
       | alloc  | a    | allocate(<%= a %>,info) | alloc(b)\n                   | allocate(b,info)\n            |
       | alloc  | a    | allocate(<%= a %>,info) | alloc(b%data)\n              | allocate(b%data,info)\n       |

    Examples: default value
       | name  | args      | body                                      | input           | result                    |
       | fail  | t=default | call ppm_error(<%= t %>)                  | fail()\n        | call ppm_error(default)\n |
       | fail  | t=default | call ppm_error(<%= t %>)                  | fail(special)\n | call ppm_error(special)\n |

    Examples: presence test
       | name  | args      | body                                      | input           | result                    |
       | check | t=nil     | <% if t %>hello<% else %>goodbye<% end %> | check()\n       | goodbye\n                 |
       | check | t=nil     | <% if t %>hello<% else %>goodbye<% end %> | check(a)\n      | hello\n                   |

    Examples: fortran functions pass unchanged
       | name  | args      | body                                      | input           | result                    |
       | log2  | n         | this will never expand                    | hello(a)\n      | hello(a)\n                |
       | log2  | n         | this will never expand                    | log(4)\n        | log(4)\n                  |

    Examples: named arguments
       | name  | args    | body              | input        | result |
       | named | a=1,b=2 | <%= a %>,<%= b %> | named(b=3)\n | 1,3\n  |

  Scenario: Full example of named args
    Given the standard macro path is "examples/testdata/macros"
    When I preprocess
    """
    fail("topoid not valid",ppm_err_argument,exit_point=8888)

    """
    Then it should expand into
    """
    call ppm_error(ppm_err_argument, &
      "topoid not valid",&
      caller, 100000 , info)
    goto 8888

    """

  Scenario: String arguments
    Given a macro "fail" with argument list ("msg") is defined as
    """
    ppm_error(<%= msg %>)

    """
    When I preprocess
    """
    fail("string, argument, list")

    """
    Then it should expand into
    """
    ppm_error("string, argument, list")
    
    """


