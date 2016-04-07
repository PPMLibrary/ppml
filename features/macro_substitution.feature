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
       | name   | args | body                              | input           | result                    |
       | alloc  | a    | allocate(<%= a %>,info)           | alloc(b)\n      | allocate(b,info)\n        |
       | alloc  | a    | allocate(<%= a %>,info)           | alloc(b%data)\n | allocate(b%data,info)\n   |
       | alloc  | a,n  | allocate(<%= a %>(<%= n %>),info) | alloc(xp,10)\n  | allocate(xp(10),info)\n   |
       | fun    | v    | sin(<%= v %>)*cos(<%= v %>)       | fun(4.0)\n      | sin(4.0)*cos(4.0)\n       |
       | fun    | v    | sin(<%= v %>)*cos(<%= v %>)       | fun(4.0_mk)\n   | sin(4.0_mk)*cos(4.0_mk)\n |

    Examples: replacement with list arguments
       | name  | args | body                                   | input            | result                 |
       | alloc | a    | allocate(<%= a[0] %>,<%= a[1] %>,info) | alloc([b1,b2])\n | allocate(b1,b2,info)\n |

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

    Examples: splat arguments
       | name  | args      | body                                          | input            | result      |
       | splat | a=b,*rest | <% if rest.is_a? Array %><%= a %> = (/<%= rest.join(', ') %>/)<% end %> | splat(c,2,3,4)\n | c = (/2, 3, 4/)\n |

  Scenario: Full example of named args
    Given the standard macro path is "examples/testdata/macros"
    When I preprocess
    """
    minclude friendly("Duderino")
    fail("topoid not valid",ppm_err_argument,exit_point=8888)

    """
    Then it should expand into
    """
    PRINT*, "Hello, Mr. ","Duderino",". Can I be of any service?"
    call ppm_error(ppm_err_argument, &
      "topoid not valid",&
      caller, 100000 , info)
    goto 8888

    """

  Scenario: String arguments
    Given a macro "test_fail" with argument list ("msg") is defined as
    """
    ppm_error(<%= msg %>)

    """
    When I preprocess
    """
    test_fail("string, argument, list")

    """
    Then it should expand into
    """
    ppm_error("string, argument, list")

    """

  Scenario: Code arguments
    Given a macro "test_exec" with argument list ("e") is defined as
    """
    <%= e %>

    """
    When I preprocess
    """
    test_exec(<#PRINT*, "hello world!"#>)

    """
    Then it should expand into
    """
    PRINT*, "hello world!"

    """

  Scenario: Multiple return arguments
    Given the standard macro path is "examples/testdata/macros"
    When I preprocess
    """
    program p
      a, b = minmax(x, y)
    end program

    """
    Then it should expand into
    """
    program p
      implicit none
      real :: a
      real :: b

      a = min(x, y)
      b = max(x, y)
    end program

    """

  Scenario: Recursive macros
    Given a macro "rectest" with argument list ("msg") is defined as
    """
    ! inside rectest
    ppm_error(<%= msg %>)

    """
    Given a macro "test" with argument list ("brg") is defined as
    """
    ! inside test
    rectest(<%= brg %>)

    """
    When I preprocess
    """
    subroutine t
      ! inside t
      test("some message")
    end subroutine t

    """
    Then it should expand into
    """
    subroutine t
      implicit none
      ! inside t
      ! inside test
      ! inside rectest
      ppm_error("some message")
    end subroutine t

    """

  Scenario: Macro with recursive foreach calls
    Given a foreach macro named "particles" with argument list (particle_set)
    And body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= body.indent 2 -%>
    ENDDO

    """
    Given a macro "test" with argument list ("pset,f") is defined as
    """
    ! inside test
    foreach p in particles(<%= pset %>)
      f_p = 0.0_mk
    end foreach

    """
    When I preprocess
    """
    subroutine t
      ! inside t
      test(parts,velocity)
    end subroutine t

    """
    Then it should expand into
    """
    subroutine t
      implicit none
      ! inside t
      ! inside test
      do p=1,parts%Npart
        f_p = 0.0_mk
      ENDDO
    end subroutine t

    """
