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

    Examples: one argument
       | name  | args      | body                                      | input           | result                    |
       | alloc | a         | allocate(<%= a %>,info)                   | alloc(b)\n      | allocate(b,info)\n        |
       | fail  | t=default | call ppm_error(<%= t %>)                  | fail()\n        | call ppm_error(default)\n |
       | fail  | t=default | call ppm_error(<%= t %>)                  | fail(special)\n | call ppm_error(special)\n |
       | check | t=nil     | <% if t %>hello<% else %>goodbye<% end %> | check()\n       | goodbye\n                 |
       | check | t=nil     | <% if t %>hello<% else %>goodbye<% end %> | check(a)\n      | hello\n                   |
       | log2  | n         | this will never expand                    | hello(a)\n      | hello(a)\n                |
       | log2  | n         | this will never expand                    | log(4)\n        | log(4)\n                  |
