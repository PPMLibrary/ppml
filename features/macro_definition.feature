Feature: macro definition

  In order to make it easy to define macros, the system should read in
  macro definitions from files. The system should have a set of
  predefined macros, but it should also be possible for a user to
  define his own, or replace an existing macro.

  Macro files should have an extesion .mac
  The system macros are in lib/macros/defs
  User files are in macros/


  Scenario: file format
    Given a file with the following structure
    """
    macro name(arg1, arg2)
    this is the body of the macro
    argument replacement <%= arg1 %>
    more replacement <%= arg2 %>
    end macro
    """
    When the file is loaded
    Then a macro named "name" should be created
    And the macro should have arguments "arg1,arg2"
    And the body of the macro should be
    """
    this is the body of the macro
    argument replacement <%= arg1 %>
    more replacement <%= arg2 %>

    """


  Scenario: multiple macros per file
    Given a file with the following structure
    """
    macro first()
    first body
    end macro
    macro second()
    second body
    end macro
    """
    When the file is loaded
    Then a macro named "first" should be created
    And the body of the macro should be
    """
    first body

    """
    And a macro named "second" should be created
    And the body of the macro should be
    """
    second body

    """

  Scenario: predefined macros
    Given the standard macro path is "examples/testdata/macros"
    When the preprocessor object is created
    Then it includes all macros from the standard path

  Scenario: user defined macros
    Given the project path is "examples/testproject/"
    When the preprocessor object is created
    Then it includes all user defined macros

  Scenario: replacing predefined macros
    Given the standard macro path is "examples/testdata/macros"
    And the project path is "examples/testproject/"
    When the preprocessor object is created
    Then it includes all user defined macros
    And predefined macros with the same names are replaced
