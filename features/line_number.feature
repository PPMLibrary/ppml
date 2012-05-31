Feature: Line number

  To print informative error messages macros should know the number of
  the line in the source file from which they were called.

  Scenario: printing line numbers
    Given a macro "line" is defined as
    """
    <%= scope.line %>
    
    """
    When I preprocess
    """
    line()
    ! hello
    line()
    ! world
    line()
    line()

    """
    Then it should expand into
    """
    1
    ! hello
    3
    ! world
    5
    6
    
    """

  Scenario: recursive macros
    Given a macro "line" is defined as
    """
    <%= scope.line %>
    
    """
    And a macro "parent" is defined as
    """
    $line()
    
    """
    When I preprocess
    """
    parent()
    a = 2
    parent()
    ! comment
    parent()

    """
    Then it should expand into
    """
    1
    a = 2
    3
    ! comment
    5
    
    """
