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
  
  Scenario: recursive line number printing
    Given a macro "line" is defined as
    """
    <%= scope.line %>
    
    """
    And a macro "test" is defined as
    """
    ! test1
    line()
    ! test2
    
    """
    When I preprocess
    """
    ! bla
    test()
    ! hello
    test()
    ! world
    test()

    """
    Then it should expand into
    """
    ! bla
    ! test1
    2
    ! test2
    ! hello
    ! test1
    4
    ! test2
    ! world
    ! test1
    6
    ! test2
    
    """
