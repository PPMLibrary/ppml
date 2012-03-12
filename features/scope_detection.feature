Feature: Scope Detection

  To enable insertion of new variables, initialization and cleanup
  code, and automatic interface definitions we need to detect target
  locations and make them available in the calling macro code.

  Background: running in comment mode
    Comment mode makes the scope object print a comment line in all
    target locations.
    # Given comment mode is turned on

  Scenario: program statement
    When I preprocess
    """
    program sample
    end program

    """
    Then it should expand into
    """
    program sample
    end program

    """
