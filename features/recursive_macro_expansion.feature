Feature: Recursive macro expansion

  Macro bodies should be able to call other defined macros in the
  system. This will give us a possibility to assemble smaller macros
  into larger ones and achieve higher code reduction and more easily
  maintainable code.

  To save a recursive invocation of the parser, and to save an escape
  into ruby we can add special syntax to macro bodies that triggers
  function call syntax macroexpansion.

  Scenario: macro expansion syntax
    Given a macro "fail" with argument list ("condition,msg") is defined as
    """
    if (<%= condition %>) then
      call ppm_error(errno, "<%= msg %>")
      goto 9999
    end if
    """
    And a macro "ppm_init" with argument list ("msg") is defined as
    """
    % m = msg.to_s * 2
    call ppm_init(many,args,info)
    fail("info.ne.0", <%= m %>)
    """
    When I preprocess
    """
    ppm_init("message")
    
    """
    Then it should expand into
    """
    call ppm_init(many,args,info)
    if (info.ne.0) then
      call ppm_error(errno, "messagemessage")
      goto 9999
    end if

    """
