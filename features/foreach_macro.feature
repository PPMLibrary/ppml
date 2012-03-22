Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "particles"
    And argument list (P) and body
    """
    do <%= iterator %>=1,<%= P %>%Npart
      <%= b %>
    end do

    """
    When I preprocess
    """
    foreach p in particles(P)
      p = 10
    end foreach

    """
    Then it should expand into
    """
    do i=1,P%Npart
      ! do something to p
    end do

    """
