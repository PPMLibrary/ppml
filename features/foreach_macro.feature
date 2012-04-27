Feature: Foreach Macros

  In order to abstract away different looping strategies we need to
  provide a looping macro.

  Scenario: basic foreach macro
    Given a foreach macro named "particles"
    And argument list (particle_set) and body
    """
    do <%= iter %>=1,<%= particle_set %>%Npart
    <%= indent(body,2) -%>
    end do

    """
    When I preprocess
    """
      ! leading comment
      foreach p in particles(P)
        ! do something to p
        p = 20
      end foreach

    """
    Then it should expand into
    """
      ! leading comment
      do p=1,P%Npart
        ! do something to p
        p = 20
      end do

    """
