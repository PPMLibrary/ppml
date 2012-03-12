# Feature: Foreach Macros

#   In order to abstract away different looping strategies we need to
#   provide a looping macro.

#   Scenario: basic foreach macro
#     Given a foreach macro named "particles" with collection "P" and body
#     """
#     do i=1,<%= P %>%Npart
#       <%= b %>
#     enddo
#     """
#     When I preprocess
#     """
#     foreach p in particles(P)
#       ! do something to p
#     end

#     """
#     Then it should expand into
#     """
#     do i=1,P%Npart
#       ! do something to p
#     enddo

#     """
