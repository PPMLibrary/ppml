Feature: time loop macro

  To create many features that automatize time management we need to
  have a standard time loop. We will do this with the timeloop keyword
  and a single timeloop macro. The loop construct will always call the
  same macro (you can have only one) but it can be redefined on a per
  project basis.

  Scenario: basic time loop
    Given setting stop.time is 10
    And setting stop.step is 1000
    When I preprocess
    """
    program timetest
      timeloop
        ! some code here
        a = 10
      end timeloop
    end program timetest

    """
    Then it should expand into
    """
    program timetest
      implicit none
      integer :: time_loop_step
      real :: time_loop_time
      while (time_loop_time < 10 .and. time_loop_step < 1000)
        ! some code here
        a = 10
      end while
    end program timetest

    """

