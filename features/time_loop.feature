Feature: time loop macro

  To create many features that automatize time management we need to
  have a standard time loop. We will do this with the timeloop keyword
  and a single timeloop macro. The loop construct will always call the
  same macro (you can have only one) but it can be redefined on a per
  project basis.

  Scenario: basic time loop
    Given setting stop.time is 10
    And setting stop.step is 1000
    And setting predictable_mangle_prefix is on
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
      integer :: mangled_time_loop_step
      real :: mangled_time_loop_time
      do while (mangled_time_loop_time < 10 .and. mangled_time_loop_step < 1000)
        ! some code here
        a = 10
      end do
    end program timetest

    """
