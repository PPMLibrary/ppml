Feature: Right hand side definition

  In order to allow the user to easily define the right hand sides (rhs) of 
  the governing equations, the system should provide an extendable template
  in the form of right hand side files. The rhs can then be called in the main
  time loop or passed as function pointers to the ppm ODE module.

  rhs files are preprocessed just like the main .ppm file and macros are expanded.

  rhs files should have an extesion .rhs
  An initial template file is placed in the project's root directory


  Scenario: file format
    Given a rhs file with the following structure
    """
    rhs name(topology=t,discretization=pset1,rhs=wp,drhs=dwp,idata=idata,ldata=ldata,rdata=rdata)
    ! the body of rhs is in fortran
    ! arguments cannot be changed but erb templates are allowed
    print *,topo_id, <%= conf.ppm.dim %>
    end rhs
    """
    When the file is loaded
    Then a rhs named "name" should be created
    And the source of the rhs should be
    """
    function name(t, pset1, wp, dwp, idata, ldata, rdata)
      implicit none
      integer :: name
      integer, intent(in) :: t
      real(mk), dimension(:,:), pointer :: pset1
      real(mk), dimension(:,:), pointer :: wp
      real(mk), dimension(:,:), pointer :: dwp
      integer, dimension(:,:), intent(in), optional :: idata
      logical, dimension(:,:), intent(in), optional :: ldata
      real(mk), dimension(:,:), intent(in), optional :: rdata
      ! the body of rhs is in fortran
      ! arguments cannot be changed but erb templates are allowed
      print *,topo_id, <%= conf.ppm.dim %>
      
    end function name

    """

