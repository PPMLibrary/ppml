Feature: Right hand side definition

  In order to allow the user to easily define the right hand sides (rhs) of 
  the governing equations, the system should provide an extendable template
  in the form of a right hand side statement. The rhs can then be called in
  the main time loop or passed as function pointers to the ppm ODE module.

  Background: use standard macros
    Given the standard macro path is "lib/macros/defs"

  Scenario: basic rhs definition
    Given there is a rhs call to "testrhs"
    And setting predictable_mangle_prefix is on
    And rhs call args are (:ppm_t_equi_mesh,:ppm_t_particles_d,:ppm_t_equi_mesh)
    And rhs call results are (:positions,:real,:vector,:integer)
    When I preprocess
    """
    rhs testrhs(f=>a,g,h) returns (dx,df,dg,dh)
      dgdata = get_data(dg)
    end rhs

    """
    Then the rhs module outputs
    """
    module ppm_autogenerated_rhs
      use ppm_autogenerated_global
      implicit none
      contains
      function testrhs(fields_discr, time, changes)
        use ppm_module_interfaces
        implicit none
        real(mk), dimension(:), pointer :: mangled_dgdata
        integer :: testrhs
        class(ppm_v_field_discr_pair), pointer :: fields_discr
        real(ppm_kind_double) :: time
        class(ppm_v_field), pointer :: changes
        class(ppm_t_field_discr_pair), pointer :: fd_pair
        class(ppm_t_field), pointer :: f
        class(ppm_t_equi_mesh), pointer :: a
        class(ppm_t_field), pointer :: g
        class(ppm_t_field), pointer :: h
        class(ppm_t_field), pointer :: dx
        class(ppm_t_field), pointer :: df
        class(ppm_t_field), pointer :: dg
        class(ppm_t_field), pointer :: dh
        fd_pair => fields_discr%at(1)
        f => fd_pair%field
        select type(fd_pair%discretization)
        class is (ppm_t_equi_mesh)
          a => fd_pair%discretization
        end select
        fd_pair => fields_discr%at(2)
        g => fd_pair%field
        fd_pair => fields_discr%at(3)
        h => fd_pair%field
        dx => changes%at(1)
        df => changes%at(2)
        dg => changes%at(3)
        dh => changes%at(4)
        mangled_dgdata = b%get_wp(dg)
      end function testrhs
      
    end module ppm_autogenerated_rhs

    """

    # program test
    #   ppm_init()
    #   f=create_field()
    #   c=create_particles()
    #   discretize_on(f,c)
    #   call rhs test_rhs(f)
    #   ode = init_ode(f,c,g,h)
    #   time loop
    #     do step(ode,testrhs(f,g,h=>m))
    #   end time loop
    #   ppm_finalize()
    # end program

  # Scenario: file format
  #   Given a rhs file with the following structure
  #   """
  #   rhs name(topology=t,discretization=pset1,rhs=wp,drhs=dwp,idata=idata,ldata=ldata,rdata=rdata)
  #   ! the body of rhs is in fortran
  #   ! arguments cannot be changed but erb templates are allowed
  #   print *,topo_id, <%= conf.ppm.dim %>
  #   end rhs

  #   """
  #   When the file is loaded
  #   Then a rhs named "name" should be created
  #   And the source of the rhs should be
  #   """
  #   function name(t, pset1, wp, dwp, idata, ldata, rdata)
  #     implicit none
  #     integer :: name
  #     integer, intent(in) :: t
  #     real(mk), dimension(:,:), pointer :: pset1
  #     real(mk), dimension(:,:), pointer :: wp
  #     real(mk), dimension(:,:), pointer :: dwp
  #     integer, dimension(:,:), intent(in), optional :: idata
  #     logical, dimension(:,:), intent(in), optional :: ldata
  #     real(mk), dimension(:,:), intent(in), optional :: rdata
  #     ! the body of rhs is in fortran
  #     ! arguments cannot be changed but erb templates are allowed
  #     print *,topo_id, <%= conf.ppm.dim %>
      
  #   end function name

  #   """

