MODULE ppm_module_mesh_typedef
!----------------------------------------------------------------------
!  Modules
!----------------------------------------------------------------------
USE ppm_module_substart
USE ppm_module_substop
USE ppm_module_data
USE ppm_module_alloc
USE ppm_module_error
USE ppm_module_util_functions
USE ppm_module_interfaces

IMPLICIT NONE

!----------------------------------------------------------------------
! Internal parameters
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Module variables 
!----------------------------------------------------------------------
INTEGER, PRIVATE, DIMENSION(4)  :: ldc

!----------------------------------------------------------------------
! Type declaration
!----------------------------------------------------------------------

TYPE,EXTENDS(ppm_t_subpatch_) :: ppm_t_subpatch
    INTEGER                       :: meshID = 0
    CLASS(ppm_t_discr_kind),POINTER:: mesh => NULL()
    INTEGER, DIMENSION(:),POINTER :: istart   => NULL()
    INTEGER, DIMENSION(:),POINTER :: iend     => NULL()
    INTEGER, DIMENSION(:),POINTER :: nnodes   => NULL()
    INTEGER, DIMENSION(:),POINTER :: istart_p => NULL()
    INTEGER, DIMENSION(:),POINTER :: iend_p   => NULL()
    CLASS(ppm_c_subpatch_data_),POINTER :: subpatch_data => NULL()
    !!! container for the data arrays for each property discretized
    !!! on this mesh
    CONTAINS
    PROCEDURE  :: create    => subpatch_create
    PROCEDURE  :: destroy   => subpatch_destroy
    PROCEDURE  :: get_pos2d   => subpatch_get_pos2d
    PROCEDURE  :: get_pos3d   => subpatch_get_pos3d
    GENERIC :: get_pos => get_pos2d, get_pos3d

END TYPE

SUBROUTINE subpatch_create(p,mesh,istart,iend,istart_p,iend_p,info)
    !!! Constructor for subpatch data structure
    CLASS(ppm_t_subpatch)              :: p
    CLASS(ppm_t_equi_mesh_),TARGET     :: mesh
    INTEGER,DIMENSION(:)               :: istart
    INTEGER,DIMENSION(:)               :: iend
    INTEGER,DIMENSION(:)               :: istart_p
    INTEGER,DIMENSION(:)               :: iend_p
    INTEGER,               INTENT(OUT) :: info
    !!! return status. On success 0
    INTEGER                            :: iopt

    start_subroutine("subpatch_create")

    p%meshID = mesh%ID
    p%mesh   => mesh
    p%istart = istart
    p%iend   = iend
    p%istart_p = istart_p
    p%iend_p   = iend_p

    end_subroutine()
END SUBROUTINE subpatch_create

!DESTROY
SUBROUTINE subpatch_destroy(p,info)
    !!! Destructor for subpatch data structure
    CLASS(ppm_t_subpatch)              :: p
    INTEGER,               INTENT(OUT) :: info

    INTEGER                            :: iopt

    start_subroutine("subpatch_destroy")

    p%meshID = 0
    p%mesh => NULL()

    end_subroutine()
END SUBROUTINE subpatch_destroy


!GET POSITION
PURE FUNCTION subpatch_get_pos2d(p,i,j) RESULT (pos)
    !!! Return position of mesh node i,j,k
    CLASS(ppm_t_subpatch),  INTENT(IN) :: p
    INTEGER,                INTENT(IN) :: i
    INTEGER,                INTENT(IN) :: j
    REAL(ppm_kind_double),DIMENSION(ppm_dim) :: pos

    SELECT TYPE(mesh => p%mesh)
    TYPE IS (ppm_t_equi_mesh)
        !numbering of mesh nodes goes from 1 to N
        pos(1) = (p%istart(1)+i-2)*mesh%h(1) + mesh%offset(1)
        pos(2) = (p%istart(2)+j-2)*mesh%h(2) + mesh%offset(2)
    END SELECT

END FUNCTION subpatch_get_pos2d

PURE FUNCTION subpatch_get_pos3d(p,i,j,k) RESULT (pos)
    !!! Return position of mesh node i,j,k
    CLASS(ppm_t_subpatch),  INTENT(IN) :: p
    INTEGER,                INTENT(IN) :: i
    INTEGER,                INTENT(IN) :: j
    INTEGER,                INTENT(IN) :: k
    REAL(ppm_kind_double),DIMENSION(ppm_dim) :: pos

    SELECT TYPE(mesh => p%mesh)
    TYPE IS (ppm_t_equi_mesh)
        !numbering of mesh nodes goes from 1 to N
        pos(1) = (p%istart(1)+i-2)*mesh%h(1) + mesh%offset(1)
        pos(2) = (p%istart(2)+j-2)*mesh%h(2) + mesh%offset(2)
        pos(3) = (p%istart(3)+k-2)*mesh%h(3) + mesh%offset(3)
    END SELECT

END FUNCTION subpatch_get_pos3d


END MODULE ppm_module_mesh_typedef
