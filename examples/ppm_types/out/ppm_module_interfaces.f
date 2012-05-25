MODULE ppm_module_interfaces

!----------------------------------------------------------------------
!  Modules
!----------------------------------------------------------------------
USE ppm_module_container_typedef
USE ppm_module_data
USE ppm_module_util_functions

IMPLICIT NONE

!----------------------------------------------------------------------
! Global parameters
!----------------------------------------------------------------------
!PPM internal parameters used only to access entries in the
!mesh data structures.
INTEGER,PARAMETER,PUBLIC   :: ppm_mdata_ghosts = 1
INTEGER,PARAMETER,PUBLIC   :: ppm_mdata_reqput = 2
INTEGER,PARAMETER,PUBLIC   :: ppm_mdata_cartesian = 3
INTEGER,PARAMETER,PUBLIC   :: ppm_mdata_lflags = 3

!----------------------------------------------------------------------
! Module variables 
!----------------------------------------------------------------------
INTEGER, PRIVATE, DIMENSION(3)  :: lda

!----------------------------------------------------------------------
! Type declaration
!----------------------------------------------------------------------



TYPE ABSTRACT ppm_t_object

END TYPE ppm_t_object


TYPE,EXTENDS(ppm_t_object) :: ppm_t_subpatch_
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
    PROCEDURE(subpatch_create_), DEFERRED  :: create
    PROCEDURE(subpatch_destroy_),DEFERRED  :: destroy
    PROCEDURE(subpatch_get_pos2d_),DEFERRED  :: get_pos2d
    PROCEDURE(subpatch_get_pos3d_),DEFERRED  :: get_pos3d
    GENERIC :: get_pos => get_pos2d, get_pos3d

END TYPE

INTERFACE

!CREATE
SUBROUTINE subpatch_create_(p,mesh,istart,iend,istart_p,iend_p,ghostsize,info)
    !!! Constructor for subpatch
    IMPORT ppm_t_subpatch_,ppm_kind_double,ppm_t_equi_mesh_
    CLASS(ppm_t_subpatch_)             :: p
    CLASS(ppm_t_equi_mesh_),TARGET     :: mesh
    INTEGER,DIMENSION(:)               :: istart
    INTEGER,DIMENSION(:)               :: iend
    INTEGER,DIMENSION(:)               :: istart_p
    INTEGER,DIMENSION(:)               :: iend_p
    INTEGER,DIMENSION(:)               :: ghostsize
    INTEGER,               INTENT(OUT) :: info
END SUBROUTINE

!DESTROY
SUBROUTINE subpatch_destroy_(p,info)
    !!! Destructor for subpatch
    IMPORT ppm_t_subpatch_
    CLASS(ppm_t_subpatch_)              :: p
    INTEGER,               INTENT(OUT) :: info
END SUBROUTINE

!GET POSITION
PURE FUNCTION subpatch_get_pos2d_(p,i,j) RESULT (pos)
    IMPORT ppm_t_subpatch_,ppm_kind_double,ppm_dim
    CLASS(ppm_t_subpatch_), INTENT(IN) :: p
    INTEGER,                INTENT(IN) :: i
    INTEGER,                INTENT(IN) :: j
    REAL(ppm_kind_double),DIMENSION(ppm_dim) :: pos
END FUNCTION
PURE FUNCTION subpatch_get_pos3d_(p,i,j,k) RESULT (pos)
    IMPORT ppm_t_subpatch_,ppm_kind_double,ppm_dim
    CLASS(ppm_t_subpatch_), INTENT(IN) :: p
    INTEGER,                INTENT(IN) :: i
    INTEGER,                INTENT(IN) :: j
    INTEGER,                INTENT(IN) :: k
    REAL(ppm_kind_double),DIMENSION(ppm_dim) :: pos
END FUNCTION

END INTERFACE

END MODULE ppm_module_interfaces
