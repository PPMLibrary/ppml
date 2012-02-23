

!----*- f90 -*-------------------------------------------------------------
!  Module       :                    ppm_module_ctrl
!--------------------------------------------------------------------------
!
!  Purpose      : Easy definition and parsing of command line and
!                 control file arguments.
!
!  Remarks      :
!
!  References   :
!
!  Revisions    :
!--------------------------------------------------------------------------
!
!--------------------------------------------------------------------------
!  Milan Mitrovic
!  ETH Zurich
!  CH-8092 Zurich, Switzerland
!--------------------------------------------------------------------------
MODULE ppm_module_ctrl
!!! This module provides an easy way to handle command line
!!! arguments and control files.
!!!
!!! .Intended Usage
!!!
!!! This module assumes that you will create a module that contains
!!! all your global variables (which we will call +client_global+) and
!!! gives you an easy way of supplying values for these variables,
!!! either through command line arguments or control files.
!!! 
!!! To do this, you have to create a subroutine (which we will call
!!! +define_args+) that will hold the initialization code. See the
!!! following example.
!!!
!!! [source,fortran]
!!! ----
!!! MODULE client_global
!!!   USE ppm_module_ctrl
!!!   INTEGER :: example
!!! CONTAINS
!!!   SUBROUTINE define_args
!!!      CALL arg(exampe, 'example',     &
!!!               flag      = '-e',      &
!!!               ctrl_name = 'example')
!!!   END SUBROUTINE define_args
!!! END MODULE client_global
!!! ----
!!! 
!!! This will create an argument of type +integer+ that can be set
!!! either through the command line flag _'-e'_ or the control file
!!! variable _'example'_. Please take note that the command line flag
!!! will override the setting in the control file if both are
!!! supplied.
!!!
!!! For this code to work you need to +call define_args+ somewhere in
!!! your initialization code, followed by a +call parse_args(info)+. 
!!! After +parse_args+ completes your globals will be initialized
!!! with the supplied values.
!!!
!!! The function +arg+ is overloaded to support +integer+, +real+,
!!! +character+, +logical+ and +complex+ arguments or fixed sized
!!! arrays of these types. The first two arguments are required and
!!! they are: the variable itself (to which a pointer will be
!!! stored) and the name of the variable (used for printing help
!!! messages).
!!!
!!! All other arguments are optional and the details of how they work
!!! can be found below. Here is just a short overview and reference
!!! of the available options:
!!! [horizontal]
!!! flag :: [_'-f'_] single character command line flag
!!! long_flag :: [_'--long-flag'_] long command line flag (starts with
!!! --)
!!! ctrl_name :: [_'name'_] name of the control file varible
!!! default :: [_42_] default value
!!! min :: [_4_] minimum value for numeric types
!!! max :: [_30_] maximum value
!!! help :: ['Description.'] help message to display in the auto
!!! generated ctrl file and command line help. You can use +\n+ to
!!! force a line break.
!!! default_func :: [_external_func_] custom function to compute the
!!! value of the variable after other globals have been set (only
!!! available when compiled with F2003 support)
!!! validator :: [_external_func_] custom function to validate the
!!! variable value (only available when compiled with F2003 support)
!!!
!!! By default the module supports _-h_ and _--help_ flags for
!!! printing the help message, and _--print-ctrl_ for printing a
!!! sample control file. There is also an optional first positional
!!! argument that is interpreted as the name of the control file.
!!!
!!! To make the output prettier you can add calls to
!!! +arg_group(_'name'_)+ to your +define_args+ and all calls to
!!! +arg+ following a group definition will be put into that group.
!!!
!!! If any one of the printing flags is present +parse_args+ will return
!!! _exit_gracefully_ which you should check for and exit gracefully.
!!!

  !------------------------------------------------------------------------
  !  Modules
  !------------------------------------------------------------------------
  USE ppm_module_data,    ONLY: ppm_rank, ppm_comm
  USE ppm_module_typedef
  USE ppm_module_substart
  USE ppm_module_substop
  USE ppm_module_error
  IMPLICIT NONE


  !------------------------------------------------------------------------
  !  Interface
  !------------------------------------------------------------------------
  PUBLIC :: arg, arg_group, parse_args, disable_help, disable_ctrl, &
       &    set_ctrl_name,                                          &

       &    integer_func, longint_func, single_func, double_func,   & 
       &    logical_func, string_func, complex_func, dcomplex_func, & 
       &    integer_array_func, longint_array_func,                 &
       &    single_array_func, double_array_func,                   &
       &    logical_array_func, string_array_func,                  &
       &    complex_array_func, dcomplex_array_func,                &

       &    reset, add_cmd, ctrl_file_test, break_help,             &
       &    find_arg, find_flag, arg_count,                         &
       &    enabling_flag, disabling_flag, exit_gracefully

  PRIVATE




  !------------------------------------------------------------------------
  !  Types
  !------------------------------------------------------------------------
  ! scalar


  TYPE INTEGER_arg


     INTEGER,                                    POINTER :: variable => NULL()
     INTEGER                                             :: default


     INTEGER                                             :: min
     INTEGER                                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(INTEGER_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(INTEGER_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE INTEGER_arg


  TYPE LONGINT_arg


     INTEGER(ppm_kind_int64),                    POINTER :: variable => NULL()
     INTEGER(ppm_kind_int64)                             :: default


     INTEGER(ppm_kind_int64)                             :: min
     INTEGER(ppm_kind_int64)                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(LONGINT_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(LONGINT_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE LONGINT_arg


  TYPE SINGLE_arg


     REAL(ppm_kind_single),                      POINTER :: variable => NULL()
     REAL(ppm_kind_single)                               :: default


     REAL(ppm_kind_single)                               :: min
     REAL(ppm_kind_single)                               :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(SINGLE_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(SINGLE_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE SINGLE_arg


  TYPE DOUBLE_arg


     REAL(ppm_kind_double),                      POINTER :: variable => NULL()
     REAL(ppm_kind_double)                               :: default


     REAL(ppm_kind_double)                               :: min
     REAL(ppm_kind_double)                               :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(DOUBLE_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(DOUBLE_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE DOUBLE_arg


  TYPE LOGICAL_arg


     LOGICAL,                                    POINTER :: variable => NULL()
     LOGICAL                                             :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: type          = .TRUE. ! enable
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(LOGICAL_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(LOGICAL_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE LOGICAL_arg


  TYPE STRING_arg


     CHARACTER(LEN=ppm_char),                    POINTER :: variable => NULL()
     CHARACTER(LEN=ppm_char)                             :: default


!      INTEGER                                             :: min
!      INTEGER                                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(STRING_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(STRING_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE STRING_arg


  TYPE COMPLEX_arg


     COMPLEX(ppm_kind_single),                   POINTER :: variable => NULL()
     COMPLEX(ppm_kind_single)                            :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(COMPLEX_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(COMPLEX_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE COMPLEX_arg


  TYPE DCOMPLEX_arg


     COMPLEX(ppm_kind_double),                   POINTER :: variable => NULL()
     COMPLEX(ppm_kind_double)                            :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(DCOMPLEX_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(DCOMPLEX_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE DCOMPLEX_arg

  ! array


  TYPE INTEGER_array_arg


     INTEGER,                  DIMENSION(:),     POINTER :: variable => NULL()
     INTEGER,                  DIMENSION(:), ALLOCATABLE :: default


     INTEGER                                             :: min
     INTEGER                                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(INTEGER_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(INTEGER_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE INTEGER_array_arg


  TYPE LONGINT_array_arg


     INTEGER(ppm_kind_int64),  DIMENSION(:),     POINTER :: variable => NULL()
     INTEGER(ppm_kind_int64),  DIMENSION(:), ALLOCATABLE :: default


     INTEGER(ppm_kind_int64)                             :: min
     INTEGER(ppm_kind_int64)                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(LONGINT_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(LONGINT_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE LONGINT_array_arg


  TYPE SINGLE_array_arg


     REAL(ppm_kind_single),    DIMENSION(:),     POINTER :: variable => NULL()
     REAL(ppm_kind_single),    DIMENSION(:), ALLOCATABLE :: default


     REAL(ppm_kind_single)                               :: min
     REAL(ppm_kind_single)                               :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(SINGLE_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(SINGLE_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE SINGLE_array_arg


  TYPE DOUBLE_array_arg


     REAL(ppm_kind_double),    DIMENSION(:),     POINTER :: variable => NULL()
     REAL(ppm_kind_double),    DIMENSION(:), ALLOCATABLE :: default


     REAL(ppm_kind_double)                               :: min
     REAL(ppm_kind_double)                               :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(DOUBLE_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(DOUBLE_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE DOUBLE_array_arg


  TYPE LOGICAL_array_arg


     LOGICAL,                  DIMENSION(:),     POINTER :: variable => NULL()
     LOGICAL,                  DIMENSION(:), ALLOCATABLE :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(LOGICAL_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(LOGICAL_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE LOGICAL_array_arg


  TYPE STRING_array_arg


     CHARACTER(LEN=ppm_char),  DIMENSION(:),     POINTER :: variable => NULL()
     CHARACTER(LEN=ppm_char),  DIMENSION(:), ALLOCATABLE :: default


!      INTEGER                                             :: min
!      INTEGER                                             :: max
     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(STRING_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(STRING_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE STRING_array_arg


  TYPE COMPLEX_array_arg


     COMPLEX(ppm_kind_single), DIMENSION(:),     POINTER :: variable => NULL()
     COMPLEX(ppm_kind_single), DIMENSION(:), ALLOCATABLE :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(COMPLEX_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(COMPLEX_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE COMPLEX_array_arg


  TYPE DCOMPLEX_array_arg


     COMPLEX(ppm_kind_double), DIMENSION(:),     POINTER :: variable => NULL()
     COMPLEX(ppm_kind_double), DIMENSION(:), ALLOCATABLE :: default


     LOGICAL                                             :: min_set       = .FALSE.
     LOGICAL                                             :: max_set       = .FALSE.
     LOGICAL                                             :: default_set   = .FALSE.
     CHARACTER(LEN=256)                                  :: name
     CHARACTER(LEN=2)                                    :: flag
     LOGICAL                                             :: flag_set      = .FALSE.
     CHARACTER(LEN=256)                                  :: long_flag
     LOGICAL                                             :: long_flag_set = .FALSE.
     CHARACTER(LEN=256)                                  :: ctrl_name
     LOGICAL                                             :: ctrl_name_set = .FALSE.
     LOGICAL                                             :: settable      = .FALSE.
     CHARACTER(LEN=ppm_char)                             :: help
     LOGICAL                                             :: help_set      = .FALSE.
     LOGICAL                                             :: clf_supplied  = .FALSE.
     INTEGER                                             :: group
     INTEGER                                             :: group_i
     LOGICAL                                             :: default_func_set = .FALSE.
     LOGICAL                                             :: validator_set = .FALSE.
     PROCEDURE(DCOMPLEX_array_func), POINTER, NOPASS :: default_func => NULL()
     PROCEDURE(DCOMPLEX_array_func), POINTER, NOPASS :: validator    => NULL()
  END TYPE DCOMPLEX_array_arg


  !------------------------------------------------------------------------
  !  Interfaces
  !------------------------------------------------------------------------
  INTERFACE arg
     ! scalar
     MODULE PROCEDURE INTEGER_add_arg
     MODULE PROCEDURE LONGINT_add_arg
     MODULE PROCEDURE SINGLE_add_arg
     MODULE PROCEDURE DOUBLE_add_arg
     MODULE PROCEDURE LOGICAL_add_arg
     MODULE PROCEDURE STRING_add_arg
     MODULE PROCEDURE COMPLEX_add_arg
     MODULE PROCEDURE DCOMPLEX_add_arg
     ! array
     MODULE PROCEDURE INTEGER_array_add_arg
     MODULE PROCEDURE LONGINT_array_add_arg
     MODULE PROCEDURE SINGLE_array_add_arg
     MODULE PROCEDURE DOUBLE_array_add_arg
     MODULE PROCEDURE LOGICAL_array_add_arg
     MODULE PROCEDURE STRING_array_add_arg
     MODULE PROCEDURE COMPLEX_array_add_arg
     MODULE PROCEDURE DCOMPLEX_array_add_arg
  END INTERFACE

  ABSTRACT INTERFACE
     !---------------------------------------------------------------------
     !  Defaults and Validators
     !---------------------------------------------------------------------
     ! scalar
     LOGICAL FUNCTION INTEGER_func(variable)
       INTEGER, POINTER :: variable
     END FUNCTION INTEGER_func

     LOGICAL FUNCTION LONGINT_func(variable)
       INTEGER(8), POINTER :: variable
     END FUNCTION LONGINT_func

     LOGICAL FUNCTION SINGLE_func(variable)
       REAL(KIND(1.0E0)), POINTER :: variable
     END FUNCTION SINGLE_func

     LOGICAL FUNCTION DOUBLE_func(variable)
       REAL(KIND(1.0D0)), POINTER :: variable
     END FUNCTION DOUBLE_func

     LOGICAL FUNCTION LOGICAL_func(variable)
       LOGICAL, POINTER :: variable
     END FUNCTION LOGICAL_func

     LOGICAL FUNCTION STRING_func(variable)
       CHARACTER(LEN=*), POINTER :: variable
     END FUNCTION STRING_func

     LOGICAL FUNCTION COMPLEX_func(variable)
       COMPLEX(KIND(1.0E0)), POINTER :: variable
     END FUNCTION COMPLEX_func

     LOGICAL FUNCTION DCOMPLEX_func(variable)
       COMPLEX(KIND(1.0D0)), POINTER :: variable
     END FUNCTION DCOMPLEX_func

     ! array
     LOGICAL FUNCTION INTEGER_array_func(variable)
       INTEGER, DIMENSION(:), POINTER :: variable
     END FUNCTION INTEGER_array_func

     LOGICAL FUNCTION LONGINT_array_func(variable)
       INTEGER(8), DIMENSION(:), POINTER :: variable
     END FUNCTION LONGINT_array_func

     LOGICAL FUNCTION SINGLE_array_func(variable)
       REAL(KIND(1.0E0)), DIMENSION(:), POINTER :: variable
     END FUNCTION SINGLE_array_func

     LOGICAL FUNCTION DOUBLE_array_func(variable)
       REAL(KIND(1.0D0)), DIMENSION(:), POINTER :: variable
     END FUNCTION DOUBLE_array_func

     LOGICAL FUNCTION LOGICAL_array_func(variable)
       LOGICAL, DIMENSION(:), POINTER :: variable
     END FUNCTION LOGICAL_array_func

     LOGICAL FUNCTION STRING_array_func(variable)
       CHARACTER(LEN=*), DIMENSION(:), POINTER :: variable
     END FUNCTION STRING_array_func

     LOGICAL FUNCTION COMPLEX_array_func(variable)
       COMPLEX(KIND(1.0E0)), DIMENSION(:), POINTER :: variable
     END FUNCTION COMPLEX_array_func

     LOGICAL FUNCTION DCOMPLEX_array_func(variable)
       COMPLEX(KIND(1.0D0)), DIMENSION(:), POINTER :: variable
     END FUNCTION DCOMPLEX_array_func

  END INTERFACE
  !------------------------------------------------------------------------
  !  Constants
  !------------------------------------------------------------------------
  LOGICAL, PARAMETER       :: enabling_flag   = .true.
!!! Value for type option of logical args. Presence of flag sets
!!! variable to +.TRUE.+
  LOGICAL, PARAMETER       :: disabling_flag  = .false.
!!! Value for type option of logical args. Presence of flag sets
!!! variable to +.FALSE.+
  INTEGER, PARAMETER       :: exit_gracefully = 42
!!! Value of the +info+ argument of +parse_args+ that signals that
!!! the program should exit without error.
  !------------------------------------------------------------------------
  !  Variables
  !------------------------------------------------------------------------
  ! by how much to grow storage
  INTEGER,                           PARAMETER    :: di = 10
  ! scalar
  TYPE(INTEGER_arg),        POINTER, DIMENSION(:) :: INTEGER_args    => NULL()
  INTEGER                                         :: INTEGER_args_i  = 0
  TYPE(LONGINT_arg),        POINTER, DIMENSION(:) :: LONGINT_args    => NULL()
  INTEGER                                         :: LONGINT_args_i  = 0
  TYPE(SINGLE_arg),         POINTER, DIMENSION(:) :: SINGLE_args     => NULL()
  INTEGER                                         :: SINGLE_args_i   = 0
  TYPE(DOUBLE_arg),         POINTER, DIMENSION(:) :: DOUBLE_args     => NULL()
  INTEGER                                         :: DOUBLE_args_i   = 0
  TYPE(LOGICAL_arg),        POINTER, DIMENSION(:) :: LOGICAL_args    => NULL()
  INTEGER                                         :: LOGICAL_args_i  = 0
  TYPE(STRING_arg),         POINTER, DIMENSION(:) :: STRING_args     => NULL()
  INTEGER                                         :: STRING_args_i   = 0
  TYPE(COMPLEX_arg),        POINTER, DIMENSION(:) :: COMPLEX_args    => NULL()
  INTEGER                                         :: COMPLEX_args_i  = 0
  TYPE(DCOMPLEX_arg),       POINTER, DIMENSION(:) :: DCOMPLEX_args   => NULL()
  INTEGER                                         :: DCOMPLEX_args_i = 0
  ! add arrays
  TYPE(INTEGER_array_arg),  POINTER, DIMENSION(:) :: INTEGER_array_args    => NULL()
  INTEGER                                         :: INTEGER_array_args_i  = 0
  TYPE(LONGINT_array_arg),  POINTER, DIMENSION(:) :: LONGINT_array_args    => NULL()
  INTEGER                                         :: LONGINT_array_args_i  = 0
  TYPE(SINGLE_array_arg),   POINTER, DIMENSION(:) :: SINGLE_array_args     => NULL()
  INTEGER                                         :: SINGLE_array_args_i   = 0
  TYPE(DOUBLE_array_arg),   POINTER, DIMENSION(:) :: DOUBLE_array_args     => NULL()
  INTEGER                                         :: DOUBLE_array_args_i   = 0
  TYPE(LOGICAL_array_arg),  POINTER, DIMENSION(:) :: LOGICAL_array_args    => NULL()
  INTEGER                                         :: LOGICAL_array_args_i  = 0
  TYPE(STRING_array_arg),   POINTER, DIMENSION(:) :: STRING_array_args     => NULL()
  INTEGER                                         :: STRING_array_args_i   = 0
  TYPE(COMPLEX_array_arg),  POINTER, DIMENSION(:) :: COMPLEX_array_args    => NULL()
  INTEGER                                         :: COMPLEX_array_args_i  = 0
  TYPE(DCOMPLEX_array_arg), POINTER, DIMENSION(:) :: DCOMPLEX_array_args   => NULL()
  INTEGER                                         :: DCOMPLEX_array_args_i = 0
  ! arg storage
  CHARACTER(LEN=ppm_char),  POINTER, DIMENSION(:) :: cmd_args       => NULL()
  INTEGER,                  POINTER, DIMENSION(:) :: cmd_args_len   => NULL()
  LOGICAL,                  POINTER, DIMENSION(:) :: cmd_args_used  => NULL()
  INTEGER                                         :: cmd_args_i     =  0
  INTEGER                                         :: cmd_i          =  0
  ! arg groups
  CHARACTER(LEN=ppm_char),  POINTER, DIMENSION(:) :: groups         => NULL()
  INTEGER,                  POINTER, DIMENSION(:) :: group_size     => NULL()
  INTEGER,                  POINTER, DIMENSION(:) :: group_max_len  => NULL()
  LOGICAL,                  POINTER, DIMENSION(:) :: group_has_ctrl => NULL()
  LOGICAL,                  POINTER, DIMENSION(:) :: group_has_arg  => NULL()
  INTEGER                                         :: groups_i       =  -1
  ! special args
  LOGICAL                                         :: help_enabled   = .TRUE.
  LOGICAL                                         :: ctrl_enabled   = .TRUE.
  CHARACTER(LEN=ppm_char)                         :: ctrl_file_name = 'Ctrl'
  CHARACTER(LEN=ppm_char)                         :: ctrl_file_test = ''
  ! test run
  LOGICAL                                         :: in_test        = .FALSE.

CONTAINS
  !------------------------------------------------------------------------
  !  Master procedure
  !------------------------------------------------------------------------
  SUBROUTINE parse_args(info)
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------
    INTEGER, INTENT(  OUT)                  :: info
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    REAL(8)                                 :: t0
    CHARACTER(LEN=*), PARAMETER             :: caller='parse_args'
    CHARACTER(LEN=256)                      :: value
    LOGICAL                                 :: ok
    LOGICAL                                 :: printing_ctrl = .FALSE.
    INTEGER                                 :: info2
    INTEGER                                 :: i
    INTEGER                                 :: rank = 0
    !----------------------------------------------------------------------
    !  Externals
    !----------------------------------------------------------------------
    EXTERNAL iargc
    INTEGER  iargc
    !----------------------------------------------------------------------
    !  Initialize
    !----------------------------------------------------------------------
    CALL substart(caller, t0, info)
    !----------------------------------------------------------------------
    !  Do everything on rank 0 and bcast at the end
    !----------------------------------------------------------------------
    IF (rank .EQ. 0) THEN
       !-------------------------------------------------------------------
       !  Copy default values into variables
       !-------------------------------------------------------------------
       CALL apply_defaults(info)
       IF (info .NE. 0) THEN
          info = ppm_error_fatal
          CALL ppm_error(ppm_err_argument, caller, &
               'Applying defaults failed!', 419, info)
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  Read in the command line
       !-------------------------------------------------------------------
       IF (.NOT. in_test) THEN
          CALL read_cmd_args(info)
          IF (info .NE. 0) THEN
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_alloc, caller, &
                  'Reading command line args failed!', 430, info)
             GOTO 100
          END IF
       END IF
       !-------------------------------------------------------------------
       !  Parse help flag
       !-------------------------------------------------------------------
       IF (help_enabled) THEN
          CALL find_flag('-h', ok)
          IF (.NOT. ok) CALL find_flag('--help', ok)
          IF (ok) THEN
             CALL print_help
             info = exit_gracefully
             GOTO 100
          END IF
       END IF
       !-------------------------------------------------------------------
       !  Print Control file
       !-------------------------------------------------------------------
       IF (ctrl_enabled) THEN
          CALL find_flag('--print-ctrl', ok)
          IF (ok) THEN
             printing_ctrl = .TRUE.
          END IF
       END IF
       !-------------------------------------------------------------------
       !  Parse rest of the command line
       !-------------------------------------------------------------------
       CALL parse_cmd_line(info)
       IF (info .NE. 0) THEN
          info = ppm_error_fatal
          CALL ppm_error(ppm_err_argument, caller, &
                  'Parsing command line args failed!', 462, info)
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  Parse Control file
       !-------------------------------------------------------------------
       IF (ctrl_enabled) THEN
          CALL find_arg(1, ok, ctrl_file_name)
          CALL parse_ctrl_file(info)
          IF (info .NE. 0) THEN
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, &
                  'Parsing control file failed!', 474, info)
             GOTO 100
          END IF
       END IF
       !-------------------------------------------------------------------
       !  Call default funcs
       !-------------------------------------------------------------------
       CALL call_default_funcs(info)
       IF (info .NE. 0) THEN
          info = ppm_error_fatal
          CALL ppm_error(ppm_err_argument, caller, &
               'Calling default functions failed!', 486, info)
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  Check minmax
       !-------------------------------------------------------------------
       CALL check_minmax(info)
       IF (info .NE. 0) THEN
          info = ppm_error_fatal
          CALL ppm_error(ppm_err_argument, caller, &
               'Min/max check failed!', 497, info)
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  Run validators
       !-------------------------------------------------------------------
       CALL call_validator_funcs(info)
       IF (info .NE. 0) THEN
          info = ppm_error_fatal
          CALL ppm_error(ppm_err_argument, caller, &
               'Calling validator functions failed!', 508, info)
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  Print Control file
       !-------------------------------------------------------------------
       IF (printing_ctrl) THEN
          CALL print_ctrl
          info = exit_gracefully
          GOTO 100
       END IF
       !-------------------------------------------------------------------
       !  DONE!
       !-------------------------------------------------------------------
    END IF ! (ppm_rank .EQ. 0)
    !----------------------------------------------------------------------
    !  Exchange data
    !----------------------------------------------------------------------
100 CONTINUE
    !----------------------------------------------------------------------
    !  Error handling
    !----------------------------------------------------------------------
9999 CONTINUE
    ! cleanup
    CALL deallocate_memory(info .NE. 0)
    CALL substop(caller, t0, info)
    RETURN
  END SUBROUTINE parse_args
  !------------------------------------------------------------------------
  !  Cleanup
  !------------------------------------------------------------------------
  SUBROUTINE deallocate_memory(all)
    LOGICAL, INTENT(IN   ) :: all
    IF (all) THEN
       IF (ASSOCIATED(cmd_args))         DEALLOCATE(cmd_args)
       IF (ASSOCIATED(cmd_args_len))     DEALLOCATE(cmd_args_len)
       IF (ASSOCIATED(cmd_args_used))    DEALLOCATE(cmd_args_used)
    END IF
    ! scalar
    IF (ASSOCIATED(INTEGER_args))        DEALLOCATE(INTEGER_args)
    IF (ASSOCIATED(LONGINT_args))        DEALLOCATE(LONGINT_args)
    IF (ASSOCIATED(SINGLE_args))         DEALLOCATE(SINGLE_args)
    IF (ASSOCIATED(DOUBLE_args))         DEALLOCATE(DOUBLE_args)
    IF (ASSOCIATED(LOGICAL_args))        DEALLOCATE(LOGICAL_args)
    IF (ASSOCIATED(STRING_args))         DEALLOCATE(STRING_args)
    IF (ASSOCIATED(COMPLEX_args))        DEALLOCATE(COMPLEX_args)
    IF (ASSOCIATED(DCOMPLEX_args))       DEALLOCATE(DCOMPLEX_args)
    ! array
    IF (ASSOCIATED(INTEGER_array_args))  DEALLOCATE(INTEGER_array_args)
    IF (ASSOCIATED(LONGINT_array_args))  DEALLOCATE(LONGINT_array_args)
    IF (ASSOCIATED(SINGLE_array_args))   DEALLOCATE(SINGLE_array_args)
    IF (ASSOCIATED(DOUBLE_array_args))   DEALLOCATE(DOUBLE_array_args)
    IF (ASSOCIATED(LOGICAL_array_args))  DEALLOCATE(LOGICAL_array_args)
    IF (ASSOCIATED(STRING_array_args))   DEALLOCATE(STRING_array_args)
    IF (ASSOCIATED(COMPLEX_array_args))  DEALLOCATE(COMPLEX_array_args)
    IF (ASSOCIATED(DCOMPLEX_array_args)) DEALLOCATE(DCOMPLEX_array_args)
    ! other
    IF (ASSOCIATED(groups))              DEALLOCATE(groups)
    IF (ASSOCIATED(group_size))          DEALLOCATE(group_size)
    IF (ASSOCIATED(group_has_ctrl))      DEALLOCATE(group_has_ctrl)
    IF (ASSOCIATED(group_has_arg))       DEALLOCATE(group_has_arg)
  END SUBROUTINE deallocate_memory

  SUBROUTINE reset
!!! Debugging and testing routine. Resets all module variables.
    CALL deallocate_memory(.TRUE.)
    ! scalar
    INTEGER_args_i        = 0
    LONGINT_args_i        = 0
    SINGLE_args_i         = 0
    DOUBLE_args_i         = 0
    LOGICAL_args_i        = 0
    STRING_args_i         = 0
    COMPLEX_args_i        = 0
    DCOMPLEX_args_i       = 0
    ! array
    INTEGER_array_args_i  = 0
    LONGINT_array_args_i  = 0
    SINGLE_array_args_i   = 0
    DOUBLE_array_args_i   = 0
    LOGICAL_array_args_i  = 0
    STRING_array_args_i   = 0
    COMPLEX_array_args_i  = 0
    DCOMPLEX_array_args_i = 0
    ! other
    cmd_args_i            = 0
    cmd_i                 = 0
    groups_i              = -1
    help_enabled          = .TRUE.
    ctrl_enabled          = .TRUE.
    ctrl_file_name        = 'Ctrl'
    ctrl_file_test        = ''
    in_test               = .FALSE.
  END SUBROUTINE reset
  !-------------------------------------------------------------------------
  !  Apply defaults
  !-------------------------------------------------------------------------
  SUBROUTINE apply_defaults(info)
    INTEGER, INTENT(  OUT) :: info
    INTEGER                :: i
    info = 0
    ! scalar
    DO i=1,INTEGER_args_i
       IF (INTEGER_args(i)%default_set) &
            INTEGER_args(i)%variable = INTEGER_args(i)%default
    END DO

    DO i=1,LONGINT_args_i
       IF (LONGINT_args(i)%default_set) &
            LONGINT_args(i)%variable = LONGINT_args(i)%default
    END DO

    DO i=1,SINGLE_args_i
       IF (SINGLE_args(i)%default_set) &
            SINGLE_args(i)%variable = SINGLE_args(i)%default
    END DO

    DO i=1,DOUBLE_args_i
       IF (DOUBLE_args(i)%default_set) &
            DOUBLE_args(i)%variable = DOUBLE_args(i)%default
    END DO

    DO i=1,LOGICAL_args_i
       IF (LOGICAL_args(i)%default_set) &
            LOGICAL_args(i)%variable = LOGICAL_args(i)%default
    END DO

    DO i=1,STRING_args_i
       IF (STRING_args(i)%default_set) &
            STRING_args(i)%variable = STRING_args(i)%default
    END DO

    DO i=1,COMPLEX_args_i
       IF (COMPLEX_args(i)%default_set) &
            COMPLEX_args(i)%variable = COMPLEX_args(i)%default
    END DO

    DO i=1,DCOMPLEX_args_i
       IF (DCOMPLEX_args(i)%default_set) &
            DCOMPLEX_args(i)%variable = DCOMPLEX_args(i)%default
    END DO

  ! array
    DO i=1,INTEGER_array_args_i
       IF (INTEGER_array_args(i)%default_set) &
            INTEGER_array_args(i)%variable = INTEGER_array_args(i)%default
    END DO

    DO i=1,LONGINT_array_args_i
       IF (LONGINT_array_args(i)%default_set) &
            LONGINT_array_args(i)%variable = LONGINT_array_args(i)%default
    END DO

    DO i=1,SINGLE_array_args_i
       IF (SINGLE_array_args(i)%default_set) &
            SINGLE_array_args(i)%variable = SINGLE_array_args(i)%default
    END DO

    DO i=1,DOUBLE_array_args_i
       IF (DOUBLE_array_args(i)%default_set) &
            DOUBLE_array_args(i)%variable = DOUBLE_array_args(i)%default
    END DO

    DO i=1,LOGICAL_array_args_i
       IF (LOGICAL_array_args(i)%default_set) &
            LOGICAL_array_args(i)%variable = LOGICAL_array_args(i)%default
    END DO

    DO i=1,STRING_array_args_i
       IF (STRING_array_args(i)%default_set) &
            STRING_array_args(i)%variable = STRING_array_args(i)%default
    END DO

    DO i=1,COMPLEX_array_args_i
       IF (COMPLEX_array_args(i)%default_set) &
            COMPLEX_array_args(i)%variable = COMPLEX_array_args(i)%default
    END DO

    DO i=1,DCOMPLEX_array_args_i
       IF (DCOMPLEX_array_args(i)%default_set) &
            DCOMPLEX_array_args(i)%variable = DCOMPLEX_array_args(i)%default
    END DO

  END SUBROUTINE apply_defaults
  !------------------------------------------------------------------------
  !  Read in command line args
  !------------------------------------------------------------------------
  SUBROUTINE read_cmd_args(info)
    INTEGER, INTENT(  OUT)          :: info
    CHARACTER(LEN=ppm_char)         :: cbuf
    INTEGER                         :: i, start, nargc
       nargc = COMMAND_ARGUMENT_COUNT()
       start = 0
       cmd_args_i = nargc-start
       ! allocate storage
       IF (ASSOCIATED(cmd_args))      DEALLOCATE(cmd_args)
       IF (ASSOCIATED(cmd_args_len))  DEALLOCATE(cmd_args_len)
       IF (ASSOCIATED(cmd_args_used)) DEALLOCATE(cmd_args_used)
       ALLOCATE(cmd_args(1:cmd_args_i),      STAT=info)
       ALLOCATE(cmd_args_len(1:cmd_args_i),  STAT=info)
       ALLOCATE(cmd_args_used(1:cmd_args_i), STAT=info)
       IF (info .NE. 0) RETURN
       cmd_args_used = .FALSE.
       ! read in all args
       DO i=1,cmd_args_i
          CALL getarg(start+i, cbuf)
          cmd_args_len(i) = LEN_TRIM(cbuf)
          cmd_args(i)     = cbuf(1:cmd_args_len(i))
       END DO
  END SUBROUTINE read_cmd_args
  !-------------------------------------------------------------------------
  !  Parse command line
  !-------------------------------------------------------------------------
  SUBROUTINE parse_cmd_line(info)
    INTEGER, INTENT(  OUT)      :: info
    CHARACTER(LEN=*), PARAMETER :: caller = 'parse_cmd_line'
    CHARACTER(LEN=ppm_char)     :: cvar
    INTEGER                     :: i, ios
    LOGICAL                     :: ok
    LOGICAL                     :: err = .FALSE.
    CHARACTER(LEN=256)          :: value
    ! scalar
             ! find match
             DO i=1,INTEGER_args_i
                ! short flag
                ok = .FALSE.
                IF (INTEGER_args(i)%flag_set) THEN
                   CALL find_flag(INTEGER_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (INTEGER_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(INTEGER_args(i) &
                        %long_flag(1:LEN_TRIM(INTEGER_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) INTEGER_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           INTEGER_args(i)%name(1:LEN_TRIM(INTEGER_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   INTEGER_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,LONGINT_args_i
                ! short flag
                ok = .FALSE.
                IF (LONGINT_args(i)%flag_set) THEN
                   CALL find_flag(LONGINT_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (LONGINT_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(LONGINT_args(i) &
                        %long_flag(1:LEN_TRIM(LONGINT_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) LONGINT_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           LONGINT_args(i)%name(1:LEN_TRIM(LONGINT_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   LONGINT_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,SINGLE_args_i
                ! short flag
                ok = .FALSE.
                IF (SINGLE_args(i)%flag_set) THEN
                   CALL find_flag(SINGLE_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (SINGLE_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(SINGLE_args(i) &
                        %long_flag(1:LEN_TRIM(SINGLE_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) SINGLE_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           SINGLE_args(i)%name(1:LEN_TRIM(SINGLE_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   SINGLE_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,DOUBLE_args_i
                ! short flag
                ok = .FALSE.
                IF (DOUBLE_args(i)%flag_set) THEN
                   CALL find_flag(DOUBLE_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (DOUBLE_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(DOUBLE_args(i) &
                        %long_flag(1:LEN_TRIM(DOUBLE_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) DOUBLE_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           DOUBLE_args(i)%name(1:LEN_TRIM(DOUBLE_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   DOUBLE_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,LOGICAL_args_i
                ! short flag
                ok = .FALSE.
                IF (LOGICAL_args(i)%flag_set) THEN
                   CALL find_flag(LOGICAL_args(i)%flag(1:2), ok)
                END IF
                IF ((.NOT. ok) .AND. (LOGICAL_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(LOGICAL_args(i) &
                        %long_flag(1:LEN_TRIM(LOGICAL_args(i) &
                        %long_flag)), ok)
                END IF
                IF (ok) THEN
                   IF (LOGICAL_args(i)%type .EQV. enabling_flag) THEN
                      LOGICAL_args(i)%variable = .TRUE.
                   ELSE
                      LOGICAL_args(i)%variable = .FALSE.
                   END IF
                   LOGICAL_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,STRING_args_i
                ! short flag
                ok = .FALSE.
                IF (STRING_args(i)%flag_set) THEN
                   CALL find_flag(STRING_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (STRING_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(STRING_args(i) &
                        %long_flag(1:LEN_TRIM(STRING_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) STRING_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           STRING_args(i)%name(1:LEN_TRIM(STRING_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   STRING_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,COMPLEX_args_i
                ! short flag
                ok = .FALSE.
                IF (COMPLEX_args(i)%flag_set) THEN
                   CALL find_flag(COMPLEX_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (COMPLEX_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(COMPLEX_args(i) &
                        %long_flag(1:LEN_TRIM(COMPLEX_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) COMPLEX_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           COMPLEX_args(i)%name(1:LEN_TRIM(COMPLEX_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   COMPLEX_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,DCOMPLEX_args_i
                ! short flag
                ok = .FALSE.
                IF (DCOMPLEX_args(i)%flag_set) THEN
                   CALL find_flag(DCOMPLEX_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (DCOMPLEX_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(DCOMPLEX_args(i) &
                        %long_flag(1:LEN_TRIM(DCOMPLEX_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) DCOMPLEX_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           DCOMPLEX_args(i)%name(1:LEN_TRIM(DCOMPLEX_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   DCOMPLEX_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
  ! array
             ! find match
             DO i=1,INTEGER_array_args_i
                ! short flag
                ok = .FALSE.
                IF (INTEGER_array_args(i)%flag_set) THEN
                   CALL find_flag(INTEGER_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (INTEGER_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(INTEGER_array_args(i) &
                        %long_flag(1:LEN_TRIM(INTEGER_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) INTEGER_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           INTEGER_array_args(i)%name(1:LEN_TRIM(INTEGER_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   INTEGER_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,LONGINT_array_args_i
                ! short flag
                ok = .FALSE.
                IF (LONGINT_array_args(i)%flag_set) THEN
                   CALL find_flag(LONGINT_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (LONGINT_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(LONGINT_array_args(i) &
                        %long_flag(1:LEN_TRIM(LONGINT_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) LONGINT_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           LONGINT_array_args(i)%name(1:LEN_TRIM(LONGINT_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   LONGINT_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,SINGLE_array_args_i
                ! short flag
                ok = .FALSE.
                IF (SINGLE_array_args(i)%flag_set) THEN
                   CALL find_flag(SINGLE_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (SINGLE_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(SINGLE_array_args(i) &
                        %long_flag(1:LEN_TRIM(SINGLE_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) SINGLE_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           SINGLE_array_args(i)%name(1:LEN_TRIM(SINGLE_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   SINGLE_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,DOUBLE_array_args_i
                ! short flag
                ok = .FALSE.
                IF (DOUBLE_array_args(i)%flag_set) THEN
                   CALL find_flag(DOUBLE_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (DOUBLE_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(DOUBLE_array_args(i) &
                        %long_flag(1:LEN_TRIM(DOUBLE_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) DOUBLE_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           DOUBLE_array_args(i)%name(1:LEN_TRIM(DOUBLE_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   DOUBLE_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,LOGICAL_array_args_i
                ! short flag
                ok = .FALSE.
                IF (LOGICAL_array_args(i)%flag_set) THEN
                   CALL find_flag(LOGICAL_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (LOGICAL_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(LOGICAL_array_args(i) &
                        %long_flag(1:LEN_TRIM(LOGICAL_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) LOGICAL_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           LOGICAL_array_args(i)%name(1:LEN_TRIM(LOGICAL_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   LOGICAL_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,STRING_array_args_i
                ! short flag
                ok = .FALSE.
                IF (STRING_array_args(i)%flag_set) THEN
                   CALL find_flag(STRING_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (STRING_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(STRING_array_args(i) &
                        %long_flag(1:LEN_TRIM(STRING_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) STRING_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           STRING_array_args(i)%name(1:LEN_TRIM(STRING_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   STRING_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,COMPLEX_array_args_i
                ! short flag
                ok = .FALSE.
                IF (COMPLEX_array_args(i)%flag_set) THEN
                   CALL find_flag(COMPLEX_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (COMPLEX_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(COMPLEX_array_args(i) &
                        %long_flag(1:LEN_TRIM(COMPLEX_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) COMPLEX_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           COMPLEX_array_args(i)%name(1:LEN_TRIM(COMPLEX_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   COMPLEX_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
             ! find match
             DO i=1,DCOMPLEX_array_args_i
                ! short flag
                ok = .FALSE.
                IF (DCOMPLEX_array_args(i)%flag_set) THEN
                   CALL find_flag(DCOMPLEX_array_args(i)%flag(1:2), ok, value, err)
                END IF
                IF ((.NOT. ok) .AND. (DCOMPLEX_array_args(i)%long_flag_set)) THEN
                   ! long flag
                   CALL find_flag(DCOMPLEX_array_args(i) &
                        %long_flag(1:LEN_TRIM(DCOMPLEX_array_args(i) &
                        %long_flag)), ok, value, err)
                END IF
                IF (ok) THEN
                   ! match found - convert the arg
                   READ (value,*,IOSTAT=ios) DCOMPLEX_array_args(i)%variable
                   ! check for failure
                   IF (ios .NE. 0) THEN
                      WRITE (cvar,*) 'Invalid argument (', value(1:LEN_TRIM(value)), ') for arg ', &
                           DCOMPLEX_array_args(i)%name(1:LEN_TRIM(DCOMPLEX_array_args(i)%name))
                      info = ppm_error_fatal
                      CALL ppm_error(ppm_err_argument, caller, cvar, 34, info)
                      GOTO 9999
                   END IF
                   DCOMPLEX_array_args(i)%clf_supplied = .TRUE.
                ELSE IF (err) THEN
                   info = ppm_error_fatal
                   WRITE (cvar,*) 'Flag ', value(1:LEN_TRIM(value)), &
                        ' has to be supplied with a value.'
                   CALL ppm_error(ppm_err_argument, caller, cvar, 49, info)
                   GOTO 9999
                END IF
             END DO
9999 CONTINUE
  END SUBROUTINE parse_cmd_line
  !------------------------------------------------------------------------
  !  Parse Control file
  !------------------------------------------------------------------------
  SUBROUTINE parse_ctrl_file(info)
    INTEGER, INTENT(  OUT)      :: info
    CHARACTER(LEN=*), PARAMETER :: caller='parse_ctrl_file'
    CHARACTER(LEN=ppm_char)     :: cbuf, cvalue, carg, cvar
    CHARACTER(LEN=ppm_char)     :: current_var
    CHARACTER(LEN=10000)        :: errmsg
    INTEGER                     :: ilenctrl
    INTEGER                     :: iUnit, istat, ios
    LOGICAL                     :: lExist
    INTEGER                     :: iline
    INTEGER                     :: ilen
    INTEGER                     :: i,j,idx
    !-------------------------------------------------------------
    !  Check that the ctrl file exists
    !-------------------------------------------------------------
    ilenctrl = LEN_TRIM(ctrl_file_name)
    IF (ilenctrl .LT. 1) THEN
       info = ppm_error_fatal
       CALL ppm_error(ppm_err_argument, caller, &
            'No ctrl file given', 834, info)
       GOTO 9999
    END IF
    INQUIRE(FILE=ctrl_file_name, EXIST=lExist)
    IF (.NOT. lExist) THEN
       info = ppm_error_fatal
       WRITE(cvar,'(2A)') 'No such file: ', ctrl_file_name(1:ilenctrl)
       CALL ppm_error(ppm_err_argument, caller, cvar, 841, info)
       GOTO 9999
    END IF
    !-------------------------------------------------------------
    !  Open the file
    !-------------------------------------------------------------
    iUnit = 19
    OPEN(iUnit, FILE=ctrl_file_name, IOSTAT=ios, ACTION='READ')
    IF (ios .NE. 0) THEN
       info = ppm_error_fatal
       WRITE(cvar,'(2A)') 'Failed to open file: ', ctrl_file_name(1:ilenctrl)
       CALL ppm_error(ppm_err_argument, caller, cvar, 852, info)
       GOTO 9999
    END IF
    !-------------------------------------------------------------
    !  Scan file
    !-------------------------------------------------------------
    iline = 0
    var_loop: DO
       iline = iline + 1
       !----------------------------------------------------------
       !  Read line
       !----------------------------------------------------------
       READ(iUnit,'(A)', END=100, ERR=200) cbuf
       ilen = LEN_TRIM(cbuf)
       !----------------------------------------------------------
       !  Skip comment or empty lines 
       !----------------------------------------------------------
       IF (ilen .GT. 0 .AND. cbuf(1:1) .NE. '#') THEN
          !-------------------------------------------------------
          !  Remove space
          !-------------------------------------------------------
          j = 0
          DO i=1,ilen
             IF (cbuf(i:i) .NE. ' ') THEN
                j = j + 1
                cbuf(j:j) = cbuf(i:i)
             END IF
          END DO
          !-------------------------------------------------------
          !  Update length of string
          !-------------------------------------------------------
          ilen = j
          !-------------------------------------------------------
          !  Find position of =
          !-------------------------------------------------------
          idx = INDEX(cbuf, '=')
          !-------------------------------------------------------
          !  Exit if missing
          !-------------------------------------------------------
          IF (idx .LT. 0) THEN
             info = ppm_error_fatal
             WRITE(cvar,'(A,I5)') 'Incorrect line: ', iline
             CALL ppm_error(ppm_err_argument, caller, cvar, 894, info)
             GOTO 9999
          ENDIF
          !-------------------------------------------------------
          !  Get argument and value
          !-------------------------------------------------------
          carg   = ADJUSTL(cbuf(1:idx-1))
          cvalue = ADJUSTL(cbuf(idx+1:ilen))
          !-------------------------------------------------------
          !  Convert to upper case
          !-------------------------------------------------------
          CALL UpperCase(carg, idx-1, info)
          !-------------------------------------------------------
          !  Find the variable
          !-------------------------------------------------------          
          ! scalar
          DO i=1,INTEGER_args_i
             IF (INTEGER_args(i)%ctrl_name_set) THEN
                cvar = INTEGER_args(i)%ctrl_name &
                     (1:LEN_TRIM(INTEGER_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. INTEGER_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) INTEGER_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,LONGINT_args_i
             IF (LONGINT_args(i)%ctrl_name_set) THEN
                cvar = LONGINT_args(i)%ctrl_name &
                     (1:LEN_TRIM(LONGINT_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. LONGINT_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) LONGINT_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,SINGLE_args_i
             IF (SINGLE_args(i)%ctrl_name_set) THEN
                cvar = SINGLE_args(i)%ctrl_name &
                     (1:LEN_TRIM(SINGLE_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. SINGLE_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) SINGLE_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,DOUBLE_args_i
             IF (DOUBLE_args(i)%ctrl_name_set) THEN
                cvar = DOUBLE_args(i)%ctrl_name &
                     (1:LEN_TRIM(DOUBLE_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. DOUBLE_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) DOUBLE_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,LOGICAL_args_i
             IF (LOGICAL_args(i)%ctrl_name_set) THEN
                cvar = LOGICAL_args(i)%ctrl_name &
                     (1:LEN_TRIM(LOGICAL_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. LOGICAL_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) LOGICAL_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,STRING_args_i
             IF (STRING_args(i)%ctrl_name_set) THEN
                cvar = STRING_args(i)%ctrl_name &
                     (1:LEN_TRIM(STRING_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. STRING_args(i)%clf_supplied) THEN
                      STRING_args(i)%variable = ADJUSTL(cvalue)
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,COMPLEX_args_i
             IF (COMPLEX_args(i)%ctrl_name_set) THEN
                cvar = COMPLEX_args(i)%ctrl_name &
                     (1:LEN_TRIM(COMPLEX_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. COMPLEX_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) COMPLEX_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,DCOMPLEX_args_i
             IF (DCOMPLEX_args(i)%ctrl_name_set) THEN
                cvar = DCOMPLEX_args(i)%ctrl_name &
                     (1:LEN_TRIM(DCOMPLEX_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. DCOMPLEX_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) DCOMPLEX_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
  ! array
          DO i=1,INTEGER_array_args_i
             IF (INTEGER_array_args(i)%ctrl_name_set) THEN
                cvar = INTEGER_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(INTEGER_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. INTEGER_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) INTEGER_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,LONGINT_array_args_i
             IF (LONGINT_array_args(i)%ctrl_name_set) THEN
                cvar = LONGINT_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(LONGINT_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. LONGINT_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) LONGINT_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,SINGLE_array_args_i
             IF (SINGLE_array_args(i)%ctrl_name_set) THEN
                cvar = SINGLE_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(SINGLE_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. SINGLE_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) SINGLE_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,DOUBLE_array_args_i
             IF (DOUBLE_array_args(i)%ctrl_name_set) THEN
                cvar = DOUBLE_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(DOUBLE_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. DOUBLE_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) DOUBLE_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,LOGICAL_array_args_i
             IF (LOGICAL_array_args(i)%ctrl_name_set) THEN
                cvar = LOGICAL_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(LOGICAL_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. LOGICAL_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) LOGICAL_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,STRING_array_args_i
             IF (STRING_array_args(i)%ctrl_name_set) THEN
                cvar = STRING_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(STRING_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. STRING_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) STRING_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,COMPLEX_array_args_i
             IF (COMPLEX_array_args(i)%ctrl_name_set) THEN
                cvar = COMPLEX_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(COMPLEX_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. COMPLEX_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) COMPLEX_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
          DO i=1,DCOMPLEX_array_args_i
             IF (DCOMPLEX_array_args(i)%ctrl_name_set) THEN
                cvar = DCOMPLEX_array_args(i)%ctrl_name &
                     (1:LEN_TRIM(DCOMPLEX_array_args(i)%ctrl_name))
                current_var = cvar
                CALL UpperCase(cvar, LEN_TRIM(cvar), info)
                IF (carg .EQ. cvar) THEN
                   IF (.NOT. DCOMPLEX_array_args(i)%clf_supplied) THEN
                      READ (cvalue, *, IOSTAT=ios, ERR=300) DCOMPLEX_array_args(i)%variable
                   END IF
                   CYCLE var_loop
                END IF
             END IF
          END DO
       END IF
    END DO var_loop
    GOTO 9999
300 CONTINUE
    info = ppm_error_fatal
    WRITE(errmsg,'(5A,I5,2A)') 'Error reading variable: ',     &
                 current_var(1:LEN_TRIM(current_var)),         &
                 ' from string: ', cvalue(1:LEN_TRIM(cvalue)), &
                 ' on line: ', iline,                          &
                 ' of file: ', ctrl_file_name(1:ilenctrl)
    CALL ppm_error(ppm_err_argument, caller, errmsg, 954, info)
    GOTO 9999
200 CONTINUE
    info = ppm_error_fatal
    WRITE(errmsg,'(A,I5,2A)') 'Error reading line: ', iline,     &
         &                  ' of file: ', ctrl_file_name(1:ilenctrl)
    CALL ppm_error(ppm_err_argument, caller, errmsg, 960, info)
    GOTO 9999
100 CONTINUE
    !----------------------------------------------------------------------
    !  Close file
    !----------------------------------------------------------------------
9999 CONTINUE
    CLOSE(iUnit)
    RETURN
  END SUBROUTINE parse_ctrl_file
  !------------------------------------------------------------------------
  !  Call default funcs
  !------------------------------------------------------------------------
  SUBROUTINE call_default_funcs(info)
    INTEGER, INTENT(  OUT)      :: info
    CHARACTER(LEN=*), PARAMETER :: caller = 'call_default_funcs'
    INTEGER                     :: i
    DO i=1,INTEGER_args_i
       IF (INTEGER_args(i)%default_func_set) THEN
          IF (.NOT. INTEGER_args(i)%default_func(INTEGER_args(i)%variable)) THEN
             IF (INTEGER_args(i)%default_set) THEN
                INTEGER_args(i)%variable = INTEGER_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,LONGINT_args_i
       IF (LONGINT_args(i)%default_func_set) THEN
          IF (.NOT. LONGINT_args(i)%default_func(LONGINT_args(i)%variable)) THEN
             IF (LONGINT_args(i)%default_set) THEN
                LONGINT_args(i)%variable = LONGINT_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,SINGLE_args_i
       IF (SINGLE_args(i)%default_func_set) THEN
          IF (.NOT. SINGLE_args(i)%default_func(SINGLE_args(i)%variable)) THEN
             IF (SINGLE_args(i)%default_set) THEN
                SINGLE_args(i)%variable = SINGLE_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,DOUBLE_args_i
       IF (DOUBLE_args(i)%default_func_set) THEN
          IF (.NOT. DOUBLE_args(i)%default_func(DOUBLE_args(i)%variable)) THEN
             IF (DOUBLE_args(i)%default_set) THEN
                DOUBLE_args(i)%variable = DOUBLE_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,LOGICAL_args_i
       IF (LOGICAL_args(i)%default_func_set) THEN
          IF (.NOT. LOGICAL_args(i)%default_func(LOGICAL_args(i)%variable)) THEN
             IF (LOGICAL_args(i)%default_set) THEN
                LOGICAL_args(i)%variable = LOGICAL_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,STRING_args_i
       IF (STRING_args(i)%default_func_set) THEN
          IF (.NOT. STRING_args(i)%default_func(STRING_args(i)%variable)) THEN
             IF (STRING_args(i)%default_set) THEN
                STRING_args(i)%variable = STRING_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,COMPLEX_args_i
       IF (COMPLEX_args(i)%default_func_set) THEN
          IF (.NOT. COMPLEX_args(i)%default_func(COMPLEX_args(i)%variable)) THEN
             IF (COMPLEX_args(i)%default_set) THEN
                COMPLEX_args(i)%variable = COMPLEX_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,DCOMPLEX_args_i
       IF (DCOMPLEX_args(i)%default_func_set) THEN
          IF (.NOT. DCOMPLEX_args(i)%default_func(DCOMPLEX_args(i)%variable)) THEN
             IF (DCOMPLEX_args(i)%default_set) THEN
                DCOMPLEX_args(i)%variable = DCOMPLEX_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
  ! array
    DO i=1,INTEGER_array_args_i
       IF (INTEGER_array_args(i)%default_func_set) THEN
          IF (.NOT. INTEGER_array_args(i)%default_func(INTEGER_array_args(i)%variable)) THEN
             IF (INTEGER_array_args(i)%default_set) THEN
                INTEGER_array_args(i)%variable = INTEGER_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,LONGINT_array_args_i
       IF (LONGINT_array_args(i)%default_func_set) THEN
          IF (.NOT. LONGINT_array_args(i)%default_func(LONGINT_array_args(i)%variable)) THEN
             IF (LONGINT_array_args(i)%default_set) THEN
                LONGINT_array_args(i)%variable = LONGINT_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,SINGLE_array_args_i
       IF (SINGLE_array_args(i)%default_func_set) THEN
          IF (.NOT. SINGLE_array_args(i)%default_func(SINGLE_array_args(i)%variable)) THEN
             IF (SINGLE_array_args(i)%default_set) THEN
                SINGLE_array_args(i)%variable = SINGLE_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,DOUBLE_array_args_i
       IF (DOUBLE_array_args(i)%default_func_set) THEN
          IF (.NOT. DOUBLE_array_args(i)%default_func(DOUBLE_array_args(i)%variable)) THEN
             IF (DOUBLE_array_args(i)%default_set) THEN
                DOUBLE_array_args(i)%variable = DOUBLE_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,LOGICAL_array_args_i
       IF (LOGICAL_array_args(i)%default_func_set) THEN
          IF (.NOT. LOGICAL_array_args(i)%default_func(LOGICAL_array_args(i)%variable)) THEN
             IF (LOGICAL_array_args(i)%default_set) THEN
                LOGICAL_array_args(i)%variable = LOGICAL_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,STRING_array_args_i
       IF (STRING_array_args(i)%default_func_set) THEN
          IF (.NOT. STRING_array_args(i)%default_func(STRING_array_args(i)%variable)) THEN
             IF (STRING_array_args(i)%default_set) THEN
                STRING_array_args(i)%variable = STRING_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,COMPLEX_array_args_i
       IF (COMPLEX_array_args(i)%default_func_set) THEN
          IF (.NOT. COMPLEX_array_args(i)%default_func(COMPLEX_array_args(i)%variable)) THEN
             IF (COMPLEX_array_args(i)%default_set) THEN
                COMPLEX_array_args(i)%variable = COMPLEX_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
    DO i=1,DCOMPLEX_array_args_i
       IF (DCOMPLEX_array_args(i)%default_func_set) THEN
          IF (.NOT. DCOMPLEX_array_args(i)%default_func(DCOMPLEX_array_args(i)%variable)) THEN
             IF (DCOMPLEX_array_args(i)%default_set) THEN
                DCOMPLEX_array_args(i)%variable = DCOMPLEX_array_args(i)%default
             ELSE
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, &
                     'Error applying default functions!', 10, info)
                GOTO 9999
             END IF
          END IF
       END IF
    END DO
9999 CONTINUE
  END SUBROUTINE call_default_funcs
  !------------------------------------------------------------------------
  !  Check min max
  !------------------------------------------------------------------------
  SUBROUTINE check_minmax(info)
    INTEGER, INTENT(  OUT)      :: info
    CHARACTER(LEN=*), PARAMETER :: caller = 'check_minmax'
    CHARACTER(LEN=ppm_char)     :: cvar
    INTEGER                     :: i
    DO i=1,INTEGER_args_i
       IF (INTEGER_args(i)%min_set) THEN
          IF (INTEGER_args(i)%variable .LT. INTEGER_args(i)%min) THEN
             WRITE (cvar,*) 'Argument ', INTEGER_args(i)&
                  &%name(1:LEN_TRIM(INTEGER_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (INTEGER_args(i)%max_set) THEN
          IF (INTEGER_args(i)%variable .GT. INTEGER_args(i)%max) THEN
             WRITE (cvar,*) 'Argument ', INTEGER_args(i)&
                  &%name(1:LEN_TRIM(INTEGER_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,LONGINT_args_i
       IF (LONGINT_args(i)%min_set) THEN
          IF (LONGINT_args(i)%variable .LT. LONGINT_args(i)%min) THEN
             WRITE (cvar,*) 'Argument ', LONGINT_args(i)&
                  &%name(1:LEN_TRIM(LONGINT_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (LONGINT_args(i)%max_set) THEN
          IF (LONGINT_args(i)%variable .GT. LONGINT_args(i)%max) THEN
             WRITE (cvar,*) 'Argument ', LONGINT_args(i)&
                  &%name(1:LEN_TRIM(LONGINT_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,SINGLE_args_i
       IF (SINGLE_args(i)%min_set) THEN
          IF (SINGLE_args(i)%variable .LT. SINGLE_args(i)%min) THEN
             WRITE (cvar,*) 'Argument ', SINGLE_args(i)&
                  &%name(1:LEN_TRIM(SINGLE_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (SINGLE_args(i)%max_set) THEN
          IF (SINGLE_args(i)%variable .GT. SINGLE_args(i)%max) THEN
             WRITE (cvar,*) 'Argument ', SINGLE_args(i)&
                  &%name(1:LEN_TRIM(SINGLE_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,DOUBLE_args_i
       IF (DOUBLE_args(i)%min_set) THEN
          IF (DOUBLE_args(i)%variable .LT. DOUBLE_args(i)%min) THEN
             WRITE (cvar,*) 'Argument ', DOUBLE_args(i)&
                  &%name(1:LEN_TRIM(DOUBLE_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (DOUBLE_args(i)%max_set) THEN
          IF (DOUBLE_args(i)%variable .GT. DOUBLE_args(i)%max) THEN
             WRITE (cvar,*) 'Argument ', DOUBLE_args(i)&
                  &%name(1:LEN_TRIM(DOUBLE_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
! #define DTYPE STRING
! #define __STRING
! #include "ctrl/minmax.f"
    DO i=1,INTEGER_array_args_i
       IF (INTEGER_array_args(i)%min_set) THEN
          IF (ANY(INTEGER_array_args(i)%variable .LT. INTEGER_array_args(i)%min)) THEN
             WRITE (cvar,*) 'Argument ', INTEGER_array_args(i)&
                  &%name(1:LEN_TRIM(INTEGER_array_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (INTEGER_array_args(i)%max_set) THEN
          IF (ANY(INTEGER_array_args(i)%variable .GT. INTEGER_array_args(i)%max)) THEN
             WRITE (cvar,*) 'Argument ', INTEGER_array_args(i)&
                  &%name(1:LEN_TRIM(INTEGER_array_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,LONGINT_array_args_i
       IF (LONGINT_array_args(i)%min_set) THEN
          IF (ANY(LONGINT_array_args(i)%variable .LT. LONGINT_array_args(i)%min)) THEN
             WRITE (cvar,*) 'Argument ', LONGINT_array_args(i)&
                  &%name(1:LEN_TRIM(LONGINT_array_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (LONGINT_array_args(i)%max_set) THEN
          IF (ANY(LONGINT_array_args(i)%variable .GT. LONGINT_array_args(i)%max)) THEN
             WRITE (cvar,*) 'Argument ', LONGINT_array_args(i)&
                  &%name(1:LEN_TRIM(LONGINT_array_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,SINGLE_array_args_i
       IF (SINGLE_array_args(i)%min_set) THEN
          IF (ANY(SINGLE_array_args(i)%variable .LT. SINGLE_array_args(i)%min)) THEN
             WRITE (cvar,*) 'Argument ', SINGLE_array_args(i)&
                  &%name(1:LEN_TRIM(SINGLE_array_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (SINGLE_array_args(i)%max_set) THEN
          IF (ANY(SINGLE_array_args(i)%variable .GT. SINGLE_array_args(i)%max)) THEN
             WRITE (cvar,*) 'Argument ', SINGLE_array_args(i)&
                  &%name(1:LEN_TRIM(SINGLE_array_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
    DO i=1,DOUBLE_array_args_i
       IF (DOUBLE_array_args(i)%min_set) THEN
          IF (ANY(DOUBLE_array_args(i)%variable .LT. DOUBLE_array_args(i)%min)) THEN
             WRITE (cvar,*) 'Argument ', DOUBLE_array_args(i)&
                  &%name(1:LEN_TRIM(DOUBLE_array_args(i)%name)), &
                  ' fails min check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 21, info)
             GOTO 9999
          END IF
       END IF
       IF (DOUBLE_array_args(i)%max_set) THEN
          IF (ANY(DOUBLE_array_args(i)%variable .GT. DOUBLE_array_args(i)%max)) THEN
             WRITE (cvar,*) 'Argument ', DOUBLE_array_args(i)&
                  &%name(1:LEN_TRIM(DOUBLE_array_args(i)%name)), &
                  ' fails max check!'
             info = ppm_error_fatal
             CALL ppm_error(ppm_err_argument, caller, cvar, 43, info)
             GOTO 9999
          END IF
       END IF
    END DO
! #define DTYPE STRING_array
! #define __STRING
! #include "ctrl/minmax.f"
9999 CONTINUE
  END SUBROUTINE check_minmax
  !------------------------------------------------------------------------
  !  Call validator functions
  !------------------------------------------------------------------------
  SUBROUTINE call_validator_funcs(info)
    INTEGER, INTENT(  OUT)      :: info
    CHARACTER(LEN=*), PARAMETER :: caller = 'call_validator_funcs'
    CHARACTER(LEN=ppm_char)     :: cvar
    INTEGER                     :: i
    ! scalar
       DO i=1,INTEGER_args_i
          IF (INTEGER_args(i)%validator_set) THEN
             IF (.NOT. INTEGER_args(i)%validator(INTEGER_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', INTEGER_args(i)%name(1:LEN_TRIM(INTEGER_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,LONGINT_args_i
          IF (LONGINT_args(i)%validator_set) THEN
             IF (.NOT. LONGINT_args(i)%validator(LONGINT_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', LONGINT_args(i)%name(1:LEN_TRIM(LONGINT_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,SINGLE_args_i
          IF (SINGLE_args(i)%validator_set) THEN
             IF (.NOT. SINGLE_args(i)%validator(SINGLE_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', SINGLE_args(i)%name(1:LEN_TRIM(SINGLE_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,DOUBLE_args_i
          IF (DOUBLE_args(i)%validator_set) THEN
             IF (.NOT. DOUBLE_args(i)%validator(DOUBLE_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', DOUBLE_args(i)%name(1:LEN_TRIM(DOUBLE_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,LOGICAL_args_i
          IF (LOGICAL_args(i)%validator_set) THEN
             IF (.NOT. LOGICAL_args(i)%validator(LOGICAL_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', LOGICAL_args(i)%name(1:LEN_TRIM(LOGICAL_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,STRING_args_i
          IF (STRING_args(i)%validator_set) THEN
             IF (.NOT. STRING_args(i)%validator(STRING_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', STRING_args(i)%name(1:LEN_TRIM(STRING_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,COMPLEX_args_i
          IF (COMPLEX_args(i)%validator_set) THEN
             IF (.NOT. COMPLEX_args(i)%validator(COMPLEX_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', COMPLEX_args(i)%name(1:LEN_TRIM(COMPLEX_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,DCOMPLEX_args_i
          IF (DCOMPLEX_args(i)%validator_set) THEN
             IF (.NOT. DCOMPLEX_args(i)%validator(DCOMPLEX_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', DCOMPLEX_args(i)%name(1:LEN_TRIM(DCOMPLEX_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
  ! array
       DO i=1,INTEGER_array_args_i
          IF (INTEGER_array_args(i)%validator_set) THEN
             IF (.NOT. INTEGER_array_args(i)%validator(INTEGER_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', INTEGER_array_args(i)%name(1:LEN_TRIM(INTEGER_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,LONGINT_array_args_i
          IF (LONGINT_array_args(i)%validator_set) THEN
             IF (.NOT. LONGINT_array_args(i)%validator(LONGINT_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', LONGINT_array_args(i)%name(1:LEN_TRIM(LONGINT_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,SINGLE_array_args_i
          IF (SINGLE_array_args(i)%validator_set) THEN
             IF (.NOT. SINGLE_array_args(i)%validator(SINGLE_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', SINGLE_array_args(i)%name(1:LEN_TRIM(SINGLE_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,DOUBLE_array_args_i
          IF (DOUBLE_array_args(i)%validator_set) THEN
             IF (.NOT. DOUBLE_array_args(i)%validator(DOUBLE_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', DOUBLE_array_args(i)%name(1:LEN_TRIM(DOUBLE_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,LOGICAL_array_args_i
          IF (LOGICAL_array_args(i)%validator_set) THEN
             IF (.NOT. LOGICAL_array_args(i)%validator(LOGICAL_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', LOGICAL_array_args(i)%name(1:LEN_TRIM(LOGICAL_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,STRING_array_args_i
          IF (STRING_array_args(i)%validator_set) THEN
             IF (.NOT. STRING_array_args(i)%validator(STRING_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', STRING_array_args(i)%name(1:LEN_TRIM(STRING_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,COMPLEX_array_args_i
          IF (COMPLEX_array_args(i)%validator_set) THEN
             IF (.NOT. COMPLEX_array_args(i)%validator(COMPLEX_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', COMPLEX_array_args(i)%name(1:LEN_TRIM(COMPLEX_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
       DO i=1,DCOMPLEX_array_args_i
          IF (DCOMPLEX_array_args(i)%validator_set) THEN
             IF (.NOT. DCOMPLEX_array_args(i)%validator(DCOMPLEX_array_args(i)%variable)) THEN
                WRITE (cvar,*) 'Argument ', DCOMPLEX_array_args(i)%name(1:LEN_TRIM(DCOMPLEX_array_args(i)%name)), &
                     ' fails validator check!'
                info = ppm_error_fatal
                CALL ppm_error(ppm_err_argument, caller, cvar, 8, info)
                GOTO 9999
             END IF
          END IF
       END DO
9999 CONTINUE
  END SUBROUTINE call_validator_funcs
  !------------------------------------------------------------------------
  !  Special args
  !------------------------------------------------------------------------
  SUBROUTINE disable_help
!!! Turns of help flag parsing.
    help_enabled = .FALSE.
  END SUBROUTINE disable_help

  SUBROUTINE disable_ctrl
!!! Turns of control file parsing.
    ctrl_enabled = .FALSE.
  END SUBROUTINE disable_ctrl

  SUBROUTINE set_ctrl_name(name)
!!! Sets the control file name.
    CHARACTER(LEN=*), INTENT(IN   )  :: name
!!! Name of the control file.
    ctrl_file_name = name
  END SUBROUTINE set_ctrl_name
  !------------------------------------------------------------------------
  !  Set command line args (usefull for running tests)
  !------------------------------------------------------------------------
  SUBROUTINE add_cmd(arg, value)
!!! Debugging and testing procedure. First call replaces the actual
!!! command arguments with the supplied values. Can be called many
!!! times to incrementally build the fake argument list.
    CHARACTER(LEN=*), INTENT(IN   )           :: arg
!!! Argument string to be appended to the list.
    CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL :: value
!!! Optional value to append.
    CHARACTER(LEN=256), POINTER, DIMENSION(:) :: temp_a => NULL()
    INTEGER,            POINTER, DIMENSION(:) :: temp_l => NULL()
    INTEGER                                   :: inc
    INTEGER                                   :: l
    in_test = .TRUE.
    inc = 1
    IF (PRESENT(value)) inc=2
    IF (.NOT. ASSOCIATED(cmd_args)) THEN
       ALLOCATE(cmd_args(1:di))
       ALLOCATE(cmd_args_len(1:di))
       ALLOCATE(cmd_args_used(1:di))
       cmd_args_used = .FALSE.
    ELSE
       l = SIZE(cmd_args)
       IF (l .LT. cmd_args_i + inc) THEN
          ALLOCATE(temp_a(1:(l + di)))
          ALLOCATE(temp_l(1:(l + di)))
          temp_a(1:l) = cmd_args(1:l)
          temp_l(1:l) = cmd_args_len(1:l)
          DEALLOCATE(cmd_args)
          DEALLOCATE(cmd_args_len)
          DEALLOCATE(cmd_args_used)
          ALLOCATE(cmd_args_used(1:(l+di)))
          cmd_args     => temp_a
          cmd_args_len => temp_l
          cmd_args_used = .FALSE.
       END IF
    END IF
    cmd_i = cmd_i + 1
    cmd_args_i = cmd_args_i + 1
    cmd_args(cmd_args_i)     =     arg
    cmd_args_len(cmd_args_i) = LEN(arg)
    IF (inc .EQ. 2) THEN
       cmd_i = cmd_i + 1
       cmd_args_i = cmd_args_i + 1
       cmd_args(cmd_args_i)     =     value
       cmd_args_len(cmd_args_i) = LEN(value)
    END IF
  END SUBROUTINE add_cmd
  !-------------------------------------------------------------------------
  !  Look-up flags
  !-------------------------------------------------------------------------
  SUBROUTINE find_flag(flag, success, value, err)
!!! Returns the value supplied with the flag _flag_.
    CHARACTER(LEN=*), INTENT(IN   )             :: flag
!!! Flag string (eg. _'-f'_ or _'--flag'_).
    LOGICAL,          INTENT(  OUT)             :: success
!!! True on success. False if there is no such flag or the value is
!!! not supplied.
    CHARACTER(LEN=*), INTENT(  OUT), OPTIONAL   :: value
!!! The value of supplied for the flag (character string).
    LOGICAL,          INTENT(  OUT), OPTIONAL   :: err
!!! Returns .TRUE. if the flag was supplied without a value.
    INTEGER                                     :: i
    CHARACTER(LEN=*), PARAMETER                 :: caller = 'find_flag'
    CHARACTER(LEN=ppm_char)                     :: cvar
    INTEGER                                     :: info
    success = .FALSE.
    IF (PRESENT(err)) err = .FALSE.
    IF (cmd_args_i .EQ. 0) RETURN
    DO i=1,cmd_args_i
       IF (cmd_args(i)(1:cmd_args_len(i)) .EQ. flag) THEN
          success          = .TRUE.
          IF (.NOT. cmd_args_used(i)) cmd_i = cmd_i - 1
          cmd_args_used(i) = .TRUE.
          IF (PRESENT(value)) THEN
             IF (i+1 .GT. cmd_args_i) THEN
                success = .FALSE.
                IF (PRESENT(err)) err = .TRUE.
                info    = ppm_error_warning
                WRITE (cvar,*) "Flag ", flag, " requires an argument."
                CALL ppm_error(ppm_err_argument, caller, cvar, &
                     1196, info)
             ELSE
                value = cmd_args(i+1)
                IF (.NOT. cmd_args_used(i+1)) cmd_i = cmd_i - 1
                cmd_args_used(i+1) = .TRUE.
             END IF
          END IF
          RETURN
       END IF
    END DO
  END SUBROUTINE find_flag
  !------------------------------------------------------------------------
  !  Get arg count
  !------------------------------------------------------------------------
  INTEGER FUNCTION arg_count()
!!! Returns the number of positional arguments found.
    arg_count = cmd_i
  END FUNCTION arg_count
  !------------------------------------------------------------------------
  !  Look-up args (after all flags have been read!!!)
  !------------------------------------------------------------------------
  SUBROUTINE find_arg(position, success, value)
!!! Returns a positional argument at _position_.
    INTEGER,          INTENT(IN   )  :: position
!!! Index of the positional argument, 1 based.
    LOGICAL,          INTENT(  OUT)  :: success
!!! True on success. False if there is no positional arg with the
!!! supplied index.
    CHARACTER(LEN=*), INTENT(INOUT)  :: value
!!! The value of the positional argument (character string).
    INTEGER                          :: i, j
    j = 0
    success = .FALSE.
    DO i=1,cmd_args_i
       IF (.NOT. cmd_args_used(i)) THEN
          j = j + 1
          IF (position .EQ. j) THEN
             value   = cmd_args(i)
             success = .TRUE.
             RETURN
          END IF
       END IF
    END DO
  END SUBROUTINE find_arg
  !------------------------------------------------------------------------
  !  Argument groups
  !------------------------------------------------------------------------
  SUBROUTINE arg_group(name)
!!! Defines an argument group.
    CHARACTER(LEN=*), INTENT(IN   )            :: name
!!! Group name
    CHARACTER(LEN=256), POINTER, DIMENSION(:)  :: temp_g
    INTEGER,            POINTER, DIMENSION(:)  :: temp_s
    INTEGER,            POINTER, DIMENSION(:)  :: temp_m
    LOGICAL,            POINTER, DIMENSION(:)  :: temp_c
    LOGICAL,            POINTER, DIMENSION(:)  :: temp_a
    groups_i = groups_i + 1
    IF (.NOT. ASSOCIATED(groups)) THEN
       ALLOCATE(groups(0:di))
       ALLOCATE(group_size(0:di))
       ALLOCATE(group_max_len(0:di))
       ALLOCATE(group_has_ctrl(0:di))
       ALLOCATE(group_has_arg(0:di))
    ELSE IF (groups_i+1 .GT. SIZE(groups)) THEN
       ALLOCATE(temp_g(0:(SIZE(groups)+di)))
       ALLOCATE(temp_s(0:(SIZE(groups)+di)))
       ALLOCATE(temp_m(0:(SIZE(groups)+di)))
       ALLOCATE(temp_c(0:(SIZE(groups)+di)))
       ALLOCATE(temp_a(0:(SIZE(groups)+di)))
       temp_g(0:SIZE(groups)) = groups(0:SIZE(groups))
       temp_s(0:SIZE(groups)) = group_size(0:SIZE(groups))
       temp_m(0:SIZE(groups)) = group_max_len(0:SIZE(groups))
       temp_c(0:SIZE(groups)) = group_has_ctrl(0:SIZE(groups))
       temp_a(0:SIZE(groups)) = group_has_arg(0:SIZE(groups))
       DEALLOCATE(groups)
       DEALLOCATE(group_size)
       DEALLOCATE(group_max_len)
       DEALLOCATE(group_has_ctrl)
       DEALLOCATE(group_has_arg)
       groups         => temp_g
       group_size     => temp_s
       group_max_len  => temp_m
       group_has_ctrl => temp_c
       group_has_arg  => temp_a
    END IF
    groups(groups_i)         = name
    group_size(groups_i)     = 0
    group_max_len(groups_i)  = 0
    group_has_ctrl(groups_i) = .false.
    group_has_arg(groups_i)  = .false.
    IF (groups_i .EQ. 0 .AND. (help_enabled .OR. ctrl_enabled)) THEN
       group_has_arg(0) = .true.
    END IF
  END SUBROUTINE arg_group
  !------------------------------------------------------------------------
  !  Help string break
  !------------------------------------------------------------------------
  SUBROUTINE break_help(ht, w, prefix, unit, skip_first_line)
    ! args
    CHARACTER(LEN=*),  INTENT(IN   ) :: ht
    INTEGER,           INTENT(IN   ) :: w
    CHARACTER(LEN=*),  INTENT(IN   ) :: prefix
    INTEGER,           INTENT(IN   ) :: unit
    LOGICAL, OPTIONAL, INTENT(IN   ) :: skip_first_line
    ! vars
    INTEGER :: start
    INTEGER :: break
    INTEGER :: next
    INTEGER :: end
    INTEGER :: skip
    LOGICAL :: fl
    IF (.NOT. PRESENT(skip_first_line)) THEN
       fl = .TRUE.
    ELSE
       fl = skip_first_line
    END IF
    start = 1
    break = 1
    next  = 1
    end   = LEN(ht)
    ! line loop
    DO WHILE (start .LT. end)
       ! break < start + w < next
       DO WHILE (next .LT. end .AND. next .LT. start+w)
          ! find next blank or new line
          skip = 1
          DO WHILE (next .LT. end .AND. ht(next:next) .NE. ' ')
             IF (next .LT. end) THEN
                IF (ht(next:next+1) .EQ. '\n') THEN
                   ! line break
                   skip = 3
                   break = next - 1
                   next = next + 2
                   GOTO 1234
                END IF
             END IF
             next = next + 1
          END DO
          break = next
          next = next + 1
       END DO
       ! print line
1234   IF (fl) THEN
          fl = .FALSE.
       ELSE
          WRITE(unit,'(A)',advance='no') prefix
       END IF
       WRITE (unit,'(A)') ht(start:break)
       ! itterate
       start = break + skip
       break = next
    END DO
  END SUBROUTINE break_help
  !------------------------------------------------------------------------
  !  Print command line help
  !------------------------------------------------------------------------
  SUBROUTINE print_help
!!! Prints usage information to the standard output.
    INTEGER  :: i, j, k, l
    CHARACTER(LEN=ppm_char) :: scratch
    IF (ctrl_enabled) THEN
       WRITE (*,'(A)') "Usage: progname {ctrl-file} [options] [args]"
    ELSE
       WRITE (*,'(A)') "Usage: progname [options] [args]"
    END IF
    DO k=0,groups_i
       IF (.NOT. group_has_arg(k)) CYCLE
       WRITE (*,'(/A)') groups(k)
       IF (k .EQ. 0) THEN
          IF (help_enabled) THEN
             WRITE (*,'(A,/A,/A)') &
'  help                          Print this help message and exit.',&
'                  short flag :  -h',&
'                   long flag :  --help'
             WRITE (*,*) ""
          END IF
          IF (ctrl_enabled) THEN
             WRITE (*,'(A,/A)') &
'  control file                  Print sample control file and exit.',&
'                   long flag :  --print-ctrl'
             WRITE (*,*) ""
          END IF
       END IF
       group_loop: DO i=1,group_size(k)
          ! scalar
       DO j=1,INTEGER_args_i
          IF (INTEGER_args(j)%group .EQ. k   .AND. &
              INTEGER_args(j)%group_i .EQ. i .AND. &
             (INTEGER_args(j)%flag_set .OR.        &
              INTEGER_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", INTEGER_args(j)%name(1:28)
             IF (INTEGER_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(INTEGER_args(j)%help     &
                     (1:LEN_TRIM(INTEGER_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (INTEGER_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  INTEGER_args(j)%flag(1:2), " <value>"
             IF (INTEGER_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  INTEGER_args(j)%long_flag(1:(LEN_TRIM(INTEGER_args(j)%long_flag))), &
                  " <value>"
             IF (INTEGER_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  INTEGER_args(j)%ctrl_name(1:LEN_TRIM(INTEGER_args(j)%ctrl_name)), &
                  " = <value>"
             IF (INTEGER_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) INTEGER_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (INTEGER_args(j)%min_set) THEN
                WRITE(scratch, *) INTEGER_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (INTEGER_args(j)%max_set) THEN
                WRITE(scratch, *) INTEGER_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,LONGINT_args_i
          IF (LONGINT_args(j)%group .EQ. k   .AND. &
              LONGINT_args(j)%group_i .EQ. i .AND. &
             (LONGINT_args(j)%flag_set .OR.        &
              LONGINT_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", LONGINT_args(j)%name(1:28)
             IF (LONGINT_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(LONGINT_args(j)%help     &
                     (1:LEN_TRIM(LONGINT_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (LONGINT_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  LONGINT_args(j)%flag(1:2), " <value>"
             IF (LONGINT_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  LONGINT_args(j)%long_flag(1:(LEN_TRIM(LONGINT_args(j)%long_flag))), &
                  " <value>"
             IF (LONGINT_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  LONGINT_args(j)%ctrl_name(1:LEN_TRIM(LONGINT_args(j)%ctrl_name)), &
                  " = <value>"
             IF (LONGINT_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) LONGINT_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (LONGINT_args(j)%min_set) THEN
                WRITE(scratch, *) LONGINT_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (LONGINT_args(j)%max_set) THEN
                WRITE(scratch, *) LONGINT_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,SINGLE_args_i
          IF (SINGLE_args(j)%group .EQ. k   .AND. &
              SINGLE_args(j)%group_i .EQ. i .AND. &
             (SINGLE_args(j)%flag_set .OR.        &
              SINGLE_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", SINGLE_args(j)%name(1:28)
             IF (SINGLE_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(SINGLE_args(j)%help     &
                     (1:LEN_TRIM(SINGLE_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (SINGLE_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  SINGLE_args(j)%flag(1:2), " <value>"
             IF (SINGLE_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  SINGLE_args(j)%long_flag(1:(LEN_TRIM(SINGLE_args(j)%long_flag))), &
                  " <value>"
             IF (SINGLE_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  SINGLE_args(j)%ctrl_name(1:LEN_TRIM(SINGLE_args(j)%ctrl_name)), &
                  " = <value>"
             IF (SINGLE_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) SINGLE_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (SINGLE_args(j)%min_set) THEN
                WRITE(scratch, *) SINGLE_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (SINGLE_args(j)%max_set) THEN
                WRITE(scratch, *) SINGLE_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,DOUBLE_args_i
          IF (DOUBLE_args(j)%group .EQ. k   .AND. &
              DOUBLE_args(j)%group_i .EQ. i .AND. &
             (DOUBLE_args(j)%flag_set .OR.        &
              DOUBLE_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", DOUBLE_args(j)%name(1:28)
             IF (DOUBLE_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(DOUBLE_args(j)%help     &
                     (1:LEN_TRIM(DOUBLE_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (DOUBLE_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  DOUBLE_args(j)%flag(1:2), " <value>"
             IF (DOUBLE_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  DOUBLE_args(j)%long_flag(1:(LEN_TRIM(DOUBLE_args(j)%long_flag))), &
                  " <value>"
             IF (DOUBLE_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  DOUBLE_args(j)%ctrl_name(1:LEN_TRIM(DOUBLE_args(j)%ctrl_name)), &
                  " = <value>"
             IF (DOUBLE_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) DOUBLE_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (DOUBLE_args(j)%min_set) THEN
                WRITE(scratch, *) DOUBLE_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (DOUBLE_args(j)%max_set) THEN
                WRITE(scratch, *) DOUBLE_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,LOGICAL_args_i
          IF (LOGICAL_args(j)%group .EQ. k   .AND. &
              LOGICAL_args(j)%group_i .EQ. i .AND. &
             (LOGICAL_args(j)%flag_set .OR.        &
              LOGICAL_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", LOGICAL_args(j)%name(1:28)
             IF (LOGICAL_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(LOGICAL_args(j)%help     &
                     (1:LEN_TRIM(LOGICAL_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (LOGICAL_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  LOGICAL_args(j)%flag(1:2)
             IF (LOGICAL_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  LOGICAL_args(j)%long_flag(1:(LEN_TRIM(LOGICAL_args(j)%long_flag)))
             IF (LOGICAL_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  LOGICAL_args(j)%ctrl_name(1:LEN_TRIM(LOGICAL_args(j)%ctrl_name)), &
                  " = <value>"
             IF (LOGICAL_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) LOGICAL_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,STRING_args_i
          IF (STRING_args(j)%group .EQ. k   .AND. &
              STRING_args(j)%group_i .EQ. i .AND. &
             (STRING_args(j)%flag_set .OR.        &
              STRING_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", STRING_args(j)%name(1:28)
             IF (STRING_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(STRING_args(j)%help     &
                     (1:LEN_TRIM(STRING_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (STRING_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  STRING_args(j)%flag(1:2), " <value>"
             IF (STRING_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  STRING_args(j)%long_flag(1:(LEN_TRIM(STRING_args(j)%long_flag))), &
                  " <value>"
             IF (STRING_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  STRING_args(j)%ctrl_name(1:LEN_TRIM(STRING_args(j)%ctrl_name)), &
                  " = <value>"
             IF (STRING_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE (*,*) STRING_args(j)%default &
                     (1:LEN_TRIM(STRING_args(j)%default))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,COMPLEX_args_i
          IF (COMPLEX_args(j)%group .EQ. k   .AND. &
              COMPLEX_args(j)%group_i .EQ. i .AND. &
             (COMPLEX_args(j)%flag_set .OR.        &
              COMPLEX_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", COMPLEX_args(j)%name(1:28)
             IF (COMPLEX_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(COMPLEX_args(j)%help     &
                     (1:LEN_TRIM(COMPLEX_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (COMPLEX_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  COMPLEX_args(j)%flag(1:2), " <value>"
             IF (COMPLEX_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  COMPLEX_args(j)%long_flag(1:(LEN_TRIM(COMPLEX_args(j)%long_flag))), &
                  " <value>"
             IF (COMPLEX_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  COMPLEX_args(j)%ctrl_name(1:LEN_TRIM(COMPLEX_args(j)%ctrl_name)), &
                  " = <value>"
             IF (COMPLEX_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) COMPLEX_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_args_i
          IF (DCOMPLEX_args(j)%group .EQ. k   .AND. &
              DCOMPLEX_args(j)%group_i .EQ. i .AND. &
             (DCOMPLEX_args(j)%flag_set .OR.        &
              DCOMPLEX_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", DCOMPLEX_args(j)%name(1:28)
             IF (DCOMPLEX_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(DCOMPLEX_args(j)%help     &
                     (1:LEN_TRIM(DCOMPLEX_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (DCOMPLEX_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  DCOMPLEX_args(j)%flag(1:2), " <value>"
             IF (DCOMPLEX_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  DCOMPLEX_args(j)%long_flag(1:(LEN_TRIM(DCOMPLEX_args(j)%long_flag))), &
                  " <value>"
             IF (DCOMPLEX_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  DCOMPLEX_args(j)%ctrl_name(1:LEN_TRIM(DCOMPLEX_args(j)%ctrl_name)), &
                  " = <value>"
             IF (DCOMPLEX_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) DCOMPLEX_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
          ! array
       DO j=1,INTEGER_array_args_i
          IF (INTEGER_array_args(j)%group .EQ. k   .AND. &
              INTEGER_array_args(j)%group_i .EQ. i .AND. &
             (INTEGER_array_args(j)%flag_set .OR.        &
              INTEGER_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", INTEGER_array_args(j)%name(1:28)
             IF (INTEGER_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(INTEGER_array_args(j)%help     &
                     (1:LEN_TRIM(INTEGER_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (INTEGER_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  INTEGER_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (INTEGER_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  INTEGER_array_args(j)%long_flag(1:(LEN_TRIM(INTEGER_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (INTEGER_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  INTEGER_array_args(j)%ctrl_name(1:LEN_TRIM(INTEGER_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (INTEGER_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) INTEGER_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (INTEGER_array_args(j)%min_set) THEN
                WRITE(scratch, *) INTEGER_array_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (INTEGER_array_args(j)%max_set) THEN
                WRITE(scratch, *) INTEGER_array_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,LONGINT_array_args_i
          IF (LONGINT_array_args(j)%group .EQ. k   .AND. &
              LONGINT_array_args(j)%group_i .EQ. i .AND. &
             (LONGINT_array_args(j)%flag_set .OR.        &
              LONGINT_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", LONGINT_array_args(j)%name(1:28)
             IF (LONGINT_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(LONGINT_array_args(j)%help     &
                     (1:LEN_TRIM(LONGINT_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (LONGINT_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  LONGINT_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (LONGINT_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  LONGINT_array_args(j)%long_flag(1:(LEN_TRIM(LONGINT_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (LONGINT_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  LONGINT_array_args(j)%ctrl_name(1:LEN_TRIM(LONGINT_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (LONGINT_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) LONGINT_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (LONGINT_array_args(j)%min_set) THEN
                WRITE(scratch, *) LONGINT_array_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (LONGINT_array_args(j)%max_set) THEN
                WRITE(scratch, *) LONGINT_array_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,SINGLE_array_args_i
          IF (SINGLE_array_args(j)%group .EQ. k   .AND. &
              SINGLE_array_args(j)%group_i .EQ. i .AND. &
             (SINGLE_array_args(j)%flag_set .OR.        &
              SINGLE_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", SINGLE_array_args(j)%name(1:28)
             IF (SINGLE_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(SINGLE_array_args(j)%help     &
                     (1:LEN_TRIM(SINGLE_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (SINGLE_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  SINGLE_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (SINGLE_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  SINGLE_array_args(j)%long_flag(1:(LEN_TRIM(SINGLE_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (SINGLE_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  SINGLE_array_args(j)%ctrl_name(1:LEN_TRIM(SINGLE_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (SINGLE_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) SINGLE_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (SINGLE_array_args(j)%min_set) THEN
                WRITE(scratch, *) SINGLE_array_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (SINGLE_array_args(j)%max_set) THEN
                WRITE(scratch, *) SINGLE_array_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,DOUBLE_array_args_i
          IF (DOUBLE_array_args(j)%group .EQ. k   .AND. &
              DOUBLE_array_args(j)%group_i .EQ. i .AND. &
             (DOUBLE_array_args(j)%flag_set .OR.        &
              DOUBLE_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", DOUBLE_array_args(j)%name(1:28)
             IF (DOUBLE_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(DOUBLE_array_args(j)%help     &
                     (1:LEN_TRIM(DOUBLE_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (DOUBLE_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  DOUBLE_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (DOUBLE_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  DOUBLE_array_args(j)%long_flag(1:(LEN_TRIM(DOUBLE_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (DOUBLE_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  DOUBLE_array_args(j)%ctrl_name(1:LEN_TRIM(DOUBLE_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (DOUBLE_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) DOUBLE_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             IF (DOUBLE_array_args(j)%min_set) THEN
                WRITE(scratch, *) DOUBLE_array_args(j)%min
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               minimum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             IF (DOUBLE_array_args(j)%max_set) THEN
                WRITE(scratch, *) DOUBLE_array_args(j)%max
                scratch = ADJUSTL(scratch)
                WRITE (*,*) "               maximum value :  ", &
                     scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,LOGICAL_array_args_i
          IF (LOGICAL_array_args(j)%group .EQ. k   .AND. &
              LOGICAL_array_args(j)%group_i .EQ. i .AND. &
             (LOGICAL_array_args(j)%flag_set .OR.        &
              LOGICAL_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", LOGICAL_array_args(j)%name(1:28)
             IF (LOGICAL_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(LOGICAL_array_args(j)%help     &
                     (1:LEN_TRIM(LOGICAL_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (LOGICAL_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  LOGICAL_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (LOGICAL_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  LOGICAL_array_args(j)%long_flag(1:(LEN_TRIM(LOGICAL_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (LOGICAL_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  LOGICAL_array_args(j)%ctrl_name(1:LEN_TRIM(LOGICAL_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (LOGICAL_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) LOGICAL_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,STRING_array_args_i
          IF (STRING_array_args(j)%group .EQ. k   .AND. &
              STRING_array_args(j)%group_i .EQ. i .AND. &
             (STRING_array_args(j)%flag_set .OR.        &
              STRING_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", STRING_array_args(j)%name(1:28)
             IF (STRING_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(STRING_array_args(j)%help     &
                     (1:LEN_TRIM(STRING_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (STRING_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  STRING_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (STRING_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  STRING_array_args(j)%long_flag(1:(LEN_TRIM(STRING_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (STRING_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  STRING_array_args(j)%ctrl_name(1:LEN_TRIM(STRING_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (STRING_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE (*,'(A)',advance='no') ' '
                DO l=LBOUND(STRING_array_args(j)%default,1), &
                     UBOUND(STRING_array_args(j)%default,1)
                   WRITE (*,'(2A)',advance='no') &
                        STRING_array_args(j)%default(l) &
                        (1:LEN_TRIM(STRING_array_args(j)%default(l))), &
                        ', '
                END DO
                WRITE (*,'(A)') ''
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,COMPLEX_array_args_i
          IF (COMPLEX_array_args(j)%group .EQ. k   .AND. &
              COMPLEX_array_args(j)%group_i .EQ. i .AND. &
             (COMPLEX_array_args(j)%flag_set .OR.        &
              COMPLEX_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", COMPLEX_array_args(j)%name(1:28)
             IF (COMPLEX_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(COMPLEX_array_args(j)%help     &
                     (1:LEN_TRIM(COMPLEX_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (COMPLEX_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  COMPLEX_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (COMPLEX_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  COMPLEX_array_args(j)%long_flag(1:(LEN_TRIM(COMPLEX_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (COMPLEX_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  COMPLEX_array_args(j)%ctrl_name(1:LEN_TRIM(COMPLEX_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (COMPLEX_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) COMPLEX_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_array_args_i
          IF (DCOMPLEX_array_args(j)%group .EQ. k   .AND. &
              DCOMPLEX_array_args(j)%group_i .EQ. i .AND. &
             (DCOMPLEX_array_args(j)%flag_set .OR.        &
              DCOMPLEX_array_args(j)%long_flag_set)) THEN
             WRITE (*,'(A,A)',advance='no') "  ", DCOMPLEX_array_args(j)%name(1:28)
             IF (DCOMPLEX_array_args(j)%help_set) THEN
                WRITE (*,'(A)',advance='no') "  "
                CALL break_help(DCOMPLEX_array_args(j)%help     &
                     (1:LEN_TRIM(DCOMPLEX_array_args(j)%help)), &
                     50, "                                ", 6)
             ELSE
                WRITE (*,*) ''
             END IF
             IF (DCOMPLEX_array_args(j)%flag_set) &
                  WRITE (*,*) "                  short flag :  ", &
                  DCOMPLEX_array_args(j)%flag(1:2), " <v1,v2,...>"
             IF (DCOMPLEX_array_args(j)%long_flag_set) &
                  WRITE (*,*) "                   long flag :  ", &
                  DCOMPLEX_array_args(j)%long_flag(1:(LEN_TRIM(DCOMPLEX_array_args(j)%long_flag))), &
                  " <v1,v2,...>"
             IF (DCOMPLEX_array_args(j)%ctrl_name_set) &
                  WRITE (*,*) "           control file name :  ", &
                  DCOMPLEX_array_args(j)%ctrl_name(1:LEN_TRIM(DCOMPLEX_array_args(j)%ctrl_name)), &
                  " = <v1,v2,...>"
             IF (DCOMPLEX_array_args(j)%default_set) THEN
                WRITE (*,'(A)',advance='no') "                default value : "
                WRITE(scratch, *) DCOMPLEX_array_args(j)%default
                scratch = ADJUSTL(scratch)
                WRITE(*,*) scratch(1:LEN_TRIM(scratch))
             END IF
             WRITE(*,*) ""
             CYCLE group_loop
          END IF
       END DO
       END DO group_loop
    END DO
  END SUBROUTINE print_help
  !------------------------------------------------------------------------
  !  Print sample control file
  !------------------------------------------------------------------------
  SUBROUTINE print_ctrl
!!! Prints a sample control file to standard output.
    INTEGER  :: i, j, k, l
    CHARACTER(LEN=ppm_char) :: scratch
    LOGICAL                 :: in_line
    IF (groups_i .EQ. -1) THEN
       WRITE (*,*) "No args have been defined..."
       RETURN
    END IF
    WRITE (*,'(A)') "#-------------------------------------------------------------------------------"
    WRITE (*,'(A)') "#  Sample control file for ppm_client"
    WRITE (*,'(A)') "#"
    WRITE (*,'(A)') "#  Edit the settings below"
    WRITE (*,'(A)') "#"
    WRITE (*,'(A)') "#-------------------------------------------------------------------------------"
    DO k=0,groups_i
       IF (.NOT. group_has_ctrl(k)) CYCLE
       WRITE (*,'(/A)') "#-------------------------------------------------------------------------------"
       WRITE (*,'(2A)') "#  ", groups(k)(1:LEN_TRIM(groups(k)))

       comment_loop: DO i=1,group_size(k)
          ! scalar
       DO j=1,INTEGER_args_i

          IF (INTEGER_args(j)%group   .EQ. k .AND. &
              INTEGER_args(j)%group_i .EQ. i) THEN

             IF (INTEGER_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     INTEGER_args(j)%name(1:LEN_TRIM(INTEGER_args(j)%name))
                in_line = .TRUE.

                IF (INTEGER_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(INTEGER_args(j)%help     &
                        (1:LEN_TRIM(INTEGER_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (INTEGER_args(j)%flag_set .OR. &
                     INTEGER_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (INTEGER_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           INTEGER_args(j)%flag(1:2), " {value}"
                      IF (INTEGER_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (INTEGER_args(j)%long_flag_set) &
                        WRITE(*,*) INTEGER_args(j)%long_flag &
                        (1:(LEN_TRIM(INTEGER_args(j)%long_flag))), " {value}"
                END IF

                IF (INTEGER_args(j)%min_set .OR. &
                     INTEGER_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (INTEGER_args(j)%min_set) THEN
                      WRITE (scratch, *) INTEGER_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{value}'
                   IF (INTEGER_args(j)%max_set) THEN
                      WRITE (scratch, *) INTEGER_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,LONGINT_args_i

          IF (LONGINT_args(j)%group   .EQ. k .AND. &
              LONGINT_args(j)%group_i .EQ. i) THEN

             IF (LONGINT_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     LONGINT_args(j)%name(1:LEN_TRIM(LONGINT_args(j)%name))
                in_line = .TRUE.

                IF (LONGINT_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(LONGINT_args(j)%help     &
                        (1:LEN_TRIM(LONGINT_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (LONGINT_args(j)%flag_set .OR. &
                     LONGINT_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LONGINT_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           LONGINT_args(j)%flag(1:2), " {value}"
                      IF (LONGINT_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (LONGINT_args(j)%long_flag_set) &
                        WRITE(*,*) LONGINT_args(j)%long_flag &
                        (1:(LEN_TRIM(LONGINT_args(j)%long_flag))), " {value}"
                END IF

                IF (LONGINT_args(j)%min_set .OR. &
                     LONGINT_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LONGINT_args(j)%min_set) THEN
                      WRITE (scratch, *) LONGINT_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{value}'
                   IF (LONGINT_args(j)%max_set) THEN
                      WRITE (scratch, *) LONGINT_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,SINGLE_args_i

          IF (SINGLE_args(j)%group   .EQ. k .AND. &
              SINGLE_args(j)%group_i .EQ. i) THEN

             IF (SINGLE_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     SINGLE_args(j)%name(1:LEN_TRIM(SINGLE_args(j)%name))
                in_line = .TRUE.

                IF (SINGLE_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(SINGLE_args(j)%help     &
                        (1:LEN_TRIM(SINGLE_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (SINGLE_args(j)%flag_set .OR. &
                     SINGLE_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (SINGLE_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           SINGLE_args(j)%flag(1:2), " {value}"
                      IF (SINGLE_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (SINGLE_args(j)%long_flag_set) &
                        WRITE(*,*) SINGLE_args(j)%long_flag &
                        (1:(LEN_TRIM(SINGLE_args(j)%long_flag))), " {value}"
                END IF

                IF (SINGLE_args(j)%min_set .OR. &
                     SINGLE_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (SINGLE_args(j)%min_set) THEN
                      WRITE (scratch, *) SINGLE_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{value}'
                   IF (SINGLE_args(j)%max_set) THEN
                      WRITE (scratch, *) SINGLE_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,DOUBLE_args_i

          IF (DOUBLE_args(j)%group   .EQ. k .AND. &
              DOUBLE_args(j)%group_i .EQ. i) THEN

             IF (DOUBLE_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     DOUBLE_args(j)%name(1:LEN_TRIM(DOUBLE_args(j)%name))
                in_line = .TRUE.

                IF (DOUBLE_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(DOUBLE_args(j)%help     &
                        (1:LEN_TRIM(DOUBLE_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (DOUBLE_args(j)%flag_set .OR. &
                     DOUBLE_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DOUBLE_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           DOUBLE_args(j)%flag(1:2), " {value}"
                      IF (DOUBLE_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (DOUBLE_args(j)%long_flag_set) &
                        WRITE(*,*) DOUBLE_args(j)%long_flag &
                        (1:(LEN_TRIM(DOUBLE_args(j)%long_flag))), " {value}"
                END IF

                IF (DOUBLE_args(j)%min_set .OR. &
                     DOUBLE_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DOUBLE_args(j)%min_set) THEN
                      WRITE (scratch, *) DOUBLE_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{value}'
                   IF (DOUBLE_args(j)%max_set) THEN
                      WRITE (scratch, *) DOUBLE_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,LOGICAL_args_i

          IF (LOGICAL_args(j)%group   .EQ. k .AND. &
              LOGICAL_args(j)%group_i .EQ. i) THEN

             IF (LOGICAL_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     LOGICAL_args(j)%name(1:LEN_TRIM(LOGICAL_args(j)%name))
                in_line = .TRUE.

                IF (LOGICAL_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(LOGICAL_args(j)%help     &
                        (1:LEN_TRIM(LOGICAL_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (LOGICAL_args(j)%flag_set .OR. &
                     LOGICAL_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LOGICAL_args(j)%flag_set) THEN
                      WRITE(*,'(A)',advance='no') &
                           LOGICAL_args(j)%flag(1:2)
                      IF (LOGICAL_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (LOGICAL_args(j)%long_flag_set) &
                        WRITE(*,*) LOGICAL_args(j)%long_flag &
                        (1:(LEN_TRIM(LOGICAL_args(j)%long_flag)))
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,STRING_args_i

          IF (STRING_args(j)%group   .EQ. k .AND. &
              STRING_args(j)%group_i .EQ. i) THEN

             IF (STRING_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     STRING_args(j)%name(1:LEN_TRIM(STRING_args(j)%name))
                in_line = .TRUE.

                IF (STRING_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(STRING_args(j)%help     &
                        (1:LEN_TRIM(STRING_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (STRING_args(j)%flag_set .OR. &
                     STRING_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (STRING_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           STRING_args(j)%flag(1:2), " {value}"
                      IF (STRING_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (STRING_args(j)%long_flag_set) &
                        WRITE(*,*) STRING_args(j)%long_flag &
                        (1:(LEN_TRIM(STRING_args(j)%long_flag))), " {value}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,COMPLEX_args_i

          IF (COMPLEX_args(j)%group   .EQ. k .AND. &
              COMPLEX_args(j)%group_i .EQ. i) THEN

             IF (COMPLEX_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     COMPLEX_args(j)%name(1:LEN_TRIM(COMPLEX_args(j)%name))
                in_line = .TRUE.

                IF (COMPLEX_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(COMPLEX_args(j)%help     &
                        (1:LEN_TRIM(COMPLEX_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (COMPLEX_args(j)%flag_set .OR. &
                     COMPLEX_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (COMPLEX_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           COMPLEX_args(j)%flag(1:2), " {value}"
                      IF (COMPLEX_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (COMPLEX_args(j)%long_flag_set) &
                        WRITE(*,*) COMPLEX_args(j)%long_flag &
                        (1:(LEN_TRIM(COMPLEX_args(j)%long_flag))), " {value}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_args_i

          IF (DCOMPLEX_args(j)%group   .EQ. k .AND. &
              DCOMPLEX_args(j)%group_i .EQ. i) THEN

             IF (DCOMPLEX_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     DCOMPLEX_args(j)%name(1:LEN_TRIM(DCOMPLEX_args(j)%name))
                in_line = .TRUE.

                IF (DCOMPLEX_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(DCOMPLEX_args(j)%help     &
                        (1:LEN_TRIM(DCOMPLEX_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (DCOMPLEX_args(j)%flag_set .OR. &
                     DCOMPLEX_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DCOMPLEX_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           DCOMPLEX_args(j)%flag(1:2), " {value}"
                      IF (DCOMPLEX_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (DCOMPLEX_args(j)%long_flag_set) &
                        WRITE(*,*) DCOMPLEX_args(j)%long_flag &
                        (1:(LEN_TRIM(DCOMPLEX_args(j)%long_flag))), " {value}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
          ! array
       DO j=1,INTEGER_array_args_i

          IF (INTEGER_array_args(j)%group   .EQ. k .AND. &
              INTEGER_array_args(j)%group_i .EQ. i) THEN

             IF (INTEGER_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     INTEGER_array_args(j)%name(1:LEN_TRIM(INTEGER_array_args(j)%name))
                in_line = .TRUE.

                IF (INTEGER_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(INTEGER_array_args(j)%help     &
                        (1:LEN_TRIM(INTEGER_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (INTEGER_array_args(j)%flag_set .OR. &
                     INTEGER_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (INTEGER_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           INTEGER_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (INTEGER_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (INTEGER_array_args(j)%long_flag_set) &
                        WRITE(*,*) INTEGER_array_args(j)%long_flag &
                        (1:(LEN_TRIM(INTEGER_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (INTEGER_array_args(j)%min_set .OR. &
                     INTEGER_array_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (INTEGER_array_args(j)%min_set) THEN
                      WRITE (scratch, *) INTEGER_array_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{v1,v2,...}'
                   IF (INTEGER_array_args(j)%max_set) THEN
                      WRITE (scratch, *) INTEGER_array_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,LONGINT_array_args_i

          IF (LONGINT_array_args(j)%group   .EQ. k .AND. &
              LONGINT_array_args(j)%group_i .EQ. i) THEN

             IF (LONGINT_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     LONGINT_array_args(j)%name(1:LEN_TRIM(LONGINT_array_args(j)%name))
                in_line = .TRUE.

                IF (LONGINT_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(LONGINT_array_args(j)%help     &
                        (1:LEN_TRIM(LONGINT_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (LONGINT_array_args(j)%flag_set .OR. &
                     LONGINT_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LONGINT_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           LONGINT_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (LONGINT_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (LONGINT_array_args(j)%long_flag_set) &
                        WRITE(*,*) LONGINT_array_args(j)%long_flag &
                        (1:(LEN_TRIM(LONGINT_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (LONGINT_array_args(j)%min_set .OR. &
                     LONGINT_array_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LONGINT_array_args(j)%min_set) THEN
                      WRITE (scratch, *) LONGINT_array_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{v1,v2,...}'
                   IF (LONGINT_array_args(j)%max_set) THEN
                      WRITE (scratch, *) LONGINT_array_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,SINGLE_array_args_i

          IF (SINGLE_array_args(j)%group   .EQ. k .AND. &
              SINGLE_array_args(j)%group_i .EQ. i) THEN

             IF (SINGLE_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     SINGLE_array_args(j)%name(1:LEN_TRIM(SINGLE_array_args(j)%name))
                in_line = .TRUE.

                IF (SINGLE_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(SINGLE_array_args(j)%help     &
                        (1:LEN_TRIM(SINGLE_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (SINGLE_array_args(j)%flag_set .OR. &
                     SINGLE_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (SINGLE_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           SINGLE_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (SINGLE_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (SINGLE_array_args(j)%long_flag_set) &
                        WRITE(*,*) SINGLE_array_args(j)%long_flag &
                        (1:(LEN_TRIM(SINGLE_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (SINGLE_array_args(j)%min_set .OR. &
                     SINGLE_array_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (SINGLE_array_args(j)%min_set) THEN
                      WRITE (scratch, *) SINGLE_array_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{v1,v2,...}'
                   IF (SINGLE_array_args(j)%max_set) THEN
                      WRITE (scratch, *) SINGLE_array_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,DOUBLE_array_args_i

          IF (DOUBLE_array_args(j)%group   .EQ. k .AND. &
              DOUBLE_array_args(j)%group_i .EQ. i) THEN

             IF (DOUBLE_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     DOUBLE_array_args(j)%name(1:LEN_TRIM(DOUBLE_array_args(j)%name))
                in_line = .TRUE.

                IF (DOUBLE_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(DOUBLE_array_args(j)%help     &
                        (1:LEN_TRIM(DOUBLE_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (DOUBLE_array_args(j)%flag_set .OR. &
                     DOUBLE_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DOUBLE_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           DOUBLE_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (DOUBLE_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (DOUBLE_array_args(j)%long_flag_set) &
                        WRITE(*,*) DOUBLE_array_args(j)%long_flag &
                        (1:(LEN_TRIM(DOUBLE_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (DOUBLE_array_args(j)%min_set .OR. &
                     DOUBLE_array_args(j)%max_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DOUBLE_array_args(j)%min_set) THEN
                      WRITE (scratch, *) DOUBLE_array_args(j)%min
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') scratch(1:LEN_TRIM(scratch)) , ' <= '
                   END IF
                   WRITE (*,'(A)',advance='no') '{v1,v2,...}'
                   IF (DOUBLE_array_args(j)%max_set) THEN
                      WRITE (scratch, *) DOUBLE_array_args(j)%max
                      scratch = ADJUSTL(scratch)
                      WRITE (*,'(A,A)',advance='no') ' <= ', scratch(1:LEN_TRIM(scratch))
                   END IF
                   WRITE(*,*) ''
                END IF
                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,LOGICAL_array_args_i

          IF (LOGICAL_array_args(j)%group   .EQ. k .AND. &
              LOGICAL_array_args(j)%group_i .EQ. i) THEN

             IF (LOGICAL_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     LOGICAL_array_args(j)%name(1:LEN_TRIM(LOGICAL_array_args(j)%name))
                in_line = .TRUE.

                IF (LOGICAL_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(LOGICAL_array_args(j)%help     &
                        (1:LEN_TRIM(LOGICAL_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (LOGICAL_array_args(j)%flag_set .OR. &
                     LOGICAL_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (LOGICAL_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           LOGICAL_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (LOGICAL_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (LOGICAL_array_args(j)%long_flag_set) &
                        WRITE(*,*) LOGICAL_array_args(j)%long_flag &
                        (1:(LEN_TRIM(LOGICAL_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,STRING_array_args_i

          IF (STRING_array_args(j)%group   .EQ. k .AND. &
              STRING_array_args(j)%group_i .EQ. i) THEN

             IF (STRING_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     STRING_array_args(j)%name(1:LEN_TRIM(STRING_array_args(j)%name))
                in_line = .TRUE.

                IF (STRING_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(STRING_array_args(j)%help     &
                        (1:LEN_TRIM(STRING_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (STRING_array_args(j)%flag_set .OR. &
                     STRING_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (STRING_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           STRING_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (STRING_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (STRING_array_args(j)%long_flag_set) &
                        WRITE(*,*) STRING_array_args(j)%long_flag &
                        (1:(LEN_TRIM(STRING_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,COMPLEX_array_args_i

          IF (COMPLEX_array_args(j)%group   .EQ. k .AND. &
              COMPLEX_array_args(j)%group_i .EQ. i) THEN

             IF (COMPLEX_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     COMPLEX_array_args(j)%name(1:LEN_TRIM(COMPLEX_array_args(j)%name))
                in_line = .TRUE.

                IF (COMPLEX_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(COMPLEX_array_args(j)%help     &
                        (1:LEN_TRIM(COMPLEX_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (COMPLEX_array_args(j)%flag_set .OR. &
                     COMPLEX_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (COMPLEX_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           COMPLEX_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (COMPLEX_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (COMPLEX_array_args(j)%long_flag_set) &
                        WRITE(*,*) COMPLEX_array_args(j)%long_flag &
                        (1:(LEN_TRIM(COMPLEX_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_array_args_i

          IF (DCOMPLEX_array_args(j)%group   .EQ. k .AND. &
              DCOMPLEX_array_args(j)%group_i .EQ. i) THEN

             IF (DCOMPLEX_array_args(j)%ctrl_name_set) THEN

                WRITE (*,'(A,A15)',advance='no') "#  ", &
                     DCOMPLEX_array_args(j)%name(1:LEN_TRIM(DCOMPLEX_array_args(j)%name))
                in_line = .TRUE.

                IF (DCOMPLEX_array_args(j)%help_set) THEN
                   WRITE (*,'(A)',advance='no') ": "
                   CALL break_help(DCOMPLEX_array_args(j)%help     &
                        (1:LEN_TRIM(DCOMPLEX_array_args(j)%help)), &
                        50, "#                   ", 6)
                   in_line = .FALSE.
                END IF

                IF (DCOMPLEX_array_args(j)%flag_set .OR. &
                     DCOMPLEX_array_args(j)%long_flag_set) THEN
                   IF (in_line) THEN
                      WRITE (*,'(A)',advance='no') ": "
                   ELSE
                      WRITE (*,'(A)',advance='no') "#                   "
                   END IF
                   in_line = .FALSE.
                   IF (DCOMPLEX_array_args(j)%flag_set) THEN
                      WRITE(*,'(A,A)',advance='no') &
                           DCOMPLEX_array_args(j)%flag(1:2), " {v1,v2,...}"
                      IF (DCOMPLEX_array_args(j)%long_flag_set) THEN
                         WRITE (*,'(A)',advance='no') ', '
                      ELSE
                         WRITE (*,*) ''
                      END IF
                   END IF

                   IF (DCOMPLEX_array_args(j)%long_flag_set) &
                        WRITE(*,*) DCOMPLEX_array_args(j)%long_flag &
                        (1:(LEN_TRIM(DCOMPLEX_array_args(j)%long_flag))), " {v1,v2,...}"
                END IF

                IF (in_line) WRITE (*,*) ''
             END IF
             CYCLE comment_loop
          END IF
       END DO
       END DO comment_loop

       WRITE (*,'(A)') "#-------------------------------------------------------------------------------"

       var_loop: DO i=1,group_size(k)
          ! scalar
       DO j=1,INTEGER_args_i
          IF (INTEGER_args(j)%group   .EQ. k .AND. &
              INTEGER_args(j)%group_i .EQ. i) THEN

             IF (INTEGER_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     INTEGER_args(j)%ctrl_name &
                     (1:LEN_TRIM(INTEGER_args(j)%ctrl_name))

!                 IF (INTEGER_args(j)%default_set) THEN
                   WRITE (scratch, *) INTEGER_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,LONGINT_args_i
          IF (LONGINT_args(j)%group   .EQ. k .AND. &
              LONGINT_args(j)%group_i .EQ. i) THEN

             IF (LONGINT_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     LONGINT_args(j)%ctrl_name &
                     (1:LEN_TRIM(LONGINT_args(j)%ctrl_name))

!                 IF (LONGINT_args(j)%default_set) THEN
                   WRITE (scratch, *) LONGINT_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,SINGLE_args_i
          IF (SINGLE_args(j)%group   .EQ. k .AND. &
              SINGLE_args(j)%group_i .EQ. i) THEN

             IF (SINGLE_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     SINGLE_args(j)%ctrl_name &
                     (1:LEN_TRIM(SINGLE_args(j)%ctrl_name))

!                 IF (SINGLE_args(j)%default_set) THEN
                   WRITE (scratch, *) SINGLE_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,DOUBLE_args_i
          IF (DOUBLE_args(j)%group   .EQ. k .AND. &
              DOUBLE_args(j)%group_i .EQ. i) THEN

             IF (DOUBLE_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     DOUBLE_args(j)%ctrl_name &
                     (1:LEN_TRIM(DOUBLE_args(j)%ctrl_name))

!                 IF (DOUBLE_args(j)%default_set) THEN
                   WRITE (scratch, *) DOUBLE_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,LOGICAL_args_i
          IF (LOGICAL_args(j)%group   .EQ. k .AND. &
              LOGICAL_args(j)%group_i .EQ. i) THEN

             IF (LOGICAL_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     LOGICAL_args(j)%ctrl_name &
                     (1:LEN_TRIM(LOGICAL_args(j)%ctrl_name))

!                 IF (LOGICAL_args(j)%default_set) THEN
                   WRITE (scratch, *) LOGICAL_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,STRING_args_i
          IF (STRING_args(j)%group   .EQ. k .AND. &
              STRING_args(j)%group_i .EQ. i) THEN

             IF (STRING_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     STRING_args(j)%ctrl_name &
                     (1:LEN_TRIM(STRING_args(j)%ctrl_name))

!                 IF (STRING_args(j)%default_set) THEN
                   WRITE (*,*) "= ", STRING_args(j)%variable &
                        (1:LEN_TRIM(STRING_args(j)%variable))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,COMPLEX_args_i
          IF (COMPLEX_args(j)%group   .EQ. k .AND. &
              COMPLEX_args(j)%group_i .EQ. i) THEN

             IF (COMPLEX_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     COMPLEX_args(j)%ctrl_name &
                     (1:LEN_TRIM(COMPLEX_args(j)%ctrl_name))

!                 IF (COMPLEX_args(j)%default_set) THEN
                   WRITE (scratch, *) COMPLEX_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_args_i
          IF (DCOMPLEX_args(j)%group   .EQ. k .AND. &
              DCOMPLEX_args(j)%group_i .EQ. i) THEN

             IF (DCOMPLEX_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     DCOMPLEX_args(j)%ctrl_name &
                     (1:LEN_TRIM(DCOMPLEX_args(j)%ctrl_name))

!                 IF (DCOMPLEX_args(j)%default_set) THEN
                   WRITE (scratch, *) DCOMPLEX_args(j)%variable
                   scratch = ADJUSTL(scratch)
                   WRITE (*,*) "= ", scratch(1:LEN_TRIM(scratch))
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
          ! array
       DO j=1,INTEGER_array_args_i
          IF (INTEGER_array_args(j)%group   .EQ. k .AND. &
              INTEGER_array_args(j)%group_i .EQ. i) THEN

             IF (INTEGER_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     INTEGER_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(INTEGER_array_args(j)%ctrl_name))

!                 IF (INTEGER_array_args(j)%default_set) THEN
                   DO l=LBOUND(INTEGER_array_args(j)%variable,1), &
                        UBOUND(INTEGER_array_args(j)%variable,1)
                      WRITE(scratch,*) INTEGER_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(INTEGER_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,LONGINT_array_args_i
          IF (LONGINT_array_args(j)%group   .EQ. k .AND. &
              LONGINT_array_args(j)%group_i .EQ. i) THEN

             IF (LONGINT_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     LONGINT_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(LONGINT_array_args(j)%ctrl_name))

!                 IF (LONGINT_array_args(j)%default_set) THEN
                   DO l=LBOUND(LONGINT_array_args(j)%variable,1), &
                        UBOUND(LONGINT_array_args(j)%variable,1)
                      WRITE(scratch,*) LONGINT_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(LONGINT_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,SINGLE_array_args_i
          IF (SINGLE_array_args(j)%group   .EQ. k .AND. &
              SINGLE_array_args(j)%group_i .EQ. i) THEN

             IF (SINGLE_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     SINGLE_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(SINGLE_array_args(j)%ctrl_name))

!                 IF (SINGLE_array_args(j)%default_set) THEN
                   DO l=LBOUND(SINGLE_array_args(j)%variable,1), &
                        UBOUND(SINGLE_array_args(j)%variable,1)
                      WRITE(scratch,*) SINGLE_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(SINGLE_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,DOUBLE_array_args_i
          IF (DOUBLE_array_args(j)%group   .EQ. k .AND. &
              DOUBLE_array_args(j)%group_i .EQ. i) THEN

             IF (DOUBLE_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     DOUBLE_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(DOUBLE_array_args(j)%ctrl_name))

!                 IF (DOUBLE_array_args(j)%default_set) THEN
                   DO l=LBOUND(DOUBLE_array_args(j)%variable,1), &
                        UBOUND(DOUBLE_array_args(j)%variable,1)
                      WRITE(scratch,*) DOUBLE_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(DOUBLE_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,LOGICAL_array_args_i
          IF (LOGICAL_array_args(j)%group   .EQ. k .AND. &
              LOGICAL_array_args(j)%group_i .EQ. i) THEN

             IF (LOGICAL_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     LOGICAL_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(LOGICAL_array_args(j)%ctrl_name))

!                 IF (LOGICAL_array_args(j)%default_set) THEN
                   DO l=LBOUND(LOGICAL_array_args(j)%variable,1), &
                        UBOUND(LOGICAL_array_args(j)%variable,1)
                      WRITE(scratch,*) LOGICAL_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(LOGICAL_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,STRING_array_args_i
          IF (STRING_array_args(j)%group   .EQ. k .AND. &
              STRING_array_args(j)%group_i .EQ. i) THEN

             IF (STRING_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     STRING_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(STRING_array_args(j)%ctrl_name))

!                 IF (STRING_array_args(j)%default_set) THEN
                   WRITE (*,'(A)',advance='no') '= '
                   DO l=LBOUND(STRING_array_args(j)%variable,1), &
                        UBOUND(STRING_array_args(j)%variable,1)
                      WRITE (*,'(2A)',advance='no') &
                           STRING_array_args(j)%variable(l) &
                           (1:LEN_TRIM(STRING_array_args(j)%variable(l))), &
                           ', '
                   END DO
                   WRITE (*,'(A)') ''
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,COMPLEX_array_args_i
          IF (COMPLEX_array_args(j)%group   .EQ. k .AND. &
              COMPLEX_array_args(j)%group_i .EQ. i) THEN

             IF (COMPLEX_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     COMPLEX_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(COMPLEX_array_args(j)%ctrl_name))

!                 IF (COMPLEX_array_args(j)%default_set) THEN
                   DO l=LBOUND(COMPLEX_array_args(j)%variable,1), &
                        UBOUND(COMPLEX_array_args(j)%variable,1)
                      WRITE(scratch,*) COMPLEX_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(COMPLEX_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       DO j=1,DCOMPLEX_array_args_i
          IF (DCOMPLEX_array_args(j)%group   .EQ. k .AND. &
              DCOMPLEX_array_args(j)%group_i .EQ. i) THEN

             IF (DCOMPLEX_array_args(j)%ctrl_name_set) THEN
                WRITE (scratch, *) group_max_len(k)
                WRITE (*,'(A' // scratch(1:LEN_TRIM(scratch)) // ')',advance='no') &
                     DCOMPLEX_array_args(j)%ctrl_name &
                     (1:LEN_TRIM(DCOMPLEX_array_args(j)%ctrl_name))

!                 IF (DCOMPLEX_array_args(j)%default_set) THEN
                   DO l=LBOUND(DCOMPLEX_array_args(j)%variable,1), &
                        UBOUND(DCOMPLEX_array_args(j)%variable,1)
                      WRITE(scratch,*) DCOMPLEX_array_args(j)%variable(l)
                      WRITE (*,'(A)',advance='no') &
                           scratch(1:LEN_TRIM(scratch))
                      IF (l .EQ. UBOUND(DCOMPLEX_array_args(j)%variable,1)) THEN
                         WRITE (*,*) ''
                      ELSE
                         WRITE (*,'(A)',advance='no') ', '
                      END IF
                   END DO
                END IF
!              END IF

             CYCLE var_loop
          END IF
       END DO
       END DO var_loop

    END DO
  END SUBROUTINE print_ctrl
  !------------------------------------------------------------------------
  !  Debug
  !------------------------------------------------------------------------
!   SUBROUTINE dump_defines
! !!! Debugging procedure. Prints all defined args.
!     INTEGER :: i
!     WRITE (*,'(//A)') "DUMPING DEFINED ARGS"
!     WRITE (*,'(//A/)') "Integers: "
!     DO i=1,INTEGER_args_i
! #define DTYPE INTEGER
! #include "ctrl/dump.f"
! #undef DTYPE
!     END DO
!     WRITE (*,'(//A/)') "Reals: "
!     DO i=1,REAL_args_i
! #define DTYPE REAL
! #include "ctrl/dump.f"
! #undef DTYPE
!     END DO
!     WRITE (*,'(//A/)') "Chars: "
!     DO i=1,CHAR_args_i
! #define DTYPE CHAR
! #define STRING
! #include "ctrl/dump.f"
! #undef STRING
! #undef DTYPE
!     END DO
!     WRITE (*,'(//A/)') "Logicals: "
!     DO i=1,LOGICAL_args_i
! #define DTYPE LOGICAL
! #define BOOL
! #include "ctrl/dump.f"
! #undef BOOL
! #undef DTYPE
!     END DO
!   END SUBROUTINE dump_defines
  SUBROUTINE dump_args
!!! Debugging procedure. Prints all supplied command line args.
    INTEGER :: i
    WRITE (*,'(//A/)') 'DUMPING COMMAND ARGS'
    DO i=1,cmd_args_i
       WRITE (*,*) 'arg : ', cmd_args(i)(1:cmd_args_len(i))
       IF (cmd_args_used(i)) THEN
          WRITE (*,*) 'used'
       ELSE
          WRITE (*,*) 'not used'
       END IF
    END DO
  END SUBROUTINE dump_args
  !------------------------------------------------------------------------
  !  Adders - black or otherwise
  !------------------------------------------------------------------------

  ! scalar
  SUBROUTINE INTEGER_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     INTEGER,                                TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     INTEGER,                                OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     INTEGER,                                OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     INTEGER,                                OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(INTEGER_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(INTEGER_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(INTEGER_arg), DIMENSION(:), POINTER            :: temp
    TYPE(INTEGER_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(INTEGER_args)) THEN
       ALLOCATE(INTEGER_args(1:di))
       INTEGER_args_i = 0
    END IF
    ! increment counter
    INTEGER_args_i = INTEGER_args_i + 1
    ! grow storage by di if needed
    IF (INTEGER_args_i .GT. SIZE(INTEGER_args)) THEN
       ALLOCATE(temp(1:SIZE(INTEGER_args)+di))
       temp(1:SIZE(INTEGER_args)) = INTEGER_args
       DEALLOCATE(INTEGER_args)
       INTEGER_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    INTEGER_args(INTEGER_args_i) = def
  END SUBROUTINE INTEGER_add_arg

  SUBROUTINE LONGINT_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     INTEGER(ppm_kind_int64),                TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     INTEGER(ppm_kind_int64),                OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     INTEGER(ppm_kind_int64),                OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     INTEGER(ppm_kind_int64),                OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(LONGINT_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(LONGINT_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(LONGINT_arg), DIMENSION(:), POINTER            :: temp
    TYPE(LONGINT_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(LONGINT_args)) THEN
       ALLOCATE(LONGINT_args(1:di))
       LONGINT_args_i = 0
    END IF
    ! increment counter
    LONGINT_args_i = LONGINT_args_i + 1
    ! grow storage by di if needed
    IF (LONGINT_args_i .GT. SIZE(LONGINT_args)) THEN
       ALLOCATE(temp(1:SIZE(LONGINT_args)+di))
       temp(1:SIZE(LONGINT_args)) = LONGINT_args
       DEALLOCATE(LONGINT_args)
       LONGINT_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    LONGINT_args(LONGINT_args_i) = def
  END SUBROUTINE LONGINT_add_arg

  SUBROUTINE SINGLE_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     REAL(ppm_kind_single),                  TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     REAL(ppm_kind_single),                  OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     REAL(ppm_kind_single),                  OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     REAL(ppm_kind_single),                  OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(SINGLE_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(SINGLE_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(SINGLE_arg), DIMENSION(:), POINTER            :: temp
    TYPE(SINGLE_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(SINGLE_args)) THEN
       ALLOCATE(SINGLE_args(1:di))
       SINGLE_args_i = 0
    END IF
    ! increment counter
    SINGLE_args_i = SINGLE_args_i + 1
    ! grow storage by di if needed
    IF (SINGLE_args_i .GT. SIZE(SINGLE_args)) THEN
       ALLOCATE(temp(1:SIZE(SINGLE_args)+di))
       temp(1:SIZE(SINGLE_args)) = SINGLE_args
       DEALLOCATE(SINGLE_args)
       SINGLE_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    SINGLE_args(SINGLE_args_i) = def
  END SUBROUTINE SINGLE_add_arg

  SUBROUTINE DOUBLE_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     REAL(ppm_kind_double),                  TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     REAL(ppm_kind_double),                  OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     REAL(ppm_kind_double),                  OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     REAL(ppm_kind_double),                  OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(DOUBLE_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(DOUBLE_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(DOUBLE_arg), DIMENSION(:), POINTER            :: temp
    TYPE(DOUBLE_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(DOUBLE_args)) THEN
       ALLOCATE(DOUBLE_args(1:di))
       DOUBLE_args_i = 0
    END IF
    ! increment counter
    DOUBLE_args_i = DOUBLE_args_i + 1
    ! grow storage by di if needed
    IF (DOUBLE_args_i .GT. SIZE(DOUBLE_args)) THEN
       ALLOCATE(temp(1:SIZE(DOUBLE_args)+di))
       temp(1:SIZE(DOUBLE_args)) = DOUBLE_args
       DEALLOCATE(DOUBLE_args)
       DOUBLE_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    DOUBLE_args(DOUBLE_args_i) = def
  END SUBROUTINE DOUBLE_add_arg

  SUBROUTINE LOGICAL_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 type, &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     LOGICAL,                                TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     LOGICAL,                                OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    LOGICAL,                                 OPTIONAL, INTENT(IN   ) :: type
!!! Type of flag. Logical flags require no value to be supplied.
!!! Instead the behavior depends on this argument. One of:
!!! enabling_flag :: Presence of flag sets variable to .TRUE.
!!! disabling_flag :: Presence of flag sets variable to .FALSE.
    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(LOGICAL_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(LOGICAL_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(LOGICAL_arg), DIMENSION(:), POINTER            :: temp
    TYPE(LOGICAL_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(LOGICAL_args)) THEN
       ALLOCATE(LOGICAL_args(1:di))
       LOGICAL_args_i = 0
    END IF
    ! increment counter
    LOGICAL_args_i = LOGICAL_args_i + 1
    ! grow storage by di if needed
    IF (LOGICAL_args_i .GT. SIZE(LOGICAL_args)) THEN
       ALLOCATE(temp(1:SIZE(LOGICAL_args)+di))
       temp(1:SIZE(LOGICAL_args)) = LOGICAL_args
       DEALLOCATE(LOGICAL_args)
       LOGICAL_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(type)) THEN
       def%type         =  type
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    LOGICAL_args(LOGICAL_args_i) = def
  END SUBROUTINE LOGICAL_add_arg

  SUBROUTINE STRING_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
!        &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     CHARACTER(LEN=*),                       TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     CHARACTER(LEN=*),                       OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


!      INTEGER,                                OPTIONAL, INTENT(IN   ) :: min
! !!! Minimum length of string.
!      INTEGER,                                OPTIONAL, INTENT(IN   ) :: max
! !!! Maximum length of string.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(STRING_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(STRING_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(STRING_arg), DIMENSION(:), POINTER            :: temp
    TYPE(STRING_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(STRING_args)) THEN
       ALLOCATE(STRING_args(1:di))
       STRING_args_i = 0
    END IF
    ! increment counter
    STRING_args_i = STRING_args_i + 1
    ! grow storage by di if needed
    IF (STRING_args_i .GT. SIZE(STRING_args)) THEN
       ALLOCATE(temp(1:SIZE(STRING_args)+di))
       temp(1:SIZE(STRING_args)) = STRING_args
       DEALLOCATE(STRING_args)
       STRING_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
!     IF (PRESENT(min)) THEN
!        def%min                  =  min
!        def%min_set              =  .TRUE.
!     END IF
!     IF (PRESENT(max)) THEN
!        def%max                  =  max
!        def%max_set              =  .TRUE.
!     END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    STRING_args(STRING_args_i) = def
  END SUBROUTINE STRING_add_arg

  SUBROUTINE COMPLEX_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     COMPLEX(ppm_kind_single),               TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     COMPLEX(ppm_kind_single),               OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(COMPLEX_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(COMPLEX_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(COMPLEX_arg), DIMENSION(:), POINTER            :: temp
    TYPE(COMPLEX_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(COMPLEX_args)) THEN
       ALLOCATE(COMPLEX_args(1:di))
       COMPLEX_args_i = 0
    END IF
    ! increment counter
    COMPLEX_args_i = COMPLEX_args_i + 1
    ! grow storage by di if needed
    IF (COMPLEX_args_i .GT. SIZE(COMPLEX_args)) THEN
       ALLOCATE(temp(1:SIZE(COMPLEX_args)+di))
       temp(1:SIZE(COMPLEX_args)) = COMPLEX_args
       DEALLOCATE(COMPLEX_args)
       COMPLEX_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    COMPLEX_args(COMPLEX_args_i) = def
  END SUBROUTINE COMPLEX_add_arg

  SUBROUTINE DCOMPLEX_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     COMPLEX(ppm_kind_double),               TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     COMPLEX(ppm_kind_double),               OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(DCOMPLEX_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(DCOMPLEX_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(DCOMPLEX_arg), DIMENSION(:), POINTER            :: temp
    TYPE(DCOMPLEX_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(DCOMPLEX_args)) THEN
       ALLOCATE(DCOMPLEX_args(1:di))
       DCOMPLEX_args_i = 0
    END IF
    ! increment counter
    DCOMPLEX_args_i = DCOMPLEX_args_i + 1
    ! grow storage by di if needed
    IF (DCOMPLEX_args_i .GT. SIZE(DCOMPLEX_args)) THEN
       ALLOCATE(temp(1:SIZE(DCOMPLEX_args)+di))
       temp(1:SIZE(DCOMPLEX_args)) = DCOMPLEX_args
       DEALLOCATE(DCOMPLEX_args)
       DCOMPLEX_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    DCOMPLEX_args(DCOMPLEX_args_i) = def
  END SUBROUTINE DCOMPLEX_add_arg

  ! array

  SUBROUTINE INTEGER_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     INTEGER,                  DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     INTEGER,                  DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     INTEGER,                                OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     INTEGER,                                OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(INTEGER_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(INTEGER_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(INTEGER_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(INTEGER_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(INTEGER_array_args)) THEN
       ALLOCATE(INTEGER_array_args(1:di))
       INTEGER_array_args_i = 0
    END IF
    ! increment counter
    INTEGER_array_args_i = INTEGER_array_args_i + 1
    ! grow storage by di if needed
    IF (INTEGER_array_args_i .GT. SIZE(INTEGER_array_args)) THEN
       ALLOCATE(temp(1:SIZE(INTEGER_array_args)+di))
       temp(1:SIZE(INTEGER_array_args)) = INTEGER_array_args
       DEALLOCATE(INTEGER_array_args)
       INTEGER_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    INTEGER_array_args(INTEGER_array_args_i) = def
  END SUBROUTINE INTEGER_array_add_arg

  SUBROUTINE LONGINT_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     INTEGER(ppm_kind_int64),  DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     INTEGER(ppm_kind_int64),  DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     INTEGER(ppm_kind_int64),                OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     INTEGER(ppm_kind_int64),                OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(LONGINT_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(LONGINT_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(LONGINT_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(LONGINT_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(LONGINT_array_args)) THEN
       ALLOCATE(LONGINT_array_args(1:di))
       LONGINT_array_args_i = 0
    END IF
    ! increment counter
    LONGINT_array_args_i = LONGINT_array_args_i + 1
    ! grow storage by di if needed
    IF (LONGINT_array_args_i .GT. SIZE(LONGINT_array_args)) THEN
       ALLOCATE(temp(1:SIZE(LONGINT_array_args)+di))
       temp(1:SIZE(LONGINT_array_args)) = LONGINT_array_args
       DEALLOCATE(LONGINT_array_args)
       LONGINT_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    LONGINT_array_args(LONGINT_array_args_i) = def
  END SUBROUTINE LONGINT_array_add_arg

  SUBROUTINE SINGLE_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     REAL(ppm_kind_single),    DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     REAL(ppm_kind_single),    DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     REAL(ppm_kind_single),                  OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     REAL(ppm_kind_single),                  OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(SINGLE_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(SINGLE_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(SINGLE_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(SINGLE_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(SINGLE_array_args)) THEN
       ALLOCATE(SINGLE_array_args(1:di))
       SINGLE_array_args_i = 0
    END IF
    ! increment counter
    SINGLE_array_args_i = SINGLE_array_args_i + 1
    ! grow storage by di if needed
    IF (SINGLE_array_args_i .GT. SIZE(SINGLE_array_args)) THEN
       ALLOCATE(temp(1:SIZE(SINGLE_array_args)+di))
       temp(1:SIZE(SINGLE_array_args)) = SINGLE_array_args
       DEALLOCATE(SINGLE_array_args)
       SINGLE_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    SINGLE_array_args(SINGLE_array_args_i) = def
  END SUBROUTINE SINGLE_array_add_arg

  SUBROUTINE DOUBLE_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
       &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     REAL(ppm_kind_double),    DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     REAL(ppm_kind_double),    DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


     REAL(ppm_kind_double),                  OPTIONAL, INTENT(IN   ) :: min
!!! Minimum value of arg.
     REAL(ppm_kind_double),                  OPTIONAL, INTENT(IN   ) :: max
!!! Maximum value of arg.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(DOUBLE_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(DOUBLE_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(DOUBLE_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(DOUBLE_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(DOUBLE_array_args)) THEN
       ALLOCATE(DOUBLE_array_args(1:di))
       DOUBLE_array_args_i = 0
    END IF
    ! increment counter
    DOUBLE_array_args_i = DOUBLE_array_args_i + 1
    ! grow storage by di if needed
    IF (DOUBLE_array_args_i .GT. SIZE(DOUBLE_array_args)) THEN
       ALLOCATE(temp(1:SIZE(DOUBLE_array_args)+di))
       temp(1:SIZE(DOUBLE_array_args)) = DOUBLE_array_args
       DEALLOCATE(DOUBLE_array_args)
       DOUBLE_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    def%min       = -HUGE(min)
    def%max       =  HUGE(max)
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    IF (PRESENT(min)) THEN
       def%min                  =  min
       def%min_set              =  .TRUE.
    END IF
    IF (PRESENT(max)) THEN
       def%max                  =  max
       def%max_set              =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    DOUBLE_array_args(DOUBLE_array_args_i) = def
  END SUBROUTINE DOUBLE_array_add_arg

  SUBROUTINE LOGICAL_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     LOGICAL,                  DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     LOGICAL,                  DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(LOGICAL_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(LOGICAL_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(LOGICAL_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(LOGICAL_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(LOGICAL_array_args)) THEN
       ALLOCATE(LOGICAL_array_args(1:di))
       LOGICAL_array_args_i = 0
    END IF
    ! increment counter
    LOGICAL_array_args_i = LOGICAL_array_args_i + 1
    ! grow storage by di if needed
    IF (LOGICAL_array_args_i .GT. SIZE(LOGICAL_array_args)) THEN
       ALLOCATE(temp(1:SIZE(LOGICAL_array_args)+di))
       temp(1:SIZE(LOGICAL_array_args)) = LOGICAL_array_args
       DEALLOCATE(LOGICAL_array_args)
       LOGICAL_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    LOGICAL_array_args(LOGICAL_array_args_i) = def
  END SUBROUTINE LOGICAL_array_add_arg

  SUBROUTINE STRING_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
!        &                         min, max,                                   &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     CHARACTER(LEN=*),         DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     CHARACTER(LEN=*),         DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.


!      INTEGER,                                OPTIONAL, INTENT(IN   ) :: min
! !!! Minimum length of string.
!      INTEGER,                                OPTIONAL, INTENT(IN   ) :: max
! !!! Maximum length of string.

    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(STRING_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(STRING_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(STRING_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(STRING_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(STRING_array_args)) THEN
       ALLOCATE(STRING_array_args(1:di))
       STRING_array_args_i = 0
    END IF
    ! increment counter
    STRING_array_args_i = STRING_array_args_i + 1
    ! grow storage by di if needed
    IF (STRING_array_args_i .GT. SIZE(STRING_array_args)) THEN
       ALLOCATE(temp(1:SIZE(STRING_array_args)+di))
       temp(1:SIZE(STRING_array_args)) = STRING_array_args
       DEALLOCATE(STRING_array_args)
       STRING_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
!     IF (PRESENT(min)) THEN
!        def%min                  =  min
!        def%min_set              =  .TRUE.
!     END IF
!     IF (PRESENT(max)) THEN
!        def%max                  =  max
!        def%max_set              =  .TRUE.
!     END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    STRING_array_args(STRING_array_args_i) = def
  END SUBROUTINE STRING_array_add_arg

  SUBROUTINE COMPLEX_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     COMPLEX(ppm_kind_single), DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     COMPLEX(ppm_kind_single), DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(COMPLEX_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(COMPLEX_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(COMPLEX_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(COMPLEX_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(COMPLEX_array_args)) THEN
       ALLOCATE(COMPLEX_array_args(1:di))
       COMPLEX_array_args_i = 0
    END IF
    ! increment counter
    COMPLEX_array_args_i = COMPLEX_array_args_i + 1
    ! grow storage by di if needed
    IF (COMPLEX_array_args_i .GT. SIZE(COMPLEX_array_args)) THEN
       ALLOCATE(temp(1:SIZE(COMPLEX_array_args)+di))
       temp(1:SIZE(COMPLEX_array_args)) = COMPLEX_array_args
       DEALLOCATE(COMPLEX_array_args)
       COMPLEX_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    COMPLEX_array_args(COMPLEX_array_args_i) = def
  END SUBROUTINE COMPLEX_array_add_arg

  SUBROUTINE DCOMPLEX_array_add_arg(variable, name, flag, long_flag, ctrl_name, &
       &                         default,                                    &
                                 default_func, validator, &
                                 help)
!!! Adds a new arg definition.
    !----------------------------------------------------------------------
    !  Arguments
    !----------------------------------------------------------------------

     COMPLEX(ppm_kind_double), DIMENSION(:), TARGET,   INTENT(IN   ) :: variable
!!! Global variable to bind to.
     COMPLEX(ppm_kind_double), DIMENSION(:), OPTIONAL, INTENT(IN   ) :: default
!!! Default value.



    CHARACTER(LEN=*),                                  INTENT(IN   ) :: name
!!! Name of the arg for use in the auto generated usage message/ctrl file.
    CHARACTER(LEN=2),                        OPTIONAL, INTENT(IN   ) :: flag
!!! Single character flag (eg. _'-f'_).
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: long_flag
!!! Long flag (eg. _'--flag'_). Has to start with _'--'_!
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: ctrl_name
!!! Control file variable name.
    CHARACTER(LEN=*),                        OPTIONAL, INTENT(IN   ) :: help
!!! Help string for the auto generated usage message/ctrl file.
    PROCEDURE(DCOMPLEX_array_func),             OPTIONAL                :: default_func
!!! Default function.
    PROCEDURE(DCOMPLEX_array_func),             OPTIONAL                :: validator
!!! Validator function.
    !----------------------------------------------------------------------
    !  Local variables
    !----------------------------------------------------------------------
    TYPE(DCOMPLEX_array_arg), DIMENSION(:), POINTER            :: temp
    TYPE(DCOMPLEX_array_arg)                                   :: def
    INTEGER                                                 :: len
    !----------------------------------------------------------------------
    !  Body
    !----------------------------------------------------------------------
    ! create default group
    IF (groups_i .EQ. -1) CALL arg_group("General Options")
    ! allocate initial storage
    IF (.NOT. ASSOCIATED(DCOMPLEX_array_args)) THEN
       ALLOCATE(DCOMPLEX_array_args(1:di))
       DCOMPLEX_array_args_i = 0
    END IF
    ! increment counter
    DCOMPLEX_array_args_i = DCOMPLEX_array_args_i + 1
    ! grow storage by di if needed
    IF (DCOMPLEX_array_args_i .GT. SIZE(DCOMPLEX_array_args)) THEN
       ALLOCATE(temp(1:SIZE(DCOMPLEX_array_args)+di))
       temp(1:SIZE(DCOMPLEX_array_args)) = DCOMPLEX_array_args
       DEALLOCATE(DCOMPLEX_array_args)
       DCOMPLEX_array_args => temp
    END IF
    ! populate structure
    def%variable  => variable
    def%name      =  name
    ! default values
    ALLOCATE(def%default(LBOUND(def%variable,1):UBOUND(def%variable,1)))
    def%default   = def%variable
    def%flag      = ''
    def%long_flag = ''
    def%ctrl_name = ''
    def%help      = ''
    ! supplied values
    IF (PRESENT(flag)) THEN
       def%flag                 =  flag
       def%flag_set             =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(long_flag)) THEN
       def%long_flag            =  long_flag
       def%long_flag_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_arg(groups_i)  =  .TRUE.
    END IF
    IF (PRESENT(ctrl_name)) THEN
       def%ctrl_name            =  ctrl_name
       def%ctrl_name_set        =  .TRUE.
       def%settable             =  .TRUE.
       group_has_ctrl(groups_i) =  .TRUE.
       len                      =  LEN_TRIM(ctrl_name)
       IF (len .GT. group_max_len(groups_i)) group_max_len(groups_i) = len  
    END IF
    IF (PRESENT(help)) THEN
       def%help                 =  help
       def%help_set             =  .TRUE.
    END IF
    IF (PRESENT(default)) THEN
       def%default              =  default
       def%default_set          =  .TRUE.
    END IF
    IF (PRESENT(default_func)) THEN
       def%default_func         => default_func
       def%default_func_set     =  .TRUE.
    END IF
    IF (PRESENT(validator)) THEN
       def%validator            => validator
       def%validator_set        =  .TRUE.
    END IF
    ! group
    def%group            = groups_i
    group_size(groups_i) = group_size(groups_i) + 1
    def%group_i          = group_size(groups_i)
    DCOMPLEX_array_args(DCOMPLEX_array_args_i) = def
  END SUBROUTINE DCOMPLEX_array_add_arg


  !-------------------------------------------------------------------------
  !  My own uppercase!!! YAY!!!
  !-------------------------------------------------------------------------
  SUBROUTINE UpperCase(string,ilen,info)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: string
    INTEGER         , INTENT(IN   ) :: ilen
    INTEGER         , INTENT(  OUT) :: info
    INTEGER          :: i,j
    INTEGER          :: i1,i2,i3,iadd
    info = 0
    i1   = IACHAR('a') - 1
    i2   = IACHAR('z') + 1
    i3   = IACHAR('A')
    iadd = i3 - i1 - 1
    DO i=1,ilen
       j = IACHAR(string(i:i))
       IF (j.GT.i1.AND.j.LT.i2) THEN
          string(i:i) = CHAR(j+iadd)
       ENDIF
    ENDDO
    RETURN
  END SUBROUTINE UpperCase

END MODULE ppm_module_ctrl
