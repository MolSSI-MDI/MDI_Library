MODULE ENGINE_LIB_F90

  USE mpi
  USE ISO_C_binding
  USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_INT, MDI_CHAR, MDI_NAME_LENGTH, &
       MDI_Accept_Communicator, MDI_Recv_Command, MDI_Recv, MDI_Conversion_Factor, &
       MDI_Set_Execute_Command_Func

  IMPLICIT NONE

  LOGICAL :: terminate_flag

CONTAINS

  SUBROUTINE engine_lib_f90_create(arg_in, world_comm_in)
#if MDI_WINDOWS
    !GCC$ ATTRIBUTES DLLEXPORT :: engine_lib_f90_create
    !DEC$ ATTRIBUTES DLLEXPORT :: engine_lib_f90_create
#endif
    CHARACTER(len=1024), INTENT(IN) :: arg_in
    INTEGER, INTENT(IN)             :: world_comm_in

    INTEGER :: iarg, ierr
    INTEGER :: world_comm, world_rank
    INTEGER :: comm
    CHARACTER(len=1024) :: arg, mdi_options
    CHARACTER(len=:), ALLOCATABLE :: message

    PROCEDURE(execute_command), POINTER :: general_command => null()
    TYPE(C_PTR)                         :: class_obj
    arg = arg_in
    world_comm = world_comm_in
    general_command => execute_command

    terminate_flag = .false.

    ALLOCATE( character(MDI_NAME_LENGTH) :: message )

    ! Initialize MDI
    CALL MDI_Init( arg, world_comm, ierr )

    ! Get the MPI rank within world_comm
    CALL MPI_Comm_rank( world_comm, world_rank, ierr )

    ! Set the generic execute_command function
    CALL MDI_Set_Execute_Command_Func(general_command, class_obj, ierr)

  END SUBROUTINE engine_lib_f90_create

  SUBROUTINE execute_command(command, comm, ierr)
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: command
    INTEGER, INTENT(IN)          :: comm
    INTEGER, INTENT(OUT)         :: ierr

    INTEGER :: natoms

    SELECT CASE( TRIM(command) )
    CASE( "EXIT" )
       terminate_flag = .true.
    CASE( "<NATOMS" )
       natoms = 123
       !CALL MDI_Send(natoms, 1, MDI_INT, comm, ierr)
       WRITE(6,*)'SUCCESS!'
    CASE DEFAULT
       WRITE(6,*)'Error: command not recognized'
    END SELECT

    ierr = 0
  END SUBROUTINE execute_command

END MODULE ENGINE_LIB_F90
