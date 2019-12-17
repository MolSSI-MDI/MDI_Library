PROGRAM ENGINE_F90

  USE mpi
  USE ISO_C_binding
  USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_INT, MDI_CHAR, MDI_NAME_LENGTH, &
       MDI_Accept_Communicator, MDI_Recv_Command, MDI_Recv, MDI_Conversion_Factor, &
       MDI_Set_Execute_Command_Func, MDI_Register_Node, MDI_Register_Command

  IMPLICIT NONE

  LOGICAL :: terminate_flag

  INTEGER :: iarg, ierr
  INTEGER :: world_comm, world_rank
  INTEGER :: comm
  CHARACTER(len=1024) :: arg, mdi_options
  CHARACTER(len=:), ALLOCATABLE :: message

  PROCEDURE(execute_command), POINTER :: generic_command => null()
  TYPE(C_PTR)                         :: class_obj
  generic_command => execute_command

  terminate_flag = .false.

   ALLOCATE( character(MDI_NAME_LENGTH) :: message )

   ! Initialize the MPI environment
   call MPI_INIT(ierr)

   ! Read through all the command line options
   iarg = 0
   DO
      CALL get_command_argument(iarg, arg)
      IF (LEN_TRIM(arg) == 0) EXIT

      IF (TRIM(arg) .eq. "-mdi") THEN
         CALL get_command_argument(iarg + 1, mdi_options)

         ! Initialize the MDI Library
         world_comm = MPI_COMM_WORLD
         CALL MDI_Init( mdi_options, world_comm, ierr)

         EXIT
      END IF

      iarg = iarg + 1
   END DO

   ! Get the MPI rank within world_comm
   CALL MPI_Comm_rank( world_comm, world_rank, ierr )

   ! Register the commands
   CALL MDI_Register_Node("@GLOBAL", ierr)
   CALL MDI_Register_Command("@GLOBAL", "EXIT", ierr)
   CALL MDI_Register_Command("@GLOBAL", "<NATOMS", ierr)

   ! Set the generic execute_command function
   CALL MDI_Set_Execute_Command_Func(generic_command, class_obj, ierr)

   ! Connct to the driver
   CALL MDI_Accept_Communicator(comm, ierr)

   ! Respond to the driver's commands
   response_loop: DO

      CALL MDI_Recv_Command(message, comm, ierr)

      CALL execute_command(message, comm, ierr)

      IF ( terminate_flag ) EXIT

   END DO response_loop

   ! Synchronize all MPI ranks
   CALL MPI_Barrier( world_comm, ierr )
   CALL MPI_Finalize( ierr )

   CONTAINS

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

END PROGRAM ENGINE_F90
