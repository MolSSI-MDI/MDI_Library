PROGRAM ENGINE_F90

  USE mpi
  USE ISO_C_binding
  USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_INT, MDI_DOUBLE, MDI_CHAR, MDI_NAME_LENGTH, &
       MDI_Accept_Communicator, MDI_Recv_Command, MDI_Recv, MDI_Conversion_Factor, &
       MDI_Set_Execute_Command_Func, MDI_Get_Role, MDI_ENGINE, &
       MDI_Register_Node, MDI_Register_Command, MDI_Register_Callback

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307)

  LOGICAL :: terminate_flag

  INTEGER :: iarg, ierr, role
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
   call MPI_Init(ierr)

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

   ! Confirm that the code is being run as an ENGINE
   call MDI_Get_Role(role, ierr)
   IF ( role .ne. MDI_ENGINE ) THEN
      WRITE(6,*)'ERROR: Must run engine_f90 as an ENGINE'
   END IF

   ! Get the MPI rank within world_comm
   CALL MPI_Comm_rank( world_comm, world_rank, ierr )

   ! Register the commands
   CALL MDI_Register_Node("@GLOBAL", ierr)
   CALL MDI_Register_Command("@GLOBAL", "EXIT", ierr)
   CALL MDI_Register_Command("@GLOBAL", "<NATOMS", ierr)
   CALL MDI_Register_Command("@GLOBAL", "<COORDS", ierr)
   CALL MDI_Register_Command("@GLOBAL", "<FORCES", ierr)
   CALL MDI_Register_Node("@FORCES", ierr)
   CALL MDI_Register_Command("@FORCES", "EXIT", ierr)
   CALL MDI_Register_Command("@FORCES", "<FORCES", ierr)
   CALL MDI_Register_Command("@FORCES", ">FORCES", ierr)
   CALL MDI_Register_Callback("@FORCES", ">FORCES", ierr)

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

       CHARACTER(LEN=*), INTENT(IN)  :: command
       INTEGER, INTENT(IN)           :: comm
       INTEGER, INTENT(OUT)          :: ierr

       INTEGER                       :: icoord
       INTEGER                       :: natoms
       DOUBLE PRECISION, ALLOCATABLE :: coords(:), forces(:)

       ! set dummy molecular properties
       natoms = 10
       ALLOCATE( coords( 3 * natoms ) )
       DO icoord = 1, 3 * natoms
          coords(icoord) = 0.1_dp * ( icoord - 1 )
       END DO
       ALLOCATE( forces( 3 * natoms ) )
       DO icoord = 1, 3 * natoms
          forces(icoord) = 0.01_dp * ( icoord - 1 )
       END DO

       SELECT CASE( TRIM(command) )
       CASE( "EXIT" )
          terminate_flag = .true.
       CASE( "<NATOMS" )
          CALL MDI_Send(natoms, 1, MDI_INT, comm, ierr)
       CASE( "<COORDS" )
          CALL MDI_Send(coords, 3 * natoms, MDI_DOUBLE, comm, ierr)
       CASE( "<FORCES" )
          CALL MDI_Send(forces, 3 * natoms, MDI_DOUBLE, comm, ierr)
       CASE DEFAULT
          WRITE(6,*)'Error: command not recognized'
       END SELECT

       DEALLOCATE( coords, forces )

       ierr = 0
     END SUBROUTINE execute_command

END PROGRAM ENGINE_F90
