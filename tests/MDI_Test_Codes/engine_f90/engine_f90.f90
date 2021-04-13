PROGRAM ENGINE_F90

  USE mpi
  USE mdi,              ONLY : MDI_Init, MDI_MPI_get_world_comm
  USE mdi_implementation, ONLY : dp, initialize_mdi, respond_to_commands, world_comm

  IMPLICIT NONE

  INTEGER :: iarg, ierr
  CHARACTER(len=1024) :: arg, mdi_options

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
         CALL MDI_Init( mdi_options, ierr)
         CALL MDI_MPI_get_world_comm( world_comm, ierr )

         EXIT
      END IF

      iarg = iarg + 1
   END DO

   ! Perform one-time operations required to establish a connection with the driver
   CALL initialize_mdi()

   ! Respond to commands from the driver
   CALL respond_to_commands()

   ! Synchronize all MPI ranks
   CALL MPI_Barrier( world_comm, ierr )
   CALL MPI_Finalize( ierr )

END PROGRAM ENGINE_F90
