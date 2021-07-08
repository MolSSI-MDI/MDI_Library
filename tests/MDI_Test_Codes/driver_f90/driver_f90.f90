PROGRAM DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_CHAR, MDI_NAME_LENGTH, MDI_COMMAND_LENGTH, &
     MDI_Send, MDI_Init, MDI_MPI_get_world_comm, MDI_Get_role, &
     MDI_Accept_communicator, MDI_Send_command, MDI_Recv, MDI_Conversion_factor, &
     MDI_Check_Node_exists, MDI_Check_command_exists, MDI_Check_callback_exists, &
     MDI_Get_nnodes, MDI_Get_ncommands, MDI_Get_ncallbacks, &
     MDI_Get_node, MDI_Get_command, MDI_Get_callback, MDI_DRIVER

IMPLICIT NONE

   INTEGER :: iarg, ierr, exists, role
   INTEGER :: world_comm, world_rank
   INTEGER :: comm
   CHARACTER(len=1024) :: arg, mdi_options
   CHARACTER(len=:), ALLOCATABLE :: message

   INTEGER :: nnodes, ncommands, ncallbacks
   CHARACTER(len=MDI_NAME_LENGTH) :: test_node, test_command, test_callback

   ALLOCATE( character(MDI_COMMAND_LENGTH) :: message )

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
         call MDI_Init( mdi_options, ierr)
         call MDI_MPI_get_world_comm( world_comm, ierr )

         EXIT
      END IF

      iarg = iarg + 1
   END DO

   ! Confirm that the code is being run as a driver
   call MDI_Get_role(role, ierr)
   IF ( role .ne. MDI_DRIVER ) THEN
      WRITE(6,*)'ERROR: Must run driver_f90 as a DRIVER',role,MDI_DRIVER
   END IF

   ! Get the MPI rank within world_comm
   call MPI_Comm_rank( world_comm, world_rank, ierr );

   ! Connct to the engine
   call MDI_Accept_communicator(comm, ierr)

   ! Confirm that the engine has the @DEFAULT node
   CALL MDI_Check_node_exists("@DEFAULT", comm, exists, ierr)
   IF ( world_rank .eq. 0 .and. exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not have @DEFAULT node'
   END IF

   ! Confirm that the engine supports the EXIT command
   CALL MDI_Check_command_exists("@DEFAULT", "EXIT", comm, exists, ierr)
   IF ( world_rank .eq. 0 .and. exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not support the EXIT command'
   END IF

   ! Determine the name of the engine
   call MDI_Send_command("<NAME", comm, ierr)
   call MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)

   IF ( world_rank .eq. 0 ) WRITE(6,*)'Engine name: ', TRIM(message)

   ! Test the node, command, and callback inquiry functions 
   CALL MDI_Get_nnodes(comm, nnodes, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'NNODES: ',nnodes
   CALL MDI_Get_node(1, comm, test_node, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'NODE: ',TRIM(test_node)
   CALL MDI_Get_ncommands(test_node, comm, ncommands, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'NCOMMANDS: ',ncommands
   CALL MDI_Get_command(test_node, 2, comm, test_command, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'COMMAND: ',TRIM(test_command)
   CALL MDI_Get_ncallbacks(test_node, comm, ncallbacks, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'NCALLBACKS: ',ncallbacks
   CALL MDI_Get_callback(test_node, 0, comm, test_callback, ierr)
   IF ( world_rank .eq. 0 ) WRITE(6,*)'CALLBACK: ',TRIM(test_callback)
   CALL MDI_Check_callback_exists("@FORCES", ">FORCES", comm, exists, ierr)
   IF ( world_rank .eq. 0 .and. exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not support the EXIT command'
   END IF

   call MDI_Send_command("EXIT", comm, ierr)

   DEALLOCATE( message )

   ! Synchronize all MPI ranks
   call MPI_Barrier( world_comm, ierr )
   call MPI_Finalize( ierr )

END PROGRAM DRIVER_F90
