PROGRAM DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_CHAR, MDI_NAME_LENGTH, MDI_COMMAND_LENGTH, &
     MDI_Send, MDI_Init, MDI_Get_Role, &
     MDI_Accept_Communicator, MDI_Send_Command, MDI_Recv, MDI_Conversion_Factor, &
     MDI_Check_Node_Exists, MDI_Check_Command_Exists, MDI_Check_Callback_Exists, &
     MDI_Get_NNodes, MDI_Get_NCommands, MDI_Get_NCallbacks, &
     MDI_Get_Node, MDI_Get_Command, MDI_Get_Callback, MDI_DRIVER

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
         call MDI_Init( mdi_options, world_comm, ierr)

         EXIT
      END IF

      iarg = iarg + 1
   END DO

   ! Confirm that the code is being run as a driver
   call MDI_Get_Role(role, ierr)
   IF ( role .ne. MDI_DRIVER ) THEN
      WRITE(6,*)'ERROR: Must run driver_f90 as a DRIVER',role,MDI_DRIVER
   END IF

   ! Get the MPI rank within world_comm
   call MPI_Comm_rank( world_comm, world_rank, ierr );

   ! Connct to the engine
   call MDI_Accept_Communicator(comm, ierr)

   ! Confirm that the engine has the @GLOBAL node
   CALL MDI_Check_Node_Exists("@GLOBAL", comm, exists, ierr)
   IF ( exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not have @GLOBAL node'
   END IF

   ! Confirm that the engine supports the EXIT command
   CALL MDI_Check_Command_Exists("@GLOBAL", "EXIT", comm, exists, ierr)
   IF ( exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not support the EXIT command'
   END IF

   ! Determine the name of the engine
   call MDI_Send_Command("<NAME", comm, ierr)
   call MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, comm, ierr)

   WRITE(6,*)'Engine name: ', TRIM(message)

   ! Test the node, command, and callback inquiry functions 
   CALL MDI_Get_NNodes(comm, nnodes, ierr)
   WRITE(6,*)'NNODES: ',nnodes
   CALL MDI_Get_Node(1, comm, test_node, ierr)
   WRITE(6,*)'NODE: ',TRIM(test_node)
   CALL MDI_Get_NCommands(test_node, comm, ncommands, ierr)
   WRITE(6,*)'NCOMMANDS: ',ncommands
   CALL MDI_Get_Command(test_node, 2, comm, test_command, ierr)
   WRITE(6,*)'COMMAND: ',TRIM(test_command)
   CALL MDI_Get_NCallbacks(test_node, comm, ncallbacks, ierr)
   WRITE(6,*)'NCALLBACKS: ',ncallbacks
   CALL MDI_Get_Callback(test_node, 0, comm, test_callback, ierr)
   WRITE(6,*)'CALLBACK: ',TRIM(test_callback)
   CALL MDI_Check_Callback_Exists("@FORCES", ">FORCES", comm, exists, ierr)
   IF ( exists .ne. 1 ) THEN
      WRITE(6,*)'ERROR: Engine does not support the EXIT command'
   END IF

   call MDI_Send_Command("EXIT", comm, ierr)

   ! Synchronize all MPI ranks
   call MPI_Barrier( world_comm, ierr )
   call MPI_Finalize( ierr )

END PROGRAM DRIVER_F90
