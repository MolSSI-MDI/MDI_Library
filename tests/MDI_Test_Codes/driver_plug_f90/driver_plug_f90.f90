PROGRAM DRIVER_F90

USE mpi
USE ISO_C_binding
USE mdi,              ONLY : MDI_CHAR, MDI_NAME_LENGTH, MDI_COMMAND_LENGTH, &
     MDI_Send, MDI_Init, MDI_MPI_get_world_comm, MDI_Get_role, &
     MDI_Accept_communicator, MDI_Send_command, MDI_Recv, MDI_Conversion_factor, &
     MDI_Check_Node_exists, MDI_Check_command_exists, MDI_Check_callback_exists, &
     MDI_Get_nnodes, MDI_Get_ncommands, MDI_Get_ncallbacks, MDI_Launch_plugin, &
     MDI_Get_node, MDI_Get_command, MDI_Get_callback, MDI_DRIVER, MDI_String_to_atomic_number
USE mdi_implementation, ONLY : driver_plugin_callback

IMPLICIT NONE

   INTEGER :: iarg, ierr, exists, role, atomic_num
   INTEGER :: world_comm, world_rank
   INTEGER :: comm
   CHARACTER(len=1024) :: arg, mdi_options
   CHARACTER(len=:), ALLOCATABLE :: message

   INTEGER :: nnodes, ncommands, ncallbacks
   CHARACTER(len=MDI_NAME_LENGTH) :: test_node, test_command, test_callback

   TYPE(C_PTR) :: class_obj
   PROCEDURE(driver_plugin_callback), POINTER :: driver_callback => null()
   driver_callback => driver_plugin_callback

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

   ! Launch a plugin
   call MDI_Launch_plugin("engine_cxx", &
                          "-mdi ""-role ENGINE -method LINK -name MM""", &
                          world_comm, &
                          c_funloc(driver_callback), &
                          class_obj, &
                          ierr)

   DEALLOCATE( message )

   ! Synchronize all MPI ranks
   call MPI_Barrier( world_comm, ierr )
   call MPI_Finalize( ierr )

END PROGRAM DRIVER_F90
