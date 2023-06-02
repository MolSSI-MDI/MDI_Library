MODULE MDI_IMPLEMENTATION

  USE mpi
  USE ISO_C_binding
  USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_INT, MDI_CHAR, MDI_NAME_LENGTH, &
       MDI_Accept_communicator, MDI_Recv_command, MDI_Recv, &
       MDI_Set_execute_command_func, MDI_MPI_get_world_comm, MDI_DOUBLE, MDI_BYTE, &
       MDI_ENGINE, MDI_Get_role, MDI_Register_command, MDI_Register_node, &
       MDI_Register_callback, MDI_COMMAND_LENGTH, MDI_MPI_get_world_comm, &
       MDI_Plugin_get_argc, MDI_Plugin_get_arg, MDI_Get_communicator, &
       MDI_Get_method, MDI_Set_plugin_state, MDI_Send_command, &
       MDI_Open_plugin, MDI_Close_plugin

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307)

  ! MDI Communicator to the driver
  INTEGER :: comm

  ! MPI intra-communicator for this code
  INTEGER :: world_comm

  ! Flag to terminate MDI response function
  LOGICAL :: terminate_flag = .false.

CONTAINS

  FUNCTION driver_plugin_callback(mpi_comm, mdi_comm, class_obj) bind ( C )
    INTEGER, VALUE                  :: mpi_comm
    INTEGER, VALUE                  :: mdi_comm
    TYPE(C_PTR), VALUE, INTENT(IN)  :: class_obj
    INTEGER(KIND=C_INT)             :: driver_plugin_callback

    INTEGER :: ierr, opened_mdi_comm
    CHARACTER(len=:), ALLOCATABLE :: message

    ALLOCATE( CHARACTER(MDI_COMMAND_LENGTH) :: message )

    ! Launch another plugin
    CALL MDI_Open_plugin("engine_cxx", &
                          "-mdi ""-role ENGINE -method LINK -name OPENED""", &
                          mpi_comm, &
                          opened_mdi_comm, &
                          ierr)

    CALL MDI_Send_command("<NAME", opened_mdi_comm, ierr)
    CALL MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, opened_mdi_comm, ierr)
    WRITE(6,*)"Opened engine name: ",TRIM(message)

    CALL MDI_Send_command("EXIT", opened_mdi_comm, ierr)

    CALL MDI_Close_plugin(opened_mdi_comm, ierr)

    CALL MDI_Send_command("<NAME", mdi_comm, ierr)
    CALL MDI_Recv(message, MDI_NAME_LENGTH, MDI_CHAR, mdi_comm, ierr)
    WRITE(6,*)"Engine name: ",TRIM(message)

    CALL MDI_Send_command("EXIT", mdi_comm, ierr)

    DEALLOCATE( message )

    driver_plugin_callback = 0

  END FUNCTION driver_plugin_callback

END MODULE MDI_IMPLEMENTATION
