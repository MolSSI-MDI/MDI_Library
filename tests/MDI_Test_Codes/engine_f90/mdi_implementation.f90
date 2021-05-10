MODULE MDI_IMPLEMENTATION

  USE mpi
  USE ISO_C_binding
  USE mdi,              ONLY : MDI_Init, MDI_Send, MDI_INT, MDI_CHAR, MDI_NAME_LENGTH, &
       MDI_Accept_communicator, MDI_Recv_command, MDI_Recv, &
       MDI_Set_execute_command_func, MDI_MPI_get_world_comm, MDI_DOUBLE, MDI_BYTE, &
       MDI_ENGINE, MDI_Get_role, MDI_Register_command, MDI_Register_node, &
       MDI_Register_callback, MDI_COMMAND_LENGTH, MDI_MPI_get_world_comm, &
       MDI_Plugin_get_argc, MDI_Plugin_get_arg

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = selected_real_kind(15, 307)

  ! MDI Communicator to the driver
  INTEGER :: comm

  ! MPI intra-communicator for this code
  INTEGER :: world_comm

  ! Flag to terminate MDI response function
  LOGICAL :: terminate_flag = .false.

CONTAINS

  FUNCTION MDI_Plugin_init_engine_f90() bind ( C, name="MDI_Plugin_init_engine_f90" )
    INTEGER :: MDI_Plugin_init_engine_f90
    INTEGER :: ierr
    INTEGER :: argc
    INTEGER :: iarg
    CHARACTER(LEN=1024) :: option
    CHARACTER(LEN=1024) :: mdi_options
    LOGICAL :: mdi_options_found

    ! Get the command-line options from the driver
    mdi_options_found = .false.
    CALL MDI_Plugin_get_argc(argc, ierr)
    DO iarg=0, argc-1
       CALL MDI_Plugin_get_arg(iarg, option, ierr)
       IF ( (TRIM(option) .eq. "-mdi") .or. (TRIM(option) .eq. "--mdi") ) THEN
          IF ( argc .gt. (iarg+1) ) THEN
             CALL MDI_Plugin_get_arg(iarg+1, mdi_options, ierr)
             mdi_options_found = .true.
          ELSE
             WRITE(6,*)'ERROR: argument to -mdi option not provided'
             MDI_Plugin_init_engine_f90 = 1
             RETURN
          END IF
       END IF
    END DO
    IF ( .not. mdi_options_found ) THEN
       WRITE(6,*)'ERROR: -mdi option not provided'
       MDI_Plugin_init_engine_f90 = 1
       RETURN
    END IF

    ! Call MDI_Init
    world_comm = MPI_COMM_WORLD
    CALL MDI_Init(mdi_options, ierr)

    ! Get the MPI intra-communicator over which this plugin will run
    CALL MDI_MPI_get_world_comm(world_comm, ierr);

    ! Perform one-time operations required to establish a connection with the driver
    CALL initialize_mdi()

    ! Respond to commands from the driver
    CALL respond_to_commands()

    MDI_Plugin_init_engine_f90 = 0
  END FUNCTION MDI_Plugin_init_engine_f90


  SUBROUTINE initialize_mdi()
    INTEGER :: ierr, role

    PROCEDURE(execute_command), POINTER :: generic_command => null()
    TYPE(C_PTR)                         :: class_obj
    generic_command => execute_command

    ! Confirm that the code is being run as an ENGINE
    call MDI_Get_role(role, ierr)
    IF ( role .ne. MDI_ENGINE ) THEN
       WRITE(6,*)'ERROR: Must run engine_f90 as an ENGINE'
    END IF

    ! Register the commands
    CALL MDI_Register_node("@DEFAULT", ierr)
    CALL MDI_Register_command("@DEFAULT", "EXIT", ierr)
    CALL MDI_Register_command("@DEFAULT", "<NATOMS", ierr)
    CALL MDI_Register_command("@DEFAULT", "<COORDS", ierr)
    CALL MDI_Register_command("@DEFAULT", "<FORCES", ierr)
    CALL MDI_Register_command("@DEFAULT", "<FORCES_B", ierr)
    CALL MDI_Register_node("@FORCES", ierr)
    CALL MDI_Register_command("@FORCES", "EXIT", ierr)
    CALL MDI_Register_command("@FORCES", "<FORCES", ierr)
    CALL MDI_Register_command("@FORCES", ">FORCES", ierr)
    CALL MDI_Register_callback("@FORCES", ">FORCES", ierr)

    ! Connct to the driver
    CALL MDI_Accept_communicator(comm, ierr)

    ! Set the generic execute_command function
    CALL MDI_Set_execute_command_func(generic_command, class_obj, ierr)

  END SUBROUTINE initialize_mdi


  SUBROUTINE respond_to_commands()
    CHARACTER(len=:), ALLOCATABLE :: command
    INTEGER :: ierr

    ALLOCATE( character(MDI_COMMAND_LENGTH) :: command )

    ! Respond to the driver's commands
    response_loop: DO

       ! Receive a command from the driver and broadcast it to all ranks
       CALL MDI_Recv_command(command, comm, ierr)
       CALL MPI_Bcast(command, MDI_COMMAND_LENGTH, MPI_CHAR, 0, world_comm, ierr)

       CALL execute_command(command, comm, ierr)

       IF ( terminate_flag ) EXIT

    END DO response_loop

    DEALLOCATE( command )

  END SUBROUTINE respond_to_commands


  SUBROUTINE execute_command(command, comm, ierr)
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: command
    INTEGER, INTENT(IN)           :: comm
    INTEGER, INTENT(OUT)          :: ierr

    INTEGER                       :: icoord
    INTEGER                       :: natoms, count
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
    CASE( "<FORCES_B" )
       count = 3 * natoms * sizeof(1.d0)
       CALL MDI_Send(forces, count, MDI_BYTE, comm, ierr)
    CASE DEFAULT
       WRITE(6,*)'Error: command not recognized'
    END SELECT

    DEALLOCATE( coords, forces )

    ierr = 0
  END SUBROUTINE execute_command

END MODULE MDI_IMPLEMENTATION
