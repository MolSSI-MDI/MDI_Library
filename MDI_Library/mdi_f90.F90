MODULE MDI_GLOBAL

   USE ISO_C_BINDING

   INTEGER(KIND=C_INT), PARAMETER :: COMMAND_LENGTH = 12
   INTEGER(KIND=C_INT), PARAMETER :: NAME_LENGTH    = 12

END MODULE

MODULE MDI_INTERNAL
  USE ISO_C_BINDING
  USE MDI_GLOBAL

  IMPLICIT NONE

  ! The execute_command callbacks are implemented in a pseudo-dictionary
  ! Each key corresponds to the handle of one of the codes that has called MDI_Init
  ! The values are the actual procedure pointers
  TYPE command_func_ptr
     INTEGER                                     :: key
     PROCEDURE(execute_command), POINTER, NOPASS :: value => null()
  END TYPE command_func_ptr
  TYPE(command_func_ptr), ALLOCATABLE :: execute_commands(:)

  ABSTRACT INTERFACE
    SUBROUTINE execute_command(buf, comm, ierr)
      CHARACTER(LEN=*), INTENT(IN) :: buf
      INTEGER, INTENT(IN)          :: comm
      INTEGER, INTENT(OUT)         :: ierr
    END SUBROUTINE execute_command
  END INTERFACE

  INTERFACE

     FUNCTION MDI_Set_Execute_Command_Func_c(command_func, class_obj) bind(c, name="MDI_Set_Execute_Command_Func")
       USE ISO_C_BINDING
       TYPE(C_FUNPTR), VALUE, INTENT(IN)        :: command_func
       TYPE(C_PTR), VALUE                       :: class_obj
       INTEGER(KIND=C_INT)                      :: MDI_Set_Execute_Command_Func_c
     END FUNCTION MDI_Set_Execute_Command_Func_c

     FUNCTION MDI_Get_Current_Code_() bind(c, name="MDI_Get_Current_Code")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT)                      :: MDI_Get_Current_Code_
     END FUNCTION MDI_Get_Current_Code_

  END INTERFACE

CONTAINS

  FUNCTION str_c_to_f(cbuf, str_len)
    INTEGER                                  :: str_len
    CHARACTER(LEN=str_len)                   :: str_c_to_f
    CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(str_len)

    INTEGER                                  :: i
    LOGICAL                                  :: end_string
    CHARACTER(LEN=str_len)                   :: fbuf

    ! convert from C string to Fortran string
    fbuf = ""
    end_string = .false.
    DO i = 1, str_len
       IF ( cbuf(i) == c_null_char ) end_string = .true.
       IF ( end_string ) THEN
          fbuf(i:i) = ' '
       ELSE
          fbuf(i:i) = cbuf(i)
       END IF
    ENDDO
    str_c_to_f = fbuf
  END FUNCTION str_c_to_f

  FUNCTION str_f_to_c(fbuf, str_len)
    INTEGER                                  :: str_len
    CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: str_f_to_c(str_len)
    CHARACTER(LEN=*)                   :: fbuf

    INTEGER                                  :: i, count
    CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(str_len)

    DO i = 1, LEN_TRIM(fbuf)
       cbuf(i) = fbuf(i:i)
    END DO
    cbuf( LEN_TRIM(fbuf) + 1 ) = c_null_char
    str_f_to_c = cbuf
  END FUNCTION str_f_to_c

  ! Return the index in execute_commands that corresponds to the key argument
  FUNCTION find_execute_command(key)
    INTEGER, INTENT(IN)                      :: key
    INTEGER                                  :: find_execute_command
    INTEGER                                  :: index

    ! Check if the execute_commands dictionary has been allocated
    IF ( .not. ALLOCATED(execute_commands) ) THEN
      find_execute_command = -1
      RETURN
    END IF

    index = 1
    DO WHILE( (index .le. SIZE(execute_commands)) .and. (execute_commands(index)%key .ne. key) )
      index = index + 1
    END DO

    IF ( index .gt. SIZE(execute_commands) ) THEN
      find_execute_command = -1
      RETURN
    END IF

    find_execute_command = index
  END FUNCTION find_execute_command

  ! Add a value to the execute_command dictionary
  SUBROUTINE add_execute_command(key, value)
    INTEGER, INTENT(IN)                      :: key
    PROCEDURE(execute_command)               :: value
    INTEGER                                  :: index
    TYPE(command_func_ptr), ALLOCATABLE      :: temp_dict(:)

    IF ( .not. ALLOCATED( execute_commands ) ) THEN
       ! Just allocate the key-value arrays with a size of one
       ALLOCATE( execute_commands(1) )
    ELSE
      ! Confirm that this key does not already exist
      index = find_execute_command( key )
      IF ( index .ne. -1 ) THEN
        WRITE(6,*)'MDI ERROR: Value already exists in execute_command dictionary'
      END IF

      ! Store the execute_commands data in a temporary array
      ALLOCATE( temp_dict( SIZE(execute_commands) ) )
      temp_dict = execute_commands
      
      ! Reallocate execute_commands to the correct size
      DEALLOCATE( execute_commands )
      ALLOCATE( execute_commands( SIZE(temp_dict) + 1 ) )
      execute_commands(1:SIZE(temp_dict)) = temp_dict
      DEALLOCATE( temp_dict )
    END IF

    ! Add the key-value pair
    execute_commands( SIZE(execute_commands) )%key = key
    execute_commands( SIZE(execute_commands) )%value => value

  END SUBROUTINE add_execute_command

  ! Remove a value from the execute_command dictionary
  SUBROUTINE remove_execute_command(key)
    INTEGER, INTENT(IN)                      :: key
    INTEGER                                  :: index
    TYPE(command_func_ptr), ALLOCATABLE      :: temp_dict(:)

    index = find_execute_command( key )

    ! Ensure that this key was actually found in the execute_command dictionary
    IF ( index .eq. -1 ) THEN
      RETURN
    END IF

    ! Store the execute_commands data in a temporary array
    ALLOCATE( temp_dict( SIZE(execute_commands) ) )
    temp_dict = execute_commands

    ! Replace the deleted element with the last element
    temp_dict(index) = temp_dict( SIZE(temp_dict) )
      
    ! Reallocate execute_commands to the correct size
    DEALLOCATE( execute_commands )
    ALLOCATE( execute_commands( SIZE(temp_dict) - 1 ) )
    execute_commands(1:SIZE(execute_commands)) = temp_dict
    DEALLOCATE( temp_dict )

  END SUBROUTINE remove_execute_command

  FUNCTION MDI_Execute_Command_f(buf, comm) bind(c)
    CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: buf(COMMAND_LENGTH)
    INTEGER(KIND=C_INT), VALUE               :: comm
    INTEGER(KIND=C_INT)                      :: MDI_Execute_Command_f

    CHARACTER(LEN=COMMAND_LENGTH)        :: fbuf
    INTEGER                                  :: commf
    INTEGER                                  :: ierr

    INTEGER                                  :: i, current_code
    LOGICAL                                  :: end_string

    commf = comm

    ! convert from C string to Fortran string
    fbuf = str_c_to_f(buf, COMMAND_LENGTH)

    ! Get the correct execute_command callback
    current_code = MDI_Get_Current_Code_()
    i = find_execute_command( current_code )
    IF ( i .eq. -1 ) THEN
      WRITE(6,*)'MDI Error: Could not locate correct execute_command callback'
    END IF
    call execute_commands(i)%value(fbuf, commf, ierr)

    ! If this is the EXIT command, delete all Fortran state associated with the code
    IF ( TRIM(fbuf) .eq. "EXIT" ) THEN
      CALL remove_execute_command( current_code )
    END IF

    MDI_Execute_Command_f = ierr

  END FUNCTION MDI_Execute_Command_f

END MODULE





! Fortran 90 wrapper for the MolSSI Driver Interface

MODULE MDI
   USE ISO_C_BINDING
   USE MDI_GLOBAL

   IMPLICIT NONE

   INTEGER(KIND=C_INT), PARAMETER :: MDI_COMMAND_LENGTH = COMMAND_LENGTH
   INTEGER(KIND=C_INT), PARAMETER :: MDI_NAME_LENGTH    = NAME_LENGTH
   INTEGER(KIND=C_INT), PARAMETER :: MDI_NULL_COMM      = 0

   INTEGER(KIND=C_INT), PARAMETER :: MDI_INT            = 1
   INTEGER(KIND=C_INT), PARAMETER :: MDI_DOUBLE         = 2
   INTEGER(KIND=C_INT), PARAMETER :: MDI_CHAR           = 3
   INTEGER(KIND=C_INT), PARAMETER :: MDI_INT_NUMPY      = 4
   INTEGER(KIND=C_INT), PARAMETER :: MDI_DOUBLE_NUMPY   = 5

   INTEGER(KIND=C_INT), PARAMETER :: MDI_TCP            = 1
   INTEGER(KIND=C_INT), PARAMETER :: MDI_MPI            = 2
   INTEGER(KIND=C_INT), PARAMETER :: MDI_LIB            = 3
   INTEGER(KIND=C_INT), PARAMETER :: MDI_TEST           = 4

   INTEGER(KIND=C_INT), PARAMETER :: MDI_DRIVER         = 1
   INTEGER(KIND=C_INT), PARAMETER :: MDI_ENGINE         = 2

   INTEGER(KIND=C_INT), PROTECTED, BIND(C, name="MDI_MAJOR_VERSION")         :: MDI_MAJOR_VERSION
   INTEGER(KIND=C_INT), PROTECTED, BIND(C, name="MDI_MINOR_VERSION")         :: MDI_MINOR_VERSION
   INTEGER(KIND=C_INT), PROTECTED, BIND(C, name="MDI_PATCH_VERSION")         :: MDI_PATCH_VERSION

  INTERFACE MDI_Send
      MODULE PROCEDURE MDI_Send_s, &
                       MDI_Send_d, MDI_Send_dv, &
                       MDI_Send_i, MDI_Send_iv
  END INTERFACE 

  INTERFACE MDI_Recv
      MODULE PROCEDURE MDI_Recv_s, &
                       MDI_Recv_d, MDI_Recv_dv, &
                       MDI_Recv_i, MDI_Recv_iv
  END INTERFACE 

  INTERFACE

     FUNCTION MDI_Init_(options, world_comm) bind(c, name="MDI_Init")
       USE, INTRINSIC :: iso_c_binding
       CHARACTER(C_CHAR)                        :: options(*)
       INTEGER(KIND=C_INT)                      :: world_comm
       INTEGER(KIND=C_INT)                      :: MDI_Init_
     END FUNCTION MDI_Init_

     FUNCTION MDI_Accept_Communicator_(comm) bind(c, name="MDI_Accept_Communicator")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: comm
       INTEGER(KIND=C_INT)                      :: MDI_Accept_Communicator_
     END FUNCTION MDI_Accept_Communicator_

     FUNCTION MDI_Send_(buf, count, datatype, comm) BIND(C, name="MDI_Send")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT), VALUE               :: count, datatype, comm
       TYPE(C_PTR), VALUE                       :: buf
       INTEGER(KIND=C_INT)                      :: MDI_Send_
     END FUNCTION MDI_Send_

     FUNCTION MDI_Recv_(buf, count, datatype, comm) BIND(C, name="MDI_Recv")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT), VALUE               :: count, datatype, comm
       TYPE(C_PTR), VALUE                       :: buf
       INTEGER(KIND=C_INT)                      :: MDI_Recv_
     END FUNCTION MDI_Recv_

     FUNCTION MDI_Send_Command_(buf, comm) bind(c, name="MDI_Send_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: buf
       INTEGER(KIND=C_INT), VALUE               :: comm
       INTEGER(KIND=C_INT)                      :: MDI_Send_Command_
     END FUNCTION MDI_Send_Command_

     FUNCTION MDI_Recv_Command_(buf, comm) bind(c, name="MDI_Recv_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: buf
       INTEGER(KIND=C_INT), VALUE               :: comm
       INTEGER(KIND=C_INT)                      :: MDI_Recv_Command_
     END FUNCTION MDI_Recv_Command_

     FUNCTION MDI_Conversion_Factor_(in_unit, out_unit, conv) bind(c, name="MDI_Conversion_Factor")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: in_unit, out_unit, conv
       INTEGER(KIND=C_INT)                      :: MDI_Conversion_Factor_
     END FUNCTION MDI_Conversion_Factor_

     FUNCTION MDI_Get_Role_(role) bind(c, name="MDI_Get_Role")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: role
       INTEGER(KIND=C_INT)                      :: MDI_Get_Role_
     END FUNCTION MDI_Get_Role_

     SUBROUTINE MDI_Set_Execute_Command_Func(command_func, class_obj, ierr)
       USE MDI_INTERNAL
       PROCEDURE(execute_command)               :: command_func 
       TYPE(C_PTR), VALUE                       :: class_obj
       INTEGER, INTENT(OUT)                     :: ierr
     END SUBROUTINE MDI_Set_Execute_Command_Func

     FUNCTION MDI_Register_Node_(node) bind(c, name="MDI_Register_Node")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT)                      :: MDI_Register_Node_
     END FUNCTION MDI_Register_Node_

     FUNCTION MDI_Check_Node_Exists_(node, comm, flag) bind(c, name="MDI_Check_Node_Exists")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: flag
       INTEGER(KIND=C_INT)                      :: MDI_Check_Node_Exists_
     END FUNCTION MDI_Check_Node_Exists_

     FUNCTION MDI_Get_NNodes_(comm, nnodes) bind(c, name="MDI_Get_NNodes")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: nnodes
       INTEGER(KIND=C_INT)                      :: MDI_Get_NNodes_
     END FUNCTION MDI_Get_NNodes_

     FUNCTION MDI_Get_Node_(index, comm, node) bind(c, name="MDI_Get_Node")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT), VALUE               :: index
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT)                      :: MDI_Get_Node_
     END FUNCTION MDI_Get_Node_

     FUNCTION MDI_Register_Command_(node, command) bind(c, name="MDI_Register_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       TYPE(C_PTR), VALUE                       :: command
       INTEGER(KIND=C_INT)                      :: MDI_Register_Command_
     END FUNCTION MDI_Register_Command_

     FUNCTION MDI_Check_Command_Exists_(node, command, comm, flag) bind(c, name="MDI_Check_Command_Exists")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       TYPE(C_PTR), VALUE                       :: command
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: flag
       INTEGER(KIND=C_INT)                      :: MDI_Check_Command_Exists_
     END FUNCTION MDI_Check_Command_Exists_

     FUNCTION MDI_Get_NCommands_(node, comm, ncommands) bind(c, name="MDI_Get_NCommands")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: ncommands
       INTEGER(KIND=C_INT)                      :: MDI_Get_NCommands_
     END FUNCTION MDI_Get_NCommands_

     FUNCTION MDI_Get_Command_(node, index, comm, command) bind(c, name="MDI_Get_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT), VALUE               :: index
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: command
       INTEGER(KIND=C_INT)                      :: MDI_Get_Command_
     END FUNCTION MDI_Get_Command_

     FUNCTION MDI_Register_Callback_(node, callback) bind(c, name="MDI_Register_Callback")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       TYPE(C_PTR), VALUE                       :: callback
       INTEGER(KIND=C_INT)                      :: MDI_Register_Callback_
     END FUNCTION MDI_Register_Callback_

     FUNCTION MDI_Check_Callback_Exists_(node, callback, comm, flag) bind(c, name="MDI_Check_Callback_Exists")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       TYPE(C_PTR), VALUE                       :: callback
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: flag
       INTEGER(KIND=C_INT)                      :: MDI_Check_Callback_Exists_
     END FUNCTION MDI_Check_Callback_Exists_

     FUNCTION MDI_Get_NCallbacks_(node, comm, ncallbacks) bind(c, name="MDI_Get_NCallbacks")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: ncallbacks
       INTEGER(KIND=C_INT)                      :: MDI_Get_NCallbacks_
     END FUNCTION MDI_Get_NCallbacks_

     FUNCTION MDI_Get_Callback_(node, index, comm, callback) bind(c, name="MDI_Get_Callback")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: node
       INTEGER(KIND=C_INT), VALUE               :: index
       INTEGER(KIND=C_INT), VALUE               :: comm
       TYPE(C_PTR), VALUE                       :: callback
       INTEGER(KIND=C_INT)                      :: MDI_Get_Callback_
     END FUNCTION MDI_Get_Callback_

  END INTERFACE



CONTAINS

    SUBROUTINE MDI_Init(foptions, fworld_comm, ierr)
      IMPLICIT NONE
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Init
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Init
#endif
      CHARACTER(LEN=*), INTENT(IN) :: foptions
      INTEGER, INTENT(INOUT) :: fworld_comm
      INTEGER, INTENT(OUT) :: ierr

      ierr = MDI_Init_( TRIM(foptions)//" _language Fortran"//c_null_char, fworld_comm )
    END SUBROUTINE MDI_Init

    SUBROUTINE MDI_Accept_Communicator(communicator, ierr)
      IMPLICIT NONE
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Accept_Communicator
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Accept_Communicator
#endif
      INTEGER, INTENT(OUT) :: communicator
      INTEGER, INTENT(OUT) :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cbuf

      ierr = MDI_Accept_Communicator_(c_loc(cbuf))
      communicator = cbuf
    END SUBROUTINE MDI_Accept_Communicator

    SUBROUTINE MDI_Send_s (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_s
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_s
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      CHARACTER(LEN=*), INTENT(IN)             :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(count)

      cbuf = str_f_to_c(fbuf, count)

      ierr = MDI_Send_( c_loc(cbuf), count, datatype, comm)
    END SUBROUTINE MDI_Send_s

    SUBROUTINE MDI_Send_d (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_d
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_d
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(IN)                 :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cbuf

      cbuf = fbuf
      ierr = MDI_Send_(c_loc(cbuf), 1, datatype, comm)
    END SUBROUTINE MDI_Send_d

    SUBROUTINE MDI_Send_dv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING  
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_dv
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_dv
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(IN), TARGET         :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Send_dv

    SUBROUTINE MDI_Send_i (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_i
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_i
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER, INTENT(IN)                      :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cbuf

      cbuf = fbuf
      ierr = MDI_Send_(c_loc(cbuf), 1, datatype, comm)
    END SUBROUTINE MDI_Send_i

    SUBROUTINE MDI_Send_iv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_iv
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_iv
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER(KIND=C_INT), TARGET              :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Send_iv

    SUBROUTINE MDI_Recv_s (fbuf, count, datatype, comm, ierr)
      USE MDI_INTERNAL, ONLY : str_c_to_f
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_s
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_s
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      CHARACTER(LEN=*), INTENT(OUT)            :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(count)

      ierr = MDI_Recv_(c_loc(cbuf(1)), count, datatype, comm)

      ! convert from C string to Fortran string
       fbuf = str_c_to_f(cbuf, count)
    END SUBROUTINE MDI_Recv_s

    SUBROUTINE MDI_Recv_d (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_d
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_d
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(OUT)                :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cbuf

      ierr = MDI_Recv_(c_loc(cbuf), 1, datatype, comm)
      fbuf = cbuf
    END SUBROUTINE MDI_Recv_d

    SUBROUTINE MDI_Recv_dv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING  
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_dv
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_dv
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(OUT), TARGET        :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Recv_dv

    SUBROUTINE MDI_Recv_i (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_i
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_i
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER, INTENT(OUT)                     :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cbuf

      ierr = MDI_Recv_(c_loc(cbuf), 1, datatype, comm)
      fbuf = cbuf
    END SUBROUTINE MDI_Recv_i

    SUBROUTINE MDI_Recv_iv (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_iv
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_iv
#endif
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Recv_iv

    SUBROUTINE MDI_Send_Command(fbuf, comm, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Send_Command
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Send_Command
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fbuf
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(MDI_COMMAND_LENGTH)

       cbuf = str_f_to_c(fbuf, MDI_COMMAND_LENGTH)

      ierr = MDI_Send_Command_( c_loc(cbuf), comm)
    END SUBROUTINE MDI_Send_Command

    SUBROUTINE MDI_Recv_Command(fbuf, comm, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : MDI_Get_Current_Code_, remove_execute_command, str_c_to_f
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_Command
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Recv_Command
#endif
      CHARACTER(LEN=*), INTENT(OUT)            :: fbuf
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i, current_code
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(MDI_COMMAND_LENGTH)

      ierr = MDI_Recv_Command_(c_loc(cbuf(1)), comm)

      ! convert from C string to Fortran string
      fbuf = str_c_to_f(cbuf, MDI_COMMAND_LENGTH)

      ! If this is the EXIT command, delete all Fortran state associated with the code
      IF ( TRIM(fbuf) .eq. "EXIT" ) THEN
        current_code = MDI_Get_Current_Code_()
        CALL remove_execute_command( current_code )
      END IF
    END SUBROUTINE MDI_Recv_Command

    SUBROUTINE MDI_Conversion_Factor(fin_unit, fout_unit, factor, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Conversion_Factor
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Conversion_Factor
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fin_unit, fout_unit
      DOUBLE PRECISION, INTENT(OUT)            :: factor
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cin_unit(LEN_TRIM(fin_unit)+1)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cout_unit(LEN_TRIM(fout_unit)+1)
      REAL(KIND=C_DOUBLE), TARGET              :: cfactor

      DO i = 1, LEN_TRIM(fin_unit)
         cin_unit(i) = fin_unit(i:i)
      END DO
      cin_unit( LEN_TRIM(fin_unit) + 1 ) = c_null_char

      DO i = 1, LEN_TRIM(fout_unit)
         cout_unit(i) = fout_unit(i:i)
      END DO
      cout_unit( LEN_TRIM(fout_unit) + 1 ) = c_null_char

      ierr = MDI_Conversion_Factor_( c_loc(cin_unit), c_loc(cout_unit), c_loc(cfactor) )
      factor = cfactor
    END SUBROUTINE MDI_Conversion_Factor

    SUBROUTINE MDI_Get_Role(role, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Role
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Role
#endif
      INTEGER, INTENT(OUT)                     :: role
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: crole

      ierr = MDI_Get_Role_( c_loc(crole) )
      role = crole
    END SUBROUTINE MDI_Get_Role

    SUBROUTINE MDI_Register_Node(fnode, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Node
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Node
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)

      ierr = MDI_Register_Node_( c_loc(cnode) )
    END SUBROUTINE MDI_Register_Node

    SUBROUTINE MDI_Check_Node_Exists(fnode, comm, flag, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Node_Exists
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Node_Exists
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: flag
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cflag
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)

      ierr = MDI_Check_Node_Exists_( c_loc(cnode), comm, c_loc(cflag) )
      flag = cflag
    END SUBROUTINE MDI_Check_Node_Exists

    SUBROUTINE MDI_Get_NNodes(comm, nnodes, ierr)
      USE ISO_C_BINDING
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NNodes
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NNodes
#endif
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: nnodes
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cnnodes

      ierr = MDI_Get_NNodes_( comm, c_loc(cnnodes) )
      nnodes = cnnodes
    END SUBROUTINE MDI_Get_NNodes

    SUBROUTINE MDI_Get_Node(index, comm, fnode, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_c_to_f
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Node
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Node
#endif
      INTEGER, INTENT(IN)                      :: index
      INTEGER, INTENT(IN)                      :: comm
      CHARACTER(LEN=*), INTENT(OUT)            :: fnode
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)

      ierr = MDI_Get_Node_( index, comm, c_loc(cnode) )
      fnode = str_c_to_f(cnode, MDI_COMMAND_LENGTH)
    END SUBROUTINE MDI_Get_Node

    SUBROUTINE MDI_Register_Command(fnode, fcommand, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Command
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Command
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      CHARACTER(LEN=*), INTENT(IN)             :: fcommand
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccommand(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ccommand = str_f_to_c(fcommand, MDI_COMMAND_LENGTH)

      ierr = MDI_Register_Command_( c_loc(cnode), c_loc(ccommand) )
    END SUBROUTINE MDI_Register_Command

    SUBROUTINE MDI_Check_Command_Exists(fnode, fcommand, comm, flag, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Command_Exists
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Command_Exists
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      CHARACTER(LEN=*), INTENT(IN)             :: fcommand
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: flag
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cflag
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccommand(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ccommand = str_f_to_c(fcommand, MDI_COMMAND_LENGTH)

      ierr = MDI_Check_Command_Exists_( c_loc(cnode), c_loc(ccommand), comm, c_loc(cflag) )
      flag = cflag
    END SUBROUTINE MDI_Check_Command_Exists

    SUBROUTINE MDI_Get_NCommands(fnode, comm, ncommands, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NCommands
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NCommands
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ncommands
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cncommands
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)

      ierr = MDI_Get_NCommands_( c_loc(cnode), comm, c_loc(cncommands) )
      ncommands = cncommands
    END SUBROUTINE MDI_Get_NCommands

    SUBROUTINE MDI_Get_Command(fnode, index, comm, fcommand, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_c_to_f, str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Command
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Command
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(IN)                      :: index
      INTEGER, INTENT(IN)                      :: comm
      CHARACTER(LEN=*), INTENT(OUT)            :: fcommand
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccommand(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ierr = MDI_Get_Command_( c_loc(cnode), index, comm, c_loc(ccommand) )
      fcommand = str_c_to_f(ccommand, MDI_COMMAND_LENGTH)
    END SUBROUTINE MDI_Get_Command

    SUBROUTINE MDI_Register_Callback(fnode, fcallback, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Callback
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Register_Callback
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      CHARACTER(LEN=*), INTENT(IN)             :: fcallback
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccallback(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ccallback = str_f_to_c(fcallback, MDI_COMMAND_LENGTH)

      ierr = MDI_Register_Callback_( c_loc(cnode), c_loc(ccallback) )
    END SUBROUTINE MDI_Register_Callback

    SUBROUTINE MDI_Check_Callback_Exists(fnode, fcallback, comm, flag, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Callback_Exists
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Check_Callback_Exists
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      CHARACTER(LEN=*), INTENT(IN)             :: fcallback
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: flag
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cflag
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccallback(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ccallback = str_f_to_c(fcallback, MDI_COMMAND_LENGTH)

      ierr = MDI_Check_Callback_Exists_( c_loc(cnode), c_loc(ccallback), comm, c_loc(cflag) )
      flag = cflag
    END SUBROUTINE MDI_Check_Callback_Exists

    SUBROUTINE MDI_Get_NCallbacks(fnode, comm, ncallbacks, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NCallbacks
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_NCallbacks
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ncallbacks
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cncallbacks
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)

      ierr = MDI_Get_NCallbacks_( c_loc(cnode), comm, c_loc(cncallbacks) )
      ncallbacks = cncallbacks
    END SUBROUTINE MDI_Get_NCallbacks

    SUBROUTINE MDI_Get_Callback(fnode, index, comm, fcallback, ierr)
      USE ISO_C_BINDING
      USE MDI_INTERNAL, ONLY : str_c_to_f, str_f_to_c
#if MDI_WINDOWS
      !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Callback
      !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Get_Callback
#endif
      CHARACTER(LEN=*), INTENT(IN)             :: fnode
      INTEGER, INTENT(IN)                      :: index
      INTEGER, INTENT(IN)                      :: comm
      CHARACTER(LEN=*), INTENT(OUT)            :: fcallback
      INTEGER, INTENT(OUT)                     :: ierr

      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cnode(MDI_COMMAND_LENGTH)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: ccallback(MDI_COMMAND_LENGTH)

      cnode = str_f_to_c(fnode, MDI_COMMAND_LENGTH)
      ierr = MDI_Get_Callback_( c_loc(cnode), index, comm, c_loc(ccallback) )
      fcallback = str_c_to_f(ccallback, MDI_COMMAND_LENGTH)
    END SUBROUTINE MDI_Get_Callback

END MODULE





SUBROUTINE MDI_Set_Execute_Command_Func(command_func, class_obj, ierr)
  USE MDI_INTERNAL

#if MDI_WINDOWS
    !GCC$ ATTRIBUTES DLLEXPORT :: MDI_Set_Execute_Command_Func
    !DEC$ ATTRIBUTES DLLEXPORT :: MDI_Set_Execute_Command_Func
#endif
    PROCEDURE(execute_command)               :: command_func
    TYPE(C_PTR), VALUE                       :: class_obj
    INTEGER, INTENT(OUT)                     :: ierr
    INTEGER                                  :: current_code

    current_code = MDI_Get_Current_Code_()

    CALL add_execute_command(current_code, command_func)
    ierr = MDI_Set_Execute_Command_Func_c( c_funloc(MDI_Execute_Command_f), class_obj )

END SUBROUTINE MDI_Set_Execute_Command_Func
