! Fortran 90 wrapper for the MolSSI Driver Interface

   MODULE MDI
   USE ISO_C_BINDING

   IMPLICIT NONE

   INTEGER, PROTECTED, BIND(C, name="MDI_COMMAND_LENGTH")        :: MDI_COMMAND_LENGTH
   INTEGER, PROTECTED, BIND(C, name="MDI_NAME_LENGTH")           :: MDI_NAME_LENGTH

   INTEGER, PROTECTED, BIND(C, name="MDI_INT")                   :: MDI_INT
   INTEGER, PROTECTED, BIND(C, name="MDI_DOUBLE")                :: MDI_DOUBLE
   INTEGER, PROTECTED, BIND(C, name="MDI_CHAR")                  :: MDI_CHAR

   INTEGER, PROTECTED, BIND(C, name="MDI_TCP")                   :: MDI_TCP
   INTEGER, PROTECTED, BIND(C, name="MDI_MPI")                   :: MDI_MPI

   !----------------------!
   ! MDI unit conversions !
   !----------------------!

   ! length
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_METER_TO_BOHR")         :: MDI_METER_TO_BOHR
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_ANGSTROM_TO_BOHR")      :: MDI_ANGSTROM_TO_BOHR

   ! time
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_SECOND_TO_AUT")         :: MDI_SECOND_TO_AUT
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_PICOSECOND_TO_AUT")     :: MDI_PICOSECOND_TO_AUT

   ! force
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_NEWTON_TO_AUF")         :: MDI_NEWTON_TO_AUF

   ! energy
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_JOULE_TO_HARTREE")      :: MDI_JOULE_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_KJ_TO_HARTREE")         :: MDI_KJ_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_KJPERMOL_TO_HARTREE")   :: MDI_KJPERMOL_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_KCALPERMOL_TO_HARTREE") :: MDI_KCALPERMOL_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_EV_TO_HARTREE")         :: MDI_EV_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_RYDBERG_TO_HARTREE")    :: MDI_RYDBERG_TO_HARTREE
   REAL(KIND=8), PROTECTED, BIND(C, name="MDI_KELVIN_TO_HARTREE")     :: MDI_KELVIN_TO_HARTREE

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

     FUNCTION MDI_Init_(options, data, world_comm) bind(c, name="MDI_Init")
       USE, INTRINSIC :: iso_c_binding
       CHARACTER(C_CHAR)                        :: options(*)
       TYPE(C_PTR), VALUE                       :: data
       INTEGER(KIND=C_INT)                      :: world_comm
       INTEGER(KIND=C_INT)                      :: MDI_Init_
     END FUNCTION MDI_Init_

     FUNCTION MDI_Request_Connection_(method, options, world_comm) BIND(C, name="MDI_Request_Connection")
       USE ISO_C_BINDING
       CHARACTER(C_CHAR)                        :: method(*)
       TYPE(C_PTR), VALUE                       :: options
       TYPE(C_PTR), VALUE                       :: world_comm
       INTEGER(KIND=C_INT)                      :: MDI_Request_Connection_
     END FUNCTION MDI_Request_Connection_

     FUNCTION MDI_Accept_Connection_() bind(c, name="MDI_Accept_Connection")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT)                      :: MDI_Accept_Connection_
     END FUNCTION MDI_Accept_Connection_

     FUNCTION MDI_MPI_Comm_(comm) bind(c, name="MDI_MPI_Comm")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT)                      :: comm
       INTEGER(KIND=C_INT)                      :: MDI_MPI_Comm_
     END FUNCTION MDI_MPI_Comm_

     FUNCTION MDI_Send_(data_ptr, len, type, sockfd) BIND(C, name="MDI_Send")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT), VALUE               :: len, type, sockfd
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Send_
     END FUNCTION MDI_Send_

     FUNCTION MDI_Recv_(data_ptr, len, type, sockfd) BIND(C, name="MDI_Recv")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT), VALUE               :: len, type, sockfd
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Recv_
     END FUNCTION MDI_Recv_

     FUNCTION MDI_Send_Command_(data_ptr, sockfd) bind(c, name="MDI_Send_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT), VALUE               :: sockfd
       INTEGER(KIND=C_INT)                      :: MDI_Send_Command_
     END FUNCTION MDI_Send_Command_

     FUNCTION MDI_Recv_Command_(data_ptr, sockfd) bind(c, name="MDI_Recv_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT), VALUE               :: sockfd
       INTEGER(KIND=C_INT)                      :: MDI_Recv_Command_
     END FUNCTION MDI_Recv_Command_

  END INTERFACE



  CONTAINS

    SUBROUTINE MDI_Init(foptions, data, fworld_comm, ierr)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: foptions
      TYPE(C_PTR), INTENT(IN) :: data
      INTEGER, INTENT(INOUT) :: fworld_comm
      INTEGER, INTENT(OUT) :: ierr
      !INTEGER, TARGET :: cworld_comm

      !cworld_comm = fworld_comm
      !ierr = MDI_Init_( TRIM(foptions)//c_null_char, data, c_loc(cworld_comm) )
      ierr = MDI_Init_( TRIM(foptions)//c_null_char, data, fworld_comm )
    END SUBROUTINE MDI_Init

    SUBROUTINE MDI_Request_Connection(fmethod, foptions, fworld_comm, comm)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: fmethod
      CHARACTER(LEN=*), INTENT(IN) :: foptions
      INTEGER, INTENT(IN) :: fworld_comm
      INTEGER, TARGET, INTENT(OUT) :: comm
      INTEGER, TARGET :: cworld_comm

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: coptions( LEN_TRIM(foptions) + 1 )

      DO i = 1, LEN_TRIM(foptions)
         coptions(i) = foptions(i:i)
      END DO
      coptions( LEN_TRIM(foptions) + 1 ) = c_null_char

      cworld_comm = fworld_comm
      comm = MDI_Request_Connection_( TRIM(fmethod)//c_null_char, &
           c_loc(coptions), c_loc(cworld_comm) )
    END SUBROUTINE MDI_Request_Connection

    SUBROUTINE MDI_Accept_Connection(connection)
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: connection

      connection = MDI_Accept_Connection_()
    END SUBROUTINE MDI_Accept_Connection

    SUBROUTINE MDI_MPI_Comm(comm, ierr)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: comm
      INTEGER, INTENT(OUT) :: ierr

      ierr = MDI_MPI_Comm_(comm)
    END SUBROUTINE MDI_MPI_Comm

    SUBROUTINE MDI_Send_s (fstring, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      CHARACTER(LEN=*), INTENT(IN)             :: fstring
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(len)

      DO i = 1, LEN_TRIM(fstring)
         cstring(i) = fstring(i:i)
      END DO
      cstring( LEN_TRIM(fstring) + 1 ) = c_null_char

      ierr = MDI_Send_( c_loc(cstring), len, type, sockfd)
    END SUBROUTINE MDI_Send_s

    SUBROUTINE MDI_Send_d (fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      REAL(KIND=8), INTENT(IN)                 :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cdata

      cdata = fdata
      ierr = MDI_Send_(c_loc(cdata), 1, type, sockfd)
    END SUBROUTINE MDI_Send_d

    SUBROUTINE MDI_Send_dv(fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      REAL(KIND=8), INTENT(IN), TARGET         :: fdata(len)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fdata(1)), len, type, sockfd)
    END SUBROUTINE MDI_Send_dv

    SUBROUTINE MDI_Send_i (fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      INTEGER, INTENT(IN)                      :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cdata

      cdata = fdata
      ierr = MDI_Send_(c_loc(cdata), 1, type, sockfd)
    END SUBROUTINE MDI_Send_i

    SUBROUTINE MDI_Send_iv(fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      INTEGER(KIND=C_INT), TARGET              :: fdata(len)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fdata(1)), len, type, sockfd)
    END SUBROUTINE MDI_Send_iv

    SUBROUTINE MDI_Recv_s (fstring, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      CHARACTER(LEN=*), INTENT(OUT)            :: fstring
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(len)

      ierr = MDI_Recv_(c_loc(cstring(1)), len, type, sockfd)

      ! convert from C string to Fortran string
      fstring = ""
      end_string = .false.
      DO i = 1,len
         IF ( cstring(i) == c_null_char ) end_string = .true.
         IF ( end_string ) THEN
            fstring(i:i) = ' '
         ELSE
            fstring(i:i) = cstring(i)
         END IF
      ENDDO
    END SUBROUTINE MDI_Recv_s

    SUBROUTINE MDI_Recv_d (fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      REAL(KIND=8), INTENT(OUT)                :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cdata

      ierr = MDI_Recv_(c_loc(cdata), 1, type, sockfd)
      fdata=cdata
    END SUBROUTINE MDI_Recv_d

    SUBROUTINE MDI_Recv_dv(fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      REAL(KIND=8), INTENT(OUT), TARGET        :: fdata(len)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fdata(1)), len, type, sockfd)
    END SUBROUTINE MDI_Recv_dv

    SUBROUTINE MDI_Recv_i (fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      INTEGER, INTENT(OUT)                     :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cdata

      ierr = MDI_Recv_(c_loc(cdata), 1, type, sockfd)
      fdata = cdata
    END SUBROUTINE MDI_Recv_i

    SUBROUTINE MDI_Recv_iv (fdata, len, type, sockfd, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len, type, sockfd
      INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: fdata(len)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fdata(1)), len, type, sockfd)
    END SUBROUTINE MDI_Recv_iv

    SUBROUTINE MDI_Send_Command(fstring, sockfd, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(IN)             :: fstring
      INTEGER, INTENT(IN)                      :: sockfd
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(MDI_COMMAND_LENGTH)

      DO i = 1, LEN_TRIM(fstring)
         cstring(i) = fstring(i:i)
      END DO
      cstring( LEN_TRIM(fstring) + 1 ) = c_null_char

      ierr = MDI_Send_Command_( c_loc(cstring), sockfd)
    END SUBROUTINE MDI_Send_Command

    SUBROUTINE MDI_Recv_Command(fstring, sockfd, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(OUT)            :: fstring
      INTEGER, INTENT(IN)                      :: sockfd
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(MDI_COMMAND_LENGTH)

      ierr = MDI_Recv_Command_(c_loc(cstring(1)), sockfd)

      ! convert from C string to Fortran string
      fstring = ""
      end_string = .false.
      DO i = 1, MDI_COMMAND_LENGTH
         IF ( cstring(i) == c_null_char ) end_string = .true.
         IF ( end_string ) THEN
            fstring(i:i) = ' '
         ELSE
            fstring(i:i) = cstring(i)
         END IF
      ENDDO
    END SUBROUTINE MDI_Recv_Command

  END MODULE
