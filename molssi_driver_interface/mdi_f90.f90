! Fortran 90 wrapper for the MolSSI Driver Interface

   MODULE F90MESSAGES
   USE ISO_C_BINDING
   
   IMPLICIT NONE

   INTEGER, PROTECTED, BIND(C, name="MDI_COMMAND_LENGTH") :: MDI_COMMAND_LENGTH

   INTEGER, PROTECTED, BIND(C, name="MDI_INT")                   :: MDI_INT
   INTEGER, PROTECTED, BIND(C, name="MDI_DOUBLE")                :: MDI_DOUBLE
   INTEGER, PROTECTED, BIND(C, name="MDI_CHAR")                  :: MDI_CHAR

   !----------------------!
   ! MDI unit conversions !
   !----------------------!

   ! length
   INTEGER, PROTECTED, BIND(C, name="MDI_METER_TO_BOHR")         :: MDI_METER_TO_BOHR
   INTEGER, PROTECTED, BIND(C, name="MDI_ANGSTROM_TO_BOHR")      :: MDI_ANGSTROM_TO_BOHR

   ! time
   INTEGER, PROTECTED, BIND(C, name="MDI_SECOND_TO_AUT")         :: MDI_SECOND_TO_AUT
   INTEGER, PROTECTED, BIND(C, name="MDI_PICOSECOND_TO_AUT")     :: MDI_PICOSECOND_TO_AUT

   ! force
   INTEGER, PROTECTED, BIND(C, name="MDI_NEWTON_TO_AUF")         :: MDI_NEWTON_TO_AUF

   ! energy
   INTEGER, PROTECTED, BIND(C, name="MDI_JOULE_TO_HARTREE")      :: MDI_JOULE_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_KJ_TO_HARTREE")         :: MDI_KJ_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_KJPERMOL_TO_HARTREE")   :: MDI_KJPERMOL_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_KCALPERMOL_TO_HARTREE") :: MDI_KCALPERMOL_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_EV_TO_HARTREE")         :: MDI_EV_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_RYDBERG_TO_HARTREE")    :: MDI_RYDBERG_TO_HARTREE
   INTEGER, PROTECTED, BIND(C, name="MDI_KELVIN_TO_HARTREE")     :: MDI_KELVIN_TO_HARTREE

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

     FUNCTION MDI_Init_(sockfd_ptr) bind(c, name="MDI_Init")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT)                      :: sockfd_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Init_
     END FUNCTION MDI_Init_

     FUNCTION MDI_Accept_Connection_(sockfd_ptr, connection_ptr) bind(c, name="MDI_Accept_Connection")
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT)                      :: sockfd_ptr
       INTEGER(KIND=C_INT)                      :: connection_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Accept_Connection_
     END FUNCTION MDI_Accept_Connection_

     FUNCTION MDI_Send_Command_(data_ptr, sockfd_ptr) bind(c, name="MDI_Send_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: sockfd_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Send_Command_
     END FUNCTION MDI_Send_Command_

     FUNCTION MDI_Recv_Command_(data_ptr, sockfd_ptr) bind(c, name="MDI_Recv_Command")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: sockfd_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Recv_Command_
     END FUNCTION MDI_Recv_Command_

     SUBROUTINE launch_server(line) bind(c, name="launch_server")
       USE, INTRINSIC :: iso_c_binding
       CHARACTER(KIND=C_CHAR), DIMENSION(*)     :: line
     END SUBROUTINE launch_server

     FUNCTION MDI_Open_(sockfd_ptr, inet_ptr, port_ptr, hostname_ptr) BIND(C, name="MDI_Open")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT)                      :: sockfd_ptr, inet_ptr, port_ptr
       CHARACTER(KIND=C_CHAR), DIMENSION(*)     :: hostname_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Open_
     END FUNCTION MDI_Open_

     FUNCTION MDI_Send_(data_ptr, len_ptr, type_ptr, sockfd_ptr) BIND(C, name="MDI_Send")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT)                      :: len_ptr, type_ptr, sockfd_ptr
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Send_
     END FUNCTION MDI_Send_

     FUNCTION MDI_Recv_(data_ptr, len_ptr, type_ptr, sockfd_ptr) BIND(C, name="MDI_Recv")
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT)                      :: len_ptr, type_ptr, sockfd_ptr
       TYPE(C_PTR), VALUE                       :: data_ptr
       INTEGER(KIND=C_INT)                      :: MDI_Recv_
     END FUNCTION MDI_Recv_
  END INTERFACE


    
  CONTAINS

    SUBROUTINE MDI_Init(sockfd_ptr, ierr)
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: sockfd_ptr, ierr

      ierr = MDI_Init_(sockfd_ptr)
    END SUBROUTINE MDI_Init

    SUBROUTINE MDI_Accept_Connection(sockfd_ptr, connection_ptr, ierr)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: sockfd_ptr
      INTEGER, INTENT(OUT) :: connection_ptr, ierr

      ierr = MDI_Accept_Connection_(sockfd_ptr, connection_ptr)
    END SUBROUTINE MDI_Accept_Connection

    SUBROUTINE MDI_Open(sockfd_ptr, inet_ptr, port_ptr, hostname_ptr, ierr)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: inet_ptr, port_ptr
      INTEGER, INTENT(OUT) :: sockfd_ptr, ierr
      CHARACTER(LEN=1024), INTENT(IN) :: hostname_ptr
      CHARACTER(LEN=1,KIND=C_CHAR) :: chost(1024)

      CALL fstr2cstr(hostname_ptr, chost)
      ierr = MDI_Open_(sockfd_ptr, inet_ptr, port_ptr, chost)
    END SUBROUTINE MDI_Open

    SUBROUTINE fstr2cstr(fstr, cstr, plen)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: fstr
      CHARACTER(LEN=1,KIND=C_CHAR), INTENT(OUT) :: cstr(:)
      INTEGER, INTENT(IN), OPTIONAL :: plen
      
      INTEGER i,n
      IF (PRESENT(plen)) THEN
         n = plen
         DO i=1,n
            cstr(i) = fstr(i:i)
         ENDDO
      ELSE
         n = LEN_TRIM(fstr)
         DO i=1,n
            cstr(i) = fstr(i:i)
         ENDDO
         cstr(n+1) = C_NULL_CHAR
      END IF
    END SUBROUTINE fstr2cstr

    SUBROUTINE MDI_Send_Command(fstring, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(IN)             :: fstring
      INTEGER, INTENT(IN)                      :: sockfd_ptr
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(MDI_COMMAND_LENGTH)

      DO i = 1, LEN(fstring)
         cstring(i) = fstring(i:i)
      ENDDO
      
      !pad the command with spaces
      DO i = LEN(fstring) + 1, MDI_COMMAND_LENGTH
         cstring(i) = " "
      END DO

      ierr = MDI_Send_Command_(c_loc(cstring(1)), sockfd_ptr)
    END SUBROUTINE MDI_Send_Command

    SUBROUTINE MDI_Recv_Command(fstring, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(OUT)            :: fstring
      INTEGER, INTENT(IN)                      :: sockfd_ptr
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(MDI_COMMAND_LENGTH)

      ierr = MDI_Recv_Command_(c_loc(cstring(1)), sockfd_ptr)

      DO i = 1, MDI_COMMAND_LENGTH
         fstring(i:i) = cstring(i)
      ENDDO

    END SUBROUTINE MDI_Recv_Command

    SUBROUTINE MDI_Send_d (fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      REAL(KIND=8), INTENT(IN)                 :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cdata

      cdata = fdata
      ierr = MDI_Send_(c_loc(cdata), 1, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Send_d

    SUBROUTINE MDI_Send_i (fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      INTEGER, INTENT(IN)                      :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cdata

      cdata = fdata
      ierr = MDI_Send_(c_loc(cdata), 1, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Send_i

    SUBROUTINE MDI_Send_s (fstring, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      CHARACTER(LEN=*), INTENT(IN)             :: fstring
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(len_ptr)

      DO i = 1,len_ptr
         cstring(i) = fstring(i:i)
      ENDDO
      ierr = MDI_Send_(c_loc(cstring(1)), len_ptr, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Send_s

    SUBROUTINE MDI_Send_dv(fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      REAL(KIND=8), INTENT(IN), TARGET         :: fdata(len_ptr)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fdata(1)), len_ptr, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Send_dv

    SUBROUTINE MDI_Send_iv(fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      INTEGER(KIND=C_INT), TARGET              :: fdata(len_ptr)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fdata(1)), len_ptr, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Send_iv

    SUBROUTINE MDI_Recv_d (fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      REAL(KIND=8), INTENT(OUT)                :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cdata

      ierr = MDI_Recv_(c_loc(cdata), 1, type_ptr, sockfd_ptr)
      fdata=cdata
    END SUBROUTINE MDI_Recv_d

    SUBROUTINE MDI_Recv_iv (fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: fdata(len_ptr)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fdata(1)), len_ptr, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Recv_iv

    SUBROUTINE MDI_Recv_i (fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      INTEGER, INTENT(OUT)                     :: fdata
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cdata

      ierr = MDI_Recv_(c_loc(cdata), 1, type_ptr, sockfd_ptr)
      fdata = cdata
    END SUBROUTINE MDI_Recv_i

    SUBROUTINE MDI_Recv_s (fstring, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      CHARACTER(LEN=*), INTENT(OUT)            :: fstring
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cstring(len_ptr)

      ierr = MDI_Recv_(c_loc(cstring(1)), len_ptr, type_ptr, sockfd_ptr)
      fstring=""   
      DO i = 1,len_ptr
         fstring(i:i) = cstring(i)
      ENDDO
    END SUBROUTINE MDI_Recv_s

    SUBROUTINE MDI_Recv_dv(fdata, len_ptr, type_ptr, sockfd_ptr, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: len_ptr, type_ptr, sockfd_ptr
      REAL(KIND=8), INTENT(OUT), TARGET        :: fdata(len_ptr)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fdata(1)), len_ptr, type_ptr, sockfd_ptr)
    END SUBROUTINE MDI_Recv_dv

  END MODULE
