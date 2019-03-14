! Fortran 90 wrapper for the MolSSI Driver Interface

   MODULE MDI
   USE ISO_C_BINDING

   IMPLICIT NONE

   INTEGER(KIND=C_INT), PARAMETER :: MDI_COMMAND_LENGTH = 12
   INTEGER(KIND=C_INT), PARAMETER :: MDI_NAME_LENGTH    = 12
   INTEGER(KIND=C_INT), PARAMETER :: MDI_NULL_COMM      = 0

   INTEGER(KIND=C_INT), PARAMETER :: MDI_INT            = 0
   INTEGER(KIND=C_INT), PARAMETER :: MDI_DOUBLE         = 1
   INTEGER(KIND=C_INT), PARAMETER :: MDI_CHAR           = 2

   !----------------------!
   ! MDI unit conversions !
   !----------------------!

   ! length
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_METER_TO_BOHR")         :: MDI_METER_TO_BOHR
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_ANGSTROM_TO_BOHR")      :: MDI_ANGSTROM_TO_BOHR

   ! time
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_SECOND_TO_AUT")         :: MDI_SECOND_TO_AUT
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_PICOSECOND_TO_AUT")     :: MDI_PICOSECOND_TO_AUT

   ! force
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_NEWTON_TO_AUF")         :: MDI_NEWTON_TO_AUF

   ! energy
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_JOULE_TO_HARTREE")      :: MDI_JOULE_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_KJ_TO_HARTREE")         :: MDI_KJ_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_KJPERMOL_TO_HARTREE")   :: MDI_KJPERMOL_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_KCALPERMOL_TO_HARTREE") :: MDI_KCALPERMOL_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_EV_TO_HARTREE")         :: MDI_EV_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_RYDBERG_TO_HARTREE")    :: MDI_RYDBERG_TO_HARTREE
   REAL(KIND=C_DOUBLE), PROTECTED, BIND(C, name="MDI_KELVIN_TO_HARTREE")     :: MDI_KELVIN_TO_HARTREE

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

     FUNCTION MDI_Accept_Communicator_() bind(c, name="MDI_Accept_Communicator")
       USE, INTRINSIC :: iso_c_binding
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

     FUNCTION MDI_Conversion_Factor_(in_unit, out_unit) bind(c, name="MDI_Conversion_Factor")
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                       :: in_unit, out_unit
       REAL(KIND=C_DOUBLE)                      :: MDI_Conversion_Factor_
     END FUNCTION MDI_Conversion_Factor_

  END INTERFACE



  CONTAINS

    SUBROUTINE MDI_Init(foptions, fworld_comm, ierr)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: foptions
      INTEGER, INTENT(INOUT) :: fworld_comm
      INTEGER, INTENT(OUT) :: ierr

      ierr = MDI_Init_( TRIM(foptions)//c_null_char, fworld_comm )
    END SUBROUTINE MDI_Init

    SUBROUTINE MDI_Accept_Communicator(communicator)
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: communicator

      communicator = MDI_Accept_Communicator_()
    END SUBROUTINE MDI_Accept_Communicator

    SUBROUTINE MDI_Send_s (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      CHARACTER(LEN=*), INTENT(IN)             :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(count)

      DO i = 1, LEN_TRIM(fbuf)
         cbuf(i) = fbuf(i:i)
      END DO
      cbuf( LEN_TRIM(fbuf) + 1 ) = c_null_char

      ierr = MDI_Send_( c_loc(cbuf), count, datatype, comm)
    END SUBROUTINE MDI_Send_s

    SUBROUTINE MDI_Send_d (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(IN)                 :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cbuf

      cbuf = fbuf
      ierr = MDI_Send_(c_loc(cbuf), 1, datatype, comm)
    END SUBROUTINE MDI_Send_d

    SUBROUTINE MDI_Send_dv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(IN), TARGET         :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Send_dv

    SUBROUTINE MDI_Send_i (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER, INTENT(IN)                      :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cbuf

      cbuf = fbuf
      ierr = MDI_Send_(c_loc(cbuf), 1, datatype, comm)
    END SUBROUTINE MDI_Send_i

    SUBROUTINE MDI_Send_iv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER(KIND=C_INT), TARGET              :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Send_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Send_iv

    SUBROUTINE MDI_Recv_s (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      CHARACTER(LEN=*), INTENT(OUT)            :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(count)

      ierr = MDI_Recv_(c_loc(cbuf(1)), count, datatype, comm)

      ! convert from C string to Fortran string
      fbuf = ""
      end_string = .false.
      DO i = 1, count
         IF ( cbuf(i) == c_null_char ) end_string = .true.
         IF ( end_string ) THEN
            fbuf(i:i) = ' '
         ELSE
            fbuf(i:i) = cbuf(i)
         END IF
      ENDDO
    END SUBROUTINE MDI_Recv_s

    SUBROUTINE MDI_Recv_d (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(OUT)                :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      REAL(KIND=C_DOUBLE), TARGET              :: cbuf

      ierr = MDI_Recv_(c_loc(cbuf), 1, datatype, comm)
      fbuf = cbuf
    END SUBROUTINE MDI_Recv_d

    SUBROUTINE MDI_Recv_dv(fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING  
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      REAL(KIND=8), INTENT(OUT), TARGET        :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Recv_dv

    SUBROUTINE MDI_Recv_i (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER, INTENT(OUT)                     :: fbuf
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER(KIND=C_INT), TARGET              :: cbuf

      ierr = MDI_Recv_(c_loc(cbuf), 1, datatype, comm)
      fbuf = cbuf
    END SUBROUTINE MDI_Recv_i

    SUBROUTINE MDI_Recv_iv (fbuf, count, datatype, comm, ierr)
      USE ISO_C_BINDING
      INTEGER, INTENT(IN)                      :: count, datatype, comm
      INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: fbuf(count)
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = MDI_Recv_(c_loc(fbuf(1)), count, datatype, comm)
    END SUBROUTINE MDI_Recv_iv

    SUBROUTINE MDI_Send_Command(fbuf, comm, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(IN)             :: fbuf
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(MDI_COMMAND_LENGTH)

      DO i = 1, LEN_TRIM(fbuf)
         cbuf(i) = fbuf(i:i)
      END DO
      cbuf( LEN_TRIM(fbuf) + 1 ) = c_null_char

      ierr = MDI_Send_Command_( c_loc(cbuf), comm)
    END SUBROUTINE MDI_Send_Command

    SUBROUTINE MDI_Recv_Command(fbuf, comm, ierr)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(OUT)            :: fbuf
      INTEGER, INTENT(IN)                      :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      INTEGER                                  :: i
      LOGICAL                                  :: end_string
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cbuf(MDI_COMMAND_LENGTH)

      ierr = MDI_Recv_Command_(c_loc(cbuf(1)), comm)

      ! convert from C string to Fortran string
      fbuf = ""
      end_string = .false.
      DO i = 1, MDI_COMMAND_LENGTH
         IF ( cbuf(i) == c_null_char ) end_string = .true.
         IF ( end_string ) THEN
            fbuf(i:i) = ' '
         ELSE
            fbuf(i:i) = cbuf(i)
         END IF
      ENDDO
    END SUBROUTINE MDI_Recv_Command

    SUBROUTINE MDI_Conversion_Factor(fin_unit, fout_unit, factor)
      USE ISO_C_BINDING
      CHARACTER(LEN=*), INTENT(IN)             :: fin_unit, fout_unit
      DOUBLE PRECISION, INTENT(OUT)            :: factor

      INTEGER                                  :: i
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cin_unit(LEN_TRIM(fin_unit)+1)
      CHARACTER(LEN=1, KIND=C_CHAR), TARGET    :: cout_unit(LEN_TRIM(fout_unit)+1)

      DO i = 1, LEN_TRIM(fin_unit)
         cin_unit(i) = fin_unit(i:i)
      END DO
      cin_unit( LEN_TRIM(fin_unit) + 1 ) = c_null_char

      DO i = 1, LEN_TRIM(fout_unit)
         cout_unit(i) = fout_unit(i:i)
      END DO
      cout_unit( LEN_TRIM(fout_unit) + 1 ) = c_null_char

      factor = MDI_Conversion_Factor_( c_loc(cin_unit), c_loc(cout_unit) )
    END SUBROUTINE MDI_Conversion_Factor

  END MODULE
