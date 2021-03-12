! Fortran 90 MPI stubs

   MODULE MPI

   IMPLICIT NONE

   INTEGER :: MPI_MAX_PROCESSOR_NAME = 0
   INTEGER :: MPI_STATUS_IGNORE = 0
   INTEGER :: MPI_COMM_WORLD = 0
   INTEGER :: MPI_INT = 1
   INTEGER :: MPI_DOUBLE = 4
   INTEGER :: MPI_CHAR = 5

   INTERFACE MPI_Bcast
      MODULE PROCEDURE MPI_Bcast_s
   END INTERFACE

  CONTAINS

    SUBROUTINE MPI_Init(ierr)
      IMPLICIT NONE
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = 0
    END SUBROUTINE MPI_Init

    SUBROUTINE MPI_Comm_rank(comm, rank, ierr)
      IMPLICIT NONE
      INTEGER                                  :: comm
      INTEGER, INTENT(OUT)                     :: rank, ierr

      rank = 0
      ierr = 0
    END SUBROUTINE MPI_Comm_rank

    SUBROUTINE MPI_Barrier(comm, ierr)
      IMPLICIT NONE
      INTEGER                                  :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = 0
    END SUBROUTINE MPI_Barrier

    SUBROUTINE MPI_Bcast_s(buffer, count, datatype, root, comm, ierr)
      IMPLICIT NONE
      CHARACTER(LEN=*)                         :: buffer
      INTEGER                                  :: count, datatype, root, comm
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = 0
    END SUBROUTINE MPI_Bcast_s

    SUBROUTINE MPI_Finalize(ierr)
      IMPLICIT NONE
      INTEGER                                  :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = 0
    END SUBROUTINE MPI_Finalize

  END MODULE
