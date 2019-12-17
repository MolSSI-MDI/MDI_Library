! Fortran 90 MPI stubs

   MODULE MPI

   IMPLICIT NONE

   INTEGER :: MPI_COMM_WORLD = 0

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

    SUBROUTINE MPI_Finalize(ierr)
      IMPLICIT NONE
      INTEGER                                  :: comm
      INTEGER, INTENT(OUT)                     :: ierr

      ierr = 0
    END SUBROUTINE MPI_Finalize

  END MODULE
