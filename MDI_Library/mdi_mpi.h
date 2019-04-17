/*! \file
 *
 * \brief MPI communication implementation
 */

#ifndef MDI_MPI_IMPL
#define MDI_MPI_IMPL

#include <mpi.h>
#include "mdi.h"

extern MPI_Comm intra_MPI_comm;
extern int intra_rank;
extern int mpi_code_rank;
int gather_names(const char* hostname_ptr, int do_split);
int split_mpi_communicator(void* world_comm);

#endif
