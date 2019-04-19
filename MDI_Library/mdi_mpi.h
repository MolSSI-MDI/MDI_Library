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

int mpi_identify_codes(const char* code_name, int do_split);
int mpi_update_world_comm(void* world_comm);
int mpi_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int mpi_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

#endif
