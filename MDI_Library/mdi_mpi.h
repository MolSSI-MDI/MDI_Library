/*! \file
 *
 * \brief MPI communication implementation
 */

#ifndef MDI_MPI_IMPL
#define MDI_MPI_IMPL

#include <mpi.h>
#include "mdi.h"

typedef struct mpi_data_struct {
  /*! \brief Inter-code MPI communicator */
  MPI_Comm mpi_comm;
  /*! \brief The rank of this process within the inter-code MPI communicator */
  int mpi_rank;
  /*! \brief Flag whether to use mpi4py instead of the linked MPI library */
  int use_mpi4py;
} mpi_method_data;

extern MPI_Comm intra_MPI_comm;
extern int world_size;
extern int world_rank;

int set_world_size(int world_size_in);
int set_world_rank(int world_rank_in);

int mpi_identify_codes(const char* code_name, int use_mpi4py, MPI_Comm world_comm);
int mpi_update_world_comm(void* world_comm);
int mpi_send_msg(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int mpi_recv_msg(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int mpi_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int mpi_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);

int communicator_delete_mpi(void* comm);

#endif
