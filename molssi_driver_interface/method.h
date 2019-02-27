/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_METHOD
#define MDI_METHOD

#include <vector>
#include "mdi.h"

void sigint_handler(int dummy);

class MethodTCP
{
public:
  MethodTCP();
  int MDI_Listen_TCP(int port);
  int MDI_Request_Connection_TCP(int port, char* hostname_ptr);
  int On_Accept_Communicator();
  int tcp_socket;
};

class MethodMPI
{
public:
  MethodMPI();
  int gather_names(const char* hostname_ptr, bool do_split);
  int split_mpi_communicator(void* world_comm);
  MDI_Comm intra_MPI_comm;
  int intra_rank;
  int mpi_code_rank;
};

#endif
