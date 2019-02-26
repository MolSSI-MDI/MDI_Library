/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_METHOD
#define MDI_METHOD

#include <vector>
#include "mdi.h"

class MethodMPI
{
public:
  MethodMPI();
  int gather_names(const char* hostname_ptr, bool do_split);
  int split_mpi_communicator(void* world_comm);
  MPI_Comm intra_MPI_comm;
  int intra_rank;
  int mpi_code_rank;
};

#endif
