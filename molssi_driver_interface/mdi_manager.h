/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_CLASS
#define MDI_CLASS

#include <vector>
#include "mdi.h"

class MDIManager
{
public:
  MDIManager();
  MPI_Comm intra_MPI_comm;
  int tcp_socket;
  int intra_rank;
  int mpi_code_rank;

//private:
//  vector <Method*> methods;
};

#endif
