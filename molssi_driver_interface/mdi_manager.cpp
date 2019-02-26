/*! \file
 *
 * \brief Class definition for top-level manager of MDI operations
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mdi.h"
#include "mdi_manager.h"

using namespace std;

MDIManager::MDIManager() {
  this->intra_MPI_comm = 0;
  this->tcp_socket = -1;
  this->intra_rank = 0;
  this->mpi_code_rank = 0;
}
