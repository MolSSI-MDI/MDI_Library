/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_CLASS
#define MDI_CLASS

#include <vector>
#include "mdi.h"
#include "method.h"

class MDIManager
{
public:
  MDIManager(const char* options, void* world_comm);
  int accept_communicator();
  int send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
  int recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
  int send_command(const char* buf, MDI_Comm comm);
  int recv_command(char* buf, MDI_Comm comm);
  MethodTCP* method_tcp;
  MethodMPI* method_mpi;

private:
  uint returned_comms;
//  vector <Method*> methods;
};

#endif
