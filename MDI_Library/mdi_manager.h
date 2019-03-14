/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_CLASS
#define MDI_CLASS

#include "mdi.h"
#include "method.h"

int manager_init(const char* options, void* world_comm);
int manager_accept_communicator();
int manager_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int manager_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int manager_send_command(const char* buf, MDI_Comm comm);
int manager_recv_command(char* buf, MDI_Comm comm);

/*
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
};
*/

#endif
