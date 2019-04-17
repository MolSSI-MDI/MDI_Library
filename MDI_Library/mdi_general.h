/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_GENERAL
#define MDI_GENERAL

#include "mdi.h"
#include "method.h"

int manager_init(const char* options, void* world_comm);
int manager_accept_communicator();
int manager_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int manager_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int manager_send_command(const char* buf, MDI_Comm comm);
int manager_recv_command(char* buf, MDI_Comm comm);

#endif
