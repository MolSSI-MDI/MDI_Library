/*! \file
 *
 * \brief Generic MDI function calls
 */

#ifndef MDI_GENERAL
#define MDI_GENERAL

#include "mdi.h"

int general_init(const char* options, void* world_comm);
int general_accept_communicator();
int general_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int general_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int general_send_command(const char* buf, MDI_Comm comm);
int general_recv_command(char* buf, MDI_Comm comm);

#endif
