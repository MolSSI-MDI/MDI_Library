/*! \file
 *
 * \brief TCP communication implementation
 */

#ifndef MDI_TCP_IMPL
#define MDI_TCP_IMPL

#include "mdi.h"

void sigint_handler(int dummy);

extern int tcp_socket;
int MDI_Listen_TCP(int port);
int MDI_Request_Connection_TCP(int port, char* hostname_ptr);
int On_Accept_Communicator();

int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

#endif
