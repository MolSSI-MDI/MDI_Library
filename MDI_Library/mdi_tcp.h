/*! \file
 *
 * \brief TCP communication implementation
 */

#ifndef MDI_TCP_IMPL
#define MDI_TCP_IMPL

#include "mdi.h"
#include "mdi_global.h"

extern sock_t tcp_socket;

void sigint_handler(int dummy);

int tcp_listen(int port);
int tcp_request_connection(int port, char* hostname_ptr);
int tcp_accept_connection();
int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);

#endif
