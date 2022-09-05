/*! \file
 *
 * \brief TCP communication implementation
 */

#ifndef MDI_TCP_IMPL
#define MDI_TCP_IMPL

#include "mdi.h"
#include "mdi_global.h"

void sigint_handler(int dummy);

int enable_tcp_support(int code_id);
int tcp_on_selection();
int tcp_on_accept_communicator();
int tcp_on_send_command(const char* command, MDI_Comm comm, int* skip_flag);
int tcp_after_send_command(const char* command, MDI_Comm comm);
int tcp_on_recv_command(MDI_Comm comm);


int tcp_listen(int port_in);
int tcp_request_connection(int port_in, char* hostname_ptr);
int tcp_accept_connection();
int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);

#endif
