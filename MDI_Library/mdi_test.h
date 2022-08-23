/*! \file
 *
 * \brief Dummy communication implementation for testing purposes
 */

#ifndef MDI_TEST_IMPL
#define MDI_TEST_IMPL

#include "mdi.h"

int enable_test_support(int code_id);
int test_on_selection();
int test_on_accept_communicator();
int test_on_send_command(const char* command, MDI_Comm comm, int* skip_flag);
int test_after_send_command(const char* command, MDI_Comm comm);
int test_on_recv_command(MDI_Comm comm);

int test_initialize();
int test_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int test_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);

#endif
