/*! \file
 *
 * \brief Dummy communication implementation for testing purposes
 */

#ifndef MDI_TEST_IMPL
#define MDI_TEST_IMPL

#include "mdi.h"

int test_initialize();
int test_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int test_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

#endif
