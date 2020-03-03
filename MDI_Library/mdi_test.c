/*! \file
 *
 * \brief Dummy communication implementation for testing purposes
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "mdi.h"
#include "mdi_test.h"
#include "mdi_global.h"

/*! \brief Perform initialization of a dummy communicator for testing purposes
 *
 */
int test_initialize() {
  code* this_code = get_code(current_code);

  MDI_Comm comm_id = new_communicator(this_code->id, MDI_TEST);
  communicator* new_comm = get_communicator(this_code->id, comm_id);
  new_comm->send = test_send;
  new_comm->recv = test_recv;

  return 0;
}

/*! \brief Dummy function to handle sending data through an MDI connection, for testing purposes
 *
 * \param [in]       buf
 *                   Pointer to the data to be sent.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be sent.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be sent.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int test_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR 
       && datatype != MDI_BYTE) {
    mdi_error("MDI data type not recognized in test_send");
    return 1;
  }

  return 0;
}


/*! \brief Dummy function to handle receiving data through an MDI connection, for testing purposes
 *
 * \param [in]       buf
 *                   Pointer to the buffer where the received data will be stored.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be received.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be received.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int test_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR 
       && datatype != MDI_BYTE) {
    mdi_error("MDI data type not recognized in test_send");
    return 1;
  }

  return 0;
}
