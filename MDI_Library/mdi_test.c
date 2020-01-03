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

  new_communicator(this_code->id, MDI_TEST);

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
 */
int test_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR ) {
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
 */
int test_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR ) {
    mdi_error("MDI data type not recognized in test_send");
    return 1;
  }

  return 0;
}
