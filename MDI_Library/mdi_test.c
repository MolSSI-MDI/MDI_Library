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


/*! \brief Enable support for the TEST method */
int enable_test_support(int code_id) {
  int ret;
  int method_id;
  ret = new_method(code_id, MDI_TEST, &method_id);
  if ( ret != 0 ) {
    mdi_error("Error in enable_test_support: new_method failed");
    return ret;
  }
  method* this_method;
  ret = get_method(code_id, MDI_TEST, &this_method);
  if ( ret != 0 ) {
    mdi_error("Error in enable_test_support: get_method failed");
    return ret;
  }
  this_method->on_selection = test_on_selection;
  this_method->on_accept_communicator = test_on_accept_communicator;
  this_method->on_send_command = test_on_send_command;
  this_method->after_send_command = test_after_send_command;
  this_method->on_recv_command = test_on_recv_command;
  return 0;
}



/*! \brief Callback when the end-user selects TCP as the method */
int test_on_selection() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in test_on_selection: get_current_code failed");
    return 1;
  }

  if ( this_code->test_initialized == 1 ) {
    mdi_error("MDI_Init called after MDI was already initialized");
    return 1;
  }
  this_code->test_initialized = 1;

  test_initialize();

  return 0;
}



/*! \brief Callback when the TEST method must accept a communicator */
int test_on_accept_communicator() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in test_on_accept_communicator: get_current_code failed");
    return 1;
  }

  // If MDI hasn't returned some connections, do that now
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    communicator* comm_obj;
    ret = get_communicator(codes.current_key, this_code->returned_comms, &comm_obj);
    if ( ret != 0 ) {
      mdi_error("Error in test_on_accept_communicator: get_communicator failed");
      return ret;
    }
    comm_obj->is_accepted = 1;
    return this_code->returned_comms;
  }

  // unable to accept any connections
  return MDI_COMM_NULL;
}



/*! \brief Callback when the TEST method must send a command */
int test_on_send_command(const char* command, MDI_Comm comm, int* skip_flag) {
  return 0;
}



/*! \brief Callback after the TEST method has received a command */
int test_after_send_command(const char* command, MDI_Comm comm) {
  // if the command was "EXIT", delete this communicator
  if ( strcmp( command, "EXIT" ) == 0 ) {
    delete_communicator(codes.current_key, comm);
  }
  
  return 0;
}



/*! \brief Callback when the TEST method must receive a command */
int test_on_recv_command(MDI_Comm comm) {
  return 0;
}



/*! \brief Perform initialization of a dummy communicator for testing purposes
 *
 */
int test_initialize() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in test_initialize: get_current_code failed");
    return 1;
  }

  MDI_Comm comm_id;
  ret = new_communicator(this_code->id, MDI_TEST, &comm_id);
  if ( ret != 0 ) {
    mdi_error("Error in test_initialize: new_communicator failed");
    return 1;
  }
  communicator* new_comm;
  ret = get_communicator(this_code->id, comm_id, &new_comm);
  if ( ret != 0 ) {
    mdi_error("Error in test_initialize: get_communicator failed");
    return ret;
  }
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
