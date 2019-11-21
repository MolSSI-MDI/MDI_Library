/*! \file
 *
 * \brief Implementation of library-based communication
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "mdi.h"
#include "mdi_lib.h"
#include "mdi_global.h"
#include "mdi_general.h"

/*! \brief Perform initialization of a communicator for library-based communication
 *
 */
int library_initialize() {
  code* this_code = vector_get(&codes, current_code);
  communicator new_comm;
  new_comm.method = MDI_LIB;

  // allocate the node data
  vector* node_vec = malloc(sizeof(vector));
  vector_init(node_vec, sizeof(node));
  new_comm.nodes = node_vec;

  // allocate the method data
  library_data* libd = malloc(sizeof(library_data));
  libd->connected_code = -1;
  libd->buf_allocated = 0;
  libd->execute_on_send = 0;
  new_comm.method_data = libd;

  vector_push_back( this_code->comms, &new_comm );

  return this_code->comms->size-1;
}

/*! \brief Set the driver as the current code
 *
 */
int library_set_driver_current() {
  code* this_code = vector_get(&codes, current_code);

  // check if the current code is an ENGINE that is linked as a LIBRARY
  if ( strcmp(this_code->role, "ENGINE") == 0 ) {
    if ( this_code->is_library == 1 ) {
      // the calling code must actually be the driver, so update current_code
      int icode;
      int found_driver = 0;
      for ( icode = 0; icode < codes.size; icode++ ) {
	code* other_code = vector_get(&codes, icode);
	if ( strcmp(other_code->role, "DRIVER") == 0 ) {
	  current_code = icode;
	  found_driver = 1;
	}
      }
      // confirm that the driver was found
      if ( found_driver == 0 ) {
	mdi_error("MDI_Accept_Communicator could not locate the driver; was MDI_Init called by the driver?");
      }
    }
  }
  return 0;
}

/*! \brief Perform LIBRARY method operations upon a call to MDI_Accept_Communicator
 *
 */
int library_accept_communicator() {
  // ensure that the driver is the current code
  library_set_driver_current();

  // get the driver code
  code* this_code = vector_get(&codes, current_code);
  this_code = vector_get(&codes, current_code);

  // if this is a DRIVER, check if there are any ENGINES that are linked to it
  if ( strcmp(this_code->role, "DRIVER") == 0 ) {

    int icode;
    int found_engine = 0;
    int iengine = 0;
    for ( icode = 0; icode < codes.size; icode++ ) {
      code* other_code = vector_get(&codes, icode);
      if ( strcmp(other_code->role, "ENGINE") == 0 ) {
	if ( other_code->is_library == 1 ) {
	  iengine = icode;
	  found_engine = 1;
	}
      }
    }

    // create a new communicator for this engine
    if ( found_engine == 1 ) {
      int icomm = library_initialize();

      // set the connected code for the driver
      communicator* this_comm = vector_get(this_code->comms,icomm);
      library_data* libd = (library_data*) this_comm->method_data;
      libd->connected_code = iengine;

      // set the connected code for the engine
      code* engine_code = vector_get(&codes, iengine);
      if ( engine_code->comms->size != 1 ) {
	mdi_error("MDI_Accept_Communicator error: Engine appears to have been initialized multiple times");
      }
      communicator* engine_comm = vector_get(engine_code->comms,0);
      library_data* engine_lib = (library_data*) engine_comm->method_data;
      engine_lib->connected_code = current_code;
    }

  }

  return 0;
}


/*! \brief Get the handle to the matching communicator on the code to which comm points
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the linked code.
 */
int library_get_matching_handle(MDI_Comm comm) {
  code* this_code = vector_get(&codes, current_code);
  communicator* this = vector_get(this_code->comms, comm-1);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;
  code* engine_code = vector_get(&codes, iengine);

  // identify the communicator on the engine that connects to the driver
  int icomm;
  int found_self = 0;
  int engine_comm_handle = 0;
  for ( icomm = 0; icomm < engine_code->comms->size; icomm++ ) {
    communicator* engine_comm = vector_get(engine_code->comms, icomm);
    library_data* engine_lib = (library_data*) engine_comm->method_data;
    if ( engine_lib->connected_code == current_code ) {
      found_self = 1;
      engine_comm_handle = icomm+1;
    }
  }

  // ensure that the communicator was found
  if ( found_self == 0 ) {
    mdi_error("Could not find communicator for engine; Did the engine call MDI_Init?");
  }

  return engine_comm_handle;
}


/*! \brief Set the next command that will be executed through the library communicator
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       command
 *                   Pointer to the command to be executed.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int library_set_command(const char* command, MDI_Comm comm) {
  int idriver = current_code;
  code* this_code = vector_get(&codes, current_code);
  communicator* this = vector_get(this_code->comms, comm-1);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;
  code* engine_code = vector_get(&codes, iengine);

  // get the matching engine communicator
  MDI_Comm engine_comm_handle = library_get_matching_handle(comm);
  communicator* engine_comm = vector_get(engine_code->comms, engine_comm_handle-1);

  // set the command
  library_data* engine_lib = (library_data*) engine_comm->method_data;
  strncpy(engine_lib->command, command, MDI_COMMAND_LENGTH);

  return 0;
}


/*! \brief Execute a command through a communicator
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       command
 *                   Pointer to the command to be executed.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int library_execute_command(MDI_Comm comm) {
  int ret;

  int idriver = current_code;
  code* this_code = vector_get(&codes, current_code);
  communicator* this = vector_get(this_code->comms, comm-1);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;
  code* engine_code = vector_get(&codes, iengine);

  MDI_Comm engine_comm_handle = library_get_matching_handle(comm);
  communicator* engine_comm = vector_get(engine_code->comms, engine_comm_handle-1);
  library_data* engine_lib = (library_data*) engine_comm->method_data;

  // set the current code to the engine
  current_code = iengine;

  // check if this command corresponds to one of MDI's standard built-in commands
  int builtin_flag = general_builtin_command(engine_lib->command, engine_comm_handle);

  if ( builtin_flag == 0 ) {
    // call execute_command now
    ret = engine_code->execute_command(engine_lib->command,engine_comm_handle);
  }

  // set the current code to the driver
  current_code = idriver;

  return ret;
}



/*! \brief Function to handle sending data through an MDI connection, using library-based communication
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
int library_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR ) {
    mdi_error("MDI data type not recognized in library_send");
  }

  code* this_code = vector_get(&codes, current_code);
  communicator* this = vector_get(this_code->comms, comm-1);
  library_data* libd = (library_data*) this->method_data;
  
  // determine the byte size of the data type being sent
  size_t datasize;
  if (datatype == MDI_INT) {
    datasize = sizeof(int);
  }
  else if (datatype == MDI_DOUBLE) {
    datasize = sizeof(double);
  }
  else if (datatype == MDI_CHAR) {
    datasize = sizeof(char);
  }

  // confirm that libd->buf is not already allocated
  if ( libd->buf_allocated != 0 ) {
    mdi_error("MDI recv buffer already allocated");
  }

  // allocate the memory required for the send
  libd->buf = malloc( datasize * count );
  libd->buf_allocated = 1;

  // copy the contents of buf into libd->buf
  memcpy(libd->buf, buf, datasize * count);

  // check whether the recipient code should now execute its command
  if ( libd->execute_on_send == 1 ) {
    // have the recipient code execute its command
    library_execute_command(comm);

    // turn off the execute_on_send flag
    libd->execute_on_send = 0;
  }

  return 0;
}


/*! \brief Function to handle receiving data through an MDI connection, using library-based communication
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
int library_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR ) {
    mdi_error("MDI data type not recognized in library_send");
  }

  code* this_code = vector_get(&codes, current_code);
  communicator* this = vector_get(this_code->comms, comm-1);
  library_data* libd = (library_data*) this->method_data;

  // determine the byte size of the data type being sent
  size_t datasize;
  if (datatype == MDI_INT) {
    datasize = sizeof(int);
  }
  else if (datatype == MDI_DOUBLE) {
    datasize = sizeof(double);
  }
  else if (datatype == MDI_CHAR) {
    datasize = sizeof(char);
  }

  code* other_code = vector_get(&codes, libd->connected_code);
  MDI_Comm other_comm_handle = library_get_matching_handle(comm);
  communicator* other_comm = vector_get(other_code->comms, other_comm_handle-1);
  library_data* other_lib = (library_data*) other_comm->method_data;

  // confirm that libd->buf is initialized
  if ( other_lib->buf_allocated != 1 ) {
    mdi_error("MDI send buffer is not allocated");
  }

  // copy the contents of libd->buf into buf
  memcpy(buf, other_lib->buf, datasize * count);

  // free the memory of libd->buf
  free( other_lib->buf );
  other_lib->buf_allocated = 0;

  return 0;
}
