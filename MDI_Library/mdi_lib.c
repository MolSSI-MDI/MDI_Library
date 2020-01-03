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
  code* this_code = get_code(current_code);

  MDI_Comm comm_id = new_communicator(this_code->id, MDI_LIB);
  communicator* new_comm = get_communicator(this_code->id, comm_id);
  new_comm->delete = communicator_delete_lib;

  // allocate the method data
  library_data* libd = malloc(sizeof(library_data));
  libd->connected_code = -1;
  libd->buf_allocated = 0;
  libd->execute_on_send = 0;
  new_comm->method_data = libd;

  return new_comm->id;
}

/*! \brief Set the driver as the current code
 *
 */
int library_set_driver_current() {
  code* this_code = get_code(current_code);

  // check if the current code is an ENGINE that is linked as a LIBRARY
  if ( strcmp(this_code->role, "ENGINE") == 0 ) {
    if ( this_code->is_library == 1 || this_code->is_library == 2 ) {
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
	return 1;
      }
    }
  }
  return 0;
}

/*! \brief Perform LIBRARY method operations upon a call to MDI_Accept_Communicator
 *
 */
int library_accept_communicator() {
  // set the driver as the current code, if this is an ENGINE that is linked as a LIBRARY
  library_set_driver_current();

  // get the driver code
  code* this_code = get_code(current_code);

  // if this is a DRIVER, check if there are any ENGINES that are linked to it
  if ( strcmp(this_code->role, "DRIVER") == 0 ) {

    int icode;
    int found_engine = 0;
    int iengine = 0;
    for ( icode = 0; icode < codes.size; icode++ ) {
      code* other_code = vector_get(&codes, icode);
      if ( strcmp(other_code->role, "ENGINE") == 0 ) {
	if ( other_code->is_library == 1 ) {
	  // flag that this library has connected to the driver
	  other_code->is_library = 2;

	  //iengine = icode;
	  iengine = other_code->id;
	  found_engine = 1;
	}
      }
    }

    // create a new communicator for this engine
    if ( found_engine == 1 ) {
      int icomm = library_initialize();

      // set the connected code for the engine
      code* engine_code = get_code(iengine);
      if ( engine_code->comms->size != 1 ) {
	mdi_error("MDI_Accept_Communicator error: Engine appears to have been initialized multiple times"); 
	return 1;
      }
      communicator* engine_comm = vector_get(engine_code->comms,0);
      library_data* engine_lib = (library_data*) engine_comm->method_data;
      engine_lib->connected_code = current_code;

      // set the connected code for the driver
      communicator* this_comm = get_communicator(current_code, icomm);
      library_data* libd = (library_data*) this_comm->method_data;
      libd->connected_code = engine_code->id;
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
  communicator* this = get_communicator(current_code, comm);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;
  code* engine_code = get_code(iengine);

  // identify the communicator on the engine that connects to the driver
  int icomm;
  int found_self = 0;
  int engine_comm_handle = 0;
  for ( icomm = 0; icomm < engine_code->comms->size; icomm++ ) {
    communicator* engine_comm = vector_get(engine_code->comms, icomm);
    library_data* engine_lib = (library_data*) engine_comm->method_data;
    if ( engine_lib->connected_code == current_code ) {
      found_self = 1;
      engine_comm_handle = engine_comm->id;
    }
  }

  // ensure that the communicator was found
  if ( found_self == 0 ) {
    mdi_error("Could not find communicator for engine; Did the engine call MDI_Init?"); 
    return 1;
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
  communicator* this = get_communicator(current_code, comm);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;

  // get the matching engine communicator
  MDI_Comm engine_comm_handle = library_get_matching_handle(comm);
  communicator* engine_comm = get_communicator(iengine, engine_comm_handle);

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
  communicator* this = get_communicator(current_code, comm);

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  int iengine = libd->connected_code;
  code* engine_code = get_code(iengine);

  MDI_Comm engine_comm_handle = library_get_matching_handle(comm);
  communicator* engine_comm = get_communicator(iengine, engine_comm_handle);
  library_data* engine_lib = (library_data*) engine_comm->method_data;

  // set the current code to the engine
  current_code = iengine;

  // check if this command corresponds to one of MDI's standard built-in commands
  int builtin_flag = general_builtin_command(engine_lib->command, engine_comm_handle);

  if ( builtin_flag == 0 ) {
    // call execute_command now
    void* class_obj = engine_code->execute_command_obj;
    ret = engine_code->execute_command(engine_lib->command,engine_comm_handle,class_obj);
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
    return 1;
  }

  code* this_code = get_code(current_code);
  communicator* this = get_communicator(current_code, comm);
  library_data* libd = (library_data*) this->method_data;

  code* other_code = get_code(libd->connected_code);

  // get the rank of this process on the engine
  int engine_rank = 0;
  if ( this_code->is_library ) {
    engine_rank = this_code->intra_rank;
  }
  else {
    engine_rank = other_code->intra_rank;
  }

  // only send from rank 0
  if ( engine_rank == 0 ) {

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
      return 1;
    }

    // allocate the memory required for the send
    libd->buf = malloc( datasize * count );
    libd->buf_allocated = 1;

    // copy the contents of buf into libd->buf
    memcpy(libd->buf, buf, datasize * count);

  }

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
  code* this_code = get_code(current_code);
  communicator* this = get_communicator(current_code, comm);
  library_data* libd = (library_data*) this->method_data;

  code* other_code = get_code(libd->connected_code);
  MDI_Comm other_comm_handle = library_get_matching_handle(comm);
  communicator* other_comm = get_communicator(libd->connected_code, other_comm_handle);
  library_data* other_lib = (library_data*) other_comm->method_data;

  // only recv from rank 0 of the engine
  int engine_rank = 0;
  if ( this_code->is_library ) {
    engine_rank = this_code->intra_rank;
  }
  else {
    engine_rank = other_code->intra_rank;
  }
  if ( engine_rank != 0 ) {
    return 0;
  }

  if ( datatype != MDI_INT && datatype != MDI_DOUBLE && datatype != MDI_CHAR ) {
    mdi_error("MDI data type not recognized in library_send");
    return 1;
  }

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

  // confirm that libd->buf is initialized
  if ( other_lib->buf_allocated != 1 ) {
    mdi_error("MDI send buffer is not allocated");
    return 1;
  }

  // copy the contents of libd->buf into buf
  memcpy(buf, other_lib->buf, datasize * count);

  // free the memory of libd->buf
  free( other_lib->buf );
  other_lib->buf_allocated = 0;

  return 0;
}


/*! \brief Function for LIBRARY-specific deletion operations for communicator deletion
 */
int communicator_delete_lib(void* comm) {
  communicator* this_comm = (communicator*) comm;
  code* this_code = get_code(this_comm->code_id);
  library_data* libd = (library_data*) this_comm->method_data;

  // if this is the driver, delete the engine code
  if ( this_code->is_library == 0 ) {
    delete_code(libd->connected_code);
  }

  // delete the method-specific information
  if ( libd->buf_allocated ) {
    free( libd->buf );
  }
  free( libd );

  return 0;
}
