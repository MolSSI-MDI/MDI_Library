/*! \file
 *
 * \brief Implementation of library-based communication
 */
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "mdi.h"
#include "mdi_lib.h"
#include "mdi_global.h"
#include "mdi_general.h"
#include "mdi_plug_py.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif


/*! \brief Launch an MDI plugin
 *
 */
int library_launch_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                          MDI_Driver_node_callback_t driver_node_callback,
                          void* driver_callback_object) {
  int ret;
  int driver_code_id = current_code;
  code* this_code = get_code(driver_code_id);
  MPI_Comm mpi_comm = *(MPI_Comm*) mpi_comm_ptr;

  // Begin parsing the options char array into an argv-style array of char arrays

  // copy the input options array
  int options_len = strlen(options) + 1;
  plugin_options = malloc( options_len * sizeof(char) );
  snprintf(plugin_options, options_len, "%s", options);
  plugin_unedited_options = malloc( options_len * sizeof(char) );
  snprintf(plugin_unedited_options, options_len, "%s", options);

  // determine the number of arguments
  plugin_argc = 0;
  int ichar;
  int in_argument = 0; // was the previous character part of an argument, or just whitespace?
  int in_single_quotes = 0; // was the previous character part of a single quote?
  int in_double_quotes = 0; // was the previous character part of a double quote?
  for (ichar=0; ichar < options_len; ichar++) {
    if ( plugin_options[ichar] == '\0' ) {
      if ( in_double_quotes ) {
	mdi_error("Unterminated double quotes received in MDI_Launch_plugin \"options\" argument.");
      }
      if ( in_argument ) {
	plugin_argc++;
      }
      in_argument = 0;
    }
    else if (plugin_options[ichar] == ' ') {
      if ( ! in_double_quotes && ! in_single_quotes ) {
	if ( in_argument ) {
	  plugin_argc++;
	}
	in_argument = 0;
	plugin_options[ichar] = '\0';
      }
    }
    else if (plugin_options[ichar] == '\"') {
      if ( in_single_quotes ) {
	mdi_error("Nested quotes not supported by MDI_Launch_plugin \"options\" argument.");
      }
      in_argument = 1;
      in_double_quotes = (in_double_quotes + 1) % 2;
      plugin_options[ichar] = '\0';
    }
    else if (plugin_options[ichar] == '\'') { 
      if ( in_double_quotes ) {
	mdi_error("Nested quotes not supported by MDI_Launch_plugin \"options\" argument.");
      }
      in_argument = 1;
      in_single_quotes = (in_single_quotes + 1) % 2;
      plugin_options[ichar] = '\0';
    }
    else {
      in_argument = 1;
    }
  }

  // construct pointers to all of the arguments
  plugin_argv = malloc( plugin_argc * sizeof(char*) );
  int iarg = 0;
  for (ichar=0; ichar < options_len; ichar++) {
    if ( plugin_options[ichar] != '\0' ) {
      if ( ichar == 0 || plugin_options[ichar-1] == '\0' ) {
	plugin_argv[iarg] = &plugin_options[ichar];
	iarg++;
      }
    }
  }
  if ( iarg != plugin_argc ) {
    mdi_error("Programming error: unable to correctly parse the MDI_Launch_plugin \"options\" argument.");
  }

  //
  // Get the path to the plugin
  // Note: Eventually, should probably replace this code with libltdl
  //
  char* plugin_path = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );

  // Get the name of the plugin's init function
  char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
  snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_init_%s", plugin_name);

  // initialize a communicator for the driver
  int icomm = library_initialize();
  communicator* driver_comm = get_communicator(current_code, icomm);
  library_data* libd = (library_data*) driver_comm->method_data;
  libd->connected_code = (int)codes.size;

  MDI_Comm comm;
  ret = MDI_Accept_Communicator(&comm);
  if ( ret != 0 || comm == MDI_COMM_NULL ) {
    mdi_error("MDI unable to create communicator for plugin");
    return -1;
  }

  // Set the driver callback function to be used by this plugin instance
  libd->driver_callback_obj = driver_callback_object;
  libd->driver_node_callback = driver_node_callback;

  // Set the mpi communicator associated with this plugin instance
  libd->mpi_comm = mpi_comm;


  /*************************************************/
  /*************** BEGIN PLUGIN MODE ***************/
  /*************************************************/
  plugin_mode = 1;

  // Attempt to load a python script
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/%s.py", this_code->plugin_path, plugin_name);
  if ( file_exists(plugin_path) ) {
    ret = python_plugin_init( plugin_name, plugin_path, options, mpi_comm_ptr );
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init");
      return -1;
    }
  }
  else {

#ifdef _WIN32
  // Attempt to open a library with a .dll extension
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.dll", this_code->plugin_path, plugin_name);
  HINSTANCE plugin_handle = LoadLibrary( plugin_path );
  if ( ! plugin_handle ) {
    // Unable to find the plugin library
    free( plugin_path );
    mdi_error("Unable to open MDI plugin");
    return -1;
  }

  // Load a plugin's initialization function
  MDI_Plugin_init_t plugin_init = (MDI_Plugin_init_t) (intptr_t) GetProcAddress( plugin_handle, plugin_init_name );
  if ( ! plugin_init ) {
    free( plugin_path );
    free( plugin_init_name );
    FreeLibrary( plugin_handle );
    mdi_error("Unable to load MDI plugin init function");
    return -1;
  }

#else
  // Attempt to open a library with a .so extension
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.so", this_code->plugin_path, plugin_name);
  void* plugin_handle = dlopen(plugin_path, RTLD_NOW);
  if ( ! plugin_handle ) {

    // Attempt to open a library with a .dylib extension
    snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.dylib", this_code->plugin_path, plugin_name);
    plugin_handle = dlopen(plugin_path, RTLD_NOW);
    if ( ! plugin_handle ) {
      // Unable to find the plugin library
      free( plugin_path );
      free( plugin_init_name );
      mdi_error("Unable to open MDI plugin");
      return -1;
    }
  }

  // Load a plugin's initialization function
  MDI_Plugin_init_t plugin_init = (MDI_Plugin_init_t) (intptr_t) dlsym(plugin_handle, plugin_init_name);
  if ( ! plugin_init ) {
    free( plugin_path );
    free( plugin_init_name );
    dlclose( plugin_handle );
    mdi_error("Unable to load MDI plugin init function");
    return -1;
  }
#endif

  // Initialize an instance of the plugin
  ret = plugin_init();
  if ( ret != 0 ) {
    mdi_error("MDI plugin init function returned non-zero exit code");
    return -1;
  }

  // Close the plugin library
#ifdef _WIN32
  FreeLibrary( plugin_handle );
#else
  dlclose( plugin_handle );
#endif

  }

  /*************************************************/
  /**************** END PLUGIN MODE ****************/
  /*************************************************/
  plugin_mode = 0;
  current_code = driver_code_id;

  // Delete the driver's communicator to the engine
  // This will also delete the engine code and its communicator
  delete_communicator(driver_code_id, comm);

  // free memory from loading the plugin's initialization function
  free( plugin_path );
  free( plugin_init_name );

  // free memory from storing the plugin's command-line options
  free( plugin_options );
  free( plugin_unedited_options );
  free( plugin_argv );

  return 0;
}


/*! \brief Perform initialization of a communicator for library-based communication
 *
 */
int library_initialize() {
  code* this_code = get_code(current_code);

  MDI_Comm comm_id = new_communicator(this_code->id, MDI_LINK);
  communicator* new_comm = get_communicator(this_code->id, comm_id);
  new_comm->delete = communicator_delete_lib;
  new_comm->send = library_send;
  new_comm->recv = library_recv;

  // set the MDI version number of the new communicator
  new_comm->mdi_version[0] = MDI_MAJOR_VERSION;
  new_comm->mdi_version[1] = MDI_MINOR_VERSION;
  new_comm->mdi_version[2] = MDI_PATCH_VERSION;

  // allocate the method data
  library_data* libd = malloc(sizeof(library_data));
  libd->connected_code = -1;
  libd->buf_allocated = 0;
  libd->execute_on_send = 0;
  libd->mpi_comm = MPI_COMM_NULL;
  new_comm->method_data = libd;

  // if this is an engine, go ahead and set the driver as the connected code
  if ( strcmp(this_code->role, "ENGINE") == 0 ) {
    int engine_code = current_code;
    library_set_driver_current();
    int driver_code_id = current_code;
    libd->connected_code = driver_code_id;
    current_code = engine_code;

    // set the engine's mpi communicator
    if ( plugin_mode ) {
      code* driver_code = get_code(driver_code_id);
      MDI_Comm matching_handle = library_get_matching_handle(comm_id);
      communicator* driver_comm = get_communicator(driver_code->id, matching_handle);
      library_data* driver_libd = (library_data*) driver_comm->method_data;
      libd->mpi_comm = driver_libd->mpi_comm;
      this_code->intra_MPI_comm = libd->mpi_comm;
      MPI_Comm_rank( this_code->intra_MPI_comm, &this_code->intra_rank );
    }
  }

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
  code* this_code = get_code(current_code);
  if ( this_code->called_set_execute_command_func && (! plugin_mode) ) {
    // library codes are not permitted to call MDI_Accept_communicator after calling
    // MDI_Set_execute_command_func, so assume that this call is being made by the driver
    library_set_driver_current();
  }
  this_code = get_code(current_code);

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
	  break;
	}
      }
    }

    // create a new communicator for this engine
    if ( found_engine == 1 ) {
      int icomm = library_initialize();

      // set the connected code for the driver
      code* engine_code = get_code(iengine);
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
  snprintf(engine_lib->command, COMMAND_LENGTH, "%s", command);

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
  int ret = 0;

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
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int library_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;

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
    MDI_Datatype basetype;
    ret = datatype_info(datatype, &datasize, &basetype);
    if ( ret != 0 ) {
      mdi_error("datatype_info returned nonzero value in library_send");
      return ret;
    }

    int nheader_actual = 4; // actual number of elements of nheader that were sent

    if ( msg_flag == 1 ) { // message header

      // confirm that libd->buf is not already allocated
      if ( libd->buf_allocated != 0 ) {
	mdi_error("MDI recv buffer already allocated");
	return 1;
      }

      // get the size of the message body, based on the header information
      int* header = (int*)buf;
      int body_type = header[2];
      int body_size = header[3];

      // determine the byte size of the data type being sent in the body of the message
      size_t body_stride;
      ret = datatype_info(body_type, &body_stride, &basetype);
      if ( ret != 0 ) {
	mdi_error("datatype_info returned nonzero value in library_send");
	return ret;
      }

      int msg_bytes = ( (int)datasize * count ) + ( (int)body_stride * body_size );

      // allocate the memory required for the entire message
      libd->buf = malloc( msg_bytes );
      libd->buf_allocated = 1;

      // copy the header into libd->buf
      memcpy(libd->buf, buf, nheader_actual * sizeof(int));

    }
    else if ( msg_flag == 2 ) { // message body

      // confirm that libd->buf has been allocated
      int has_header = 1;
      if ( libd->buf_allocated == 0 ) {
	// libd->buf has not been allocated, which means there is no header
	has_header = 0;
	libd->buf = malloc( datasize * count );
	libd->buf_allocated = 1;
      }

      int offset = 0;
      if ( has_header == 1 ) {
	offset = nheader_actual * sizeof(int);
      }

      // copy the body into libd->buf
      memcpy((char*)libd->buf + offset, buf, datasize * count);

    }
    else {

      mdi_error("MDI library unknown msg_flag in send\n");
      return 1;

    }

  }

  // check whether the recipient code should now execute its command
  if ( msg_flag == 2 && libd->execute_on_send ) {
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
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int library_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;

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

  // determine the byte size of the data type being sent
  size_t datasize;
  MDI_Datatype basetype;
  ret = datatype_info(datatype, &datasize, &basetype);
  if ( ret != 0 ) {
    mdi_error("datatype_info returned nonzero value in library_recv");
    return ret;
  }

  // confirm that libd->buf is initialized
  if ( other_lib->buf_allocated != 1 ) {
    mdi_error("MDI send buffer is not allocated");
    return 1;
  }

  // receive message header information
  // only do this if communicating with MDI version 1.1 or higher
  if ( ( this->mdi_version[0] > 1 ||
	 ( this->mdi_version[0] == 1 && this->mdi_version[1] >= 1 ) )
       && ipi_compatibility != 1 ) {

    if ( msg_flag == 1 ) { // message header

      memcpy(buf, other_lib->buf, count * datasize);

    }
    else if ( msg_flag == 2 ) { // message body

      int nheader = 4;
      int offset = nheader * sizeof(int);
      memcpy(buf, (char*)other_lib->buf + offset, count * datasize);

      // free the memory of libd->buf
      free( other_lib->buf );
      other_lib->buf_allocated = 0;

    }
    else {

      mdi_error("MDI library unknown msg_flag in send\n");
      return 1;

    }

  }

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
