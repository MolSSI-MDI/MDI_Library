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


/*! \brief Enable support for the PLUG method */
int enable_plug_support( int code_id ) {
  int ret;
  int method_id;
  ret = new_method(code_id, MDI_LINK, &method_id);
  if ( ret != 0 ) {
    mdi_error("Error in enable_plug_support: new_method failed");
    return ret;
  }
  method* this_method;
  ret = get_method(code_id, MDI_LINK, &this_method);
  if ( ret != 0 ) {
    mdi_error("Error in enable_plug_support: get_method failed");
    return ret;
  }
  this_method->on_selection = plug_on_selection;
  this_method->on_accept_communicator = plug_on_accept_communicator;
  this_method->on_send_command = plug_on_send_command;
  this_method->after_send_command = plug_after_send_command;
  this_method->on_recv_command = plug_on_recv_command;
  return 0;
}



/*! \brief Callback when the end-user selects PLUG as the method */
int plug_on_selection() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_selection: get_current_code failed");
    return ret;
  }

  // Check if this is an engine being used as a library
  if (strcmp(this_code->role, "ENGINE") == 0) {
    this_code->is_library = 1;
    library_initialize();
  }

  return 0;
}



/*! \brief Callback when the PLUG method must accept a communicator */
int plug_on_accept_communicator() {
  int ret;

  ret = mdi_debug("[MDI:plug_on_accept_communicator] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_accept_communicator: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_accept_communicator: get_current_code failed");
    return 1;
  }

  // Give the library method an opportunity to update the current code
  //library_accept_communicator();

  // If MDI hasn't returned some connections, do that now
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    communicator* comm_obj;
    ret = get_communicator(codes.current_key, this_code->returned_comms, &comm_obj);
    if ( ret != 0 ) {
      mdi_error("Error in plug_on_accept_communicator: get_communicator failed");
      return ret;
    }
    comm_obj->is_accepted = 1;
    return this_code->returned_comms;
  }

  // unable to accept any connections
  return MDI_COMM_NULL;
}



/*! \brief Callback when the PLUG method must send a command */
int plug_on_send_command(const char* command, MDI_Comm comm, int* skip_flag) {
  int ret;
  ret = mdi_debug("[MDI:plug_on_send_command] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_send_command: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_send_command: get_current_code failed");
    return 1;
  }
  communicator* this_comm;
  ret = get_communicator(codes.current_key, comm, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_send_command: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) this_comm->method_data;

  // Check whether MPI has been initialized
  int mpi_init_flag;
  if ( MPI_Initialized(&mpi_init_flag) ) {
    mdi_error("Error in MDI_plug_on_send_command: MPI_Initialized failed");
    return 1;
  }

  // broadcast the command to each rank
  char* command_bcast = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
  //if ( engine_code->intra_rank == 0 ) {
    int ichar;
    for ( ichar=0; ichar < MDI_COMMAND_LENGTH; ichar++) {
      command_bcast[ichar] = '\0';
    }
    for ( ichar=0; ichar < strlen(command) && ichar < MDI_COMMAND_LENGTH; ichar++ ) {
      command_bcast[ichar] = command[ichar];
    }
  //}
  /*
  if ( mpi_init_flag == 1) {
    //MPI_Bcast( &command_bcast[0], MDI_COMMAND_LENGTH, MPI_CHAR, 0, engine_code->intra_MPI_comm);
    MPI_Bcast( &command_bcast[0], MDI_COMMAND_LENGTH, MPI_CHAR, 0, *(MPI_Comm*)libd->shared_state->mpi_comm_ptr);
  }
  */

  // ensure that the driver is the current code
  library_set_driver_current(comm);

  // set the command for the engine to execute
  library_set_command(command_bcast, comm);

  if ( command_bcast[0] == '<' ) {
    // execute the command, so that the data from the engine can be received later by the driver
    libd->shared_state->engine_activate_code( libd->shared_state->engine_codes_ptr, libd->shared_state->engine_code_id );
    ret = libd->shared_state->lib_execute_command(libd->shared_state->engine_mdi_comm);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Send_Command: Unable to execute receive command through library");
      free( command_bcast );
      return ret;
    }

    libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr, libd->shared_state->driver_code_id );

    *skip_flag = 1;
  }
  else if ( command_bcast[0] == '>' ) {
    // flag the command to be executed after the next call to MDI_Send
    communicator* this;
    ret = get_communicator(codes.current_key, comm, &this);
    if ( ret != 0 ) {
      mdi_error("Error in plug_on_send_command: second get_communicator failed");
      return ret;
    }
    library_data* libd = (library_data*) this->method_data;
    libd->execute_on_send = 1;
    *skip_flag = 1;
  }
  else if ( strcmp( command_bcast, "EXIT" ) == 0 || command_bcast[0] == '@' ) {
    // this command should be received by MDI_Recv_command, rather than through the execute_command callback
  }
  else {
    // this is a command that neither sends nor receives data, so execute it now
    libd->shared_state->engine_activate_code( libd->shared_state->engine_codes_ptr, libd->shared_state->engine_code_id );
    libd->shared_state->lib_execute_command(libd->shared_state->engine_mdi_comm);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Send_Command: Unable to execute command through library");
      free( command_bcast );
      return ret;
    }

    libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr, libd->shared_state->driver_code_id );

    *skip_flag = 1;
  }

  free( command_bcast );
  return ret;
}



/*! \brief Callback after the PLUG method has received a command */
int plug_after_send_command(const char* command, MDI_Comm comm) {
  return 0;
}



/*! \brief Callback when the PLUG method must receive a command */
int plug_on_recv_command(MDI_Comm comm) {
  int ret;
  ret = mdi_debug("[MDI:plug_on_recv_command] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_recv_command: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_recv_command: get_current_code failed");
    return 1;
  }
  communicator* engine_comm;
  ret = get_communicator(codes.current_key, comm, &engine_comm);
  if ( ret != 0 ) {
    mdi_error("Error in plug_on_recv_command: get_communicator failed");
    return ret;
  }

  // get the driver code to which this communicator connects
  library_data* libd = (library_data*) engine_comm->method_data;
  int idriver = libd->connected_code;

  //MDI_Comm driver_comm_handle = library_get_matching_handle(comm);
  //communicator* driver_comm = get_communicator(idriver, driver_comm_handle);
  //library_data* driver_lib = (library_data*) driver_comm->method_data;

  // Copy the execute_command function to a shared location
  libd->shared_state->execute_command_wrapper = this_code->execute_command_wrapper;
  libd->shared_state->execute_command = this_code->execute_command;
  libd->shared_state->execute_command_obj = this_code->execute_command_obj;

  // set the current code to the driver
  libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr, 
                                            libd->shared_state->driver_code_id );

  void* mpi_ptr = libd->shared_state->mpi_comm_ptr;
  /*
  if ( this_code->language == MDI_LANGUAGE_FORTRAN ) {
    MPI_Comm* mpi_comm_ptr_cast = (MPI_Comm*)mpi_ptr;
    MPI_Fint f_comm = MPI_Comm_c2f( *mpi_comm_ptr_cast );
    mpi_ptr = (void*)(&f_comm);
  }
  */

  ret = libd->shared_state->driver_node_callback(mpi_ptr, libd->shared_state->driver_mdi_comm, libd->shared_state->driver_callback_obj);
  if ( ret != 0 ) {
    mdi_error("PLUG error in on_recv_command: driver_node_callback failed");
    return ret;
  }

  // set the current code to the engine
  libd->shared_state->engine_activate_code( libd->shared_state->engine_codes_ptr,
                                            libd->shared_state->engine_code_id );

  return 0;
}



/*! \brief Load the initialization function for a plugin
 *
 */
int library_load_init(const char* plugin_name, void* mpi_comm_ptr,
                      library_data* libd, int mode) {
  int ret;
  ret = mdi_debug("[MDI:library_load_init] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_load_init: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_load_init: get_current_code failed");
    return ret;
  }
  MPI_Comm mpi_comm = *(MPI_Comm*) mpi_comm_ptr;

  //
  // Get the path to the plugin
  // Note: Eventually, should probably replace this code with libltdl
  //
  char* plugin_path = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );

  // Get the name of the plugin's init function
  char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
  if ( mode == 0 ) { // Load MDI_Plugin_init
    snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_init_%s", plugin_name);
  }
  else { // Load MDI_Plugin_open
    snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_open_%s", plugin_name);
  }

  // Attempt to load a python script
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/%s.py", this_code->plugin_path, plugin_name);
  int exists_flag;
  ret = file_exists( plugin_path, &exists_flag );
  if ( ret != 0 ) {
    mdi_error("Error in library_load_init: file_exists failed");
    return ret;
  }
  if ( exists_flag ) {

    ret = mdi_debug("[MDI:library_load_init] Attempting to load a python plugin\n");
    if ( ret != 0 ) {
      mdi_error("Error in library_load_init: mdi_debug failed");
      return ret;
    }

    libd->is_python = 1;
    libd->shared_state->engine_language = MDI_LANGUAGE_PYTHON;
    ret = python_plugin_init( plugin_name, plugin_path, mpi_comm_ptr, libd->shared_state, mode );
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init");
      return -1;
    }
  }
  else {
    libd->is_python = 0;

#ifdef _WIN32
  // Attempt to open a library with a .dll extension
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.dll", this_code->plugin_path, plugin_name);
  libd->plugin_handle = LoadLibrary( plugin_path );
  if ( ! libd->plugin_handle ) {
    // Unable to find the plugin library
    free( plugin_path );
    mdi_error("Unable to open MDI plugin");
    return -1;
  }

  // Load a plugin's initialization function
  libd->plugin_init = (MDI_Plugin_init_t) (intptr_t) GetProcAddress( libd->plugin_handle, plugin_init_name );
  if ( ! libd->plugin_init ) {
    free( plugin_path );
    free( plugin_init_name );
    FreeLibrary( libd->plugin_handle );
    mdi_error("Unable to load MDI plugin init function");
    return -1;
  }

#else
  // Attempt to open a library with a .so extension
  snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.so", this_code->plugin_path, plugin_name);
  int dlopen_flag = RTLD_NOW | RTLD_LOCAL | RTLD_NODELETE;
  libd->plugin_handle = dlopen(plugin_path, dlopen_flag);
  if ( ! libd->plugin_handle ) {

    // Attempt to open a library with a .dylib extension
    snprintf(plugin_path, PLUGIN_PATH_LENGTH, "%s/lib%s.dylib", this_code->plugin_path, plugin_name);
    libd->plugin_handle = dlopen(plugin_path, dlopen_flag);
    if ( ! libd->plugin_handle ) {
      // Unable to find the plugin library
      free( plugin_path );
      free( plugin_init_name );
      mdi_error("Unable to open MDI plugin");
      return -1;
    }
  }

  // Load a plugin's initialization function
  libd->plugin_init = (MDI_Plugin_init_t) (intptr_t) dlsym(libd->plugin_handle, plugin_init_name);
  if ( ! libd->plugin_init ) {
    free( plugin_path );
    free( plugin_init_name );
    dlclose( libd->plugin_handle );
    mdi_error("Unable to load MDI plugin init function");
    return -1;
  }
#endif

    // Initialize an instance of the plugin
    ret = libd->plugin_init( libd->shared_state );
    if ( ret != 0 ) {
      mdi_error("MDI plugin init function returned non-zero exit code");
      return -1;
    }

  }

  // free memory from loading the plugin's initialization function
  free( plugin_path );
  free( plugin_init_name );

  return 0;
}



/*! \brief Parse command-line plugin options
 *
 */
int library_parse_options(const char* options, library_data* libd) {
  int ret;
  ret = mdi_debug("[MDI:library_parse_options] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_parse_options: mdi_debug failed");
    return ret;
  }

  // Begin parsing the options char array into an argv-style array of char arrays

  // copy the input options array
  int options_len = strlen(options) + 1;
  libd->shared_state->plugin_options = malloc( options_len * sizeof(char) );
  libd->shared_state->plugin_unedited_options = malloc( options_len * sizeof(char) );

  // zero both of the new arrays
  int ichar;
  for (ichar=0; ichar < options_len; ichar++) {
    libd->shared_state->plugin_options[ichar] = '\0';
    libd->shared_state->plugin_unedited_options[ichar] = '\0';
  }

  snprintf(libd->shared_state->plugin_options, options_len, "%s", options);
  snprintf(libd->shared_state->plugin_unedited_options, options_len, "%s", options);

  // determine the number of arguments
  libd->shared_state->plugin_argc = 0;
  int in_argument = 0; // was the previous character part of an argument, or just whitespace?
  int in_single_quotes = 0; // was the previous character part of a single quote?
  int in_double_quotes = 0; // was the previous character part of a double quote?
  for (ichar=0; ichar < options_len; ichar++) {
    if ( libd->shared_state->plugin_options[ichar] == '\0' ) {
      if ( in_double_quotes ) {
        mdi_error("Unterminated double quotes received in MDI_Launch_plugin \"options\" argument.");
      }
      if ( in_argument ) {
        libd->shared_state->plugin_argc++;
      }
      in_argument = 0;
    }
    else if (libd->shared_state->plugin_options[ichar] == ' ') {
      if ( ! in_double_quotes && ! in_single_quotes ) {
        if ( in_argument ) {
          libd->shared_state->plugin_argc++;
        }
        in_argument = 0;
        libd->shared_state->plugin_options[ichar] = '\0';
      }
    }
    else if (libd->shared_state->plugin_options[ichar] == '\"') {
      if ( in_single_quotes ) {
        mdi_error("Nested quotes not supported by MDI_Launch_plugin \"options\" argument.");
      }
      in_argument = 1;
      in_double_quotes = (in_double_quotes + 1) % 2;
      libd->shared_state->plugin_options[ichar] = '\0';
    }
    else if (libd->shared_state->plugin_options[ichar] == '\'') { 
      if ( in_double_quotes ) {
        mdi_error("Nested quotes not supported by MDI_Launch_plugin \"options\" argument.");
      }
      in_argument = 1;
      in_single_quotes = (in_single_quotes + 1) % 2;
      libd->shared_state->plugin_options[ichar] = '\0';
    }
    else {
      in_argument = 1;
    }
  }

  // construct pointers to all of the arguments
  libd->shared_state->plugin_argv = malloc( libd->shared_state->plugin_argc * sizeof(char*) );
  libd->shared_state->plugin_argv_allocated = 1;
  int iarg = 0;
  for (ichar=0; ichar < options_len; ichar++) {
    if ( libd->shared_state->plugin_options[ichar] != '\0' ) {
      if ( ichar == 0 || libd->shared_state->plugin_options[ichar-1] == '\0' ) {
        libd->shared_state->plugin_argv[iarg] = &libd->shared_state->plugin_options[ichar];
        iarg++;
      }
    }
  }
  if ( iarg != libd->shared_state->plugin_argc ) {
    mdi_error("Programming error: unable to correctly parse the MDI_Launch_plugin \"options\" argument.");
  }

  return 0;
}



/*! \brief Launch an MDI plugin
 *
 */
int library_launch_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                          MDI_Driver_node_callback_t driver_node_callback,
                          void* driver_callback_object) {
  int ret;
  ret = mdi_debug("[MDI:library_launch_plugin] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: mdi_debug failed");
    return ret;
  }

  ret = mdi_debug("[MDI:library_launch_plugin] Name: %s\n", plugin_name);
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: mdi_debug failed");
    return ret;
  }
  ret = mdi_debug("[MDI:library_launch_plugin] Options: %s\n", options);
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: second mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: get_current_code failed");
    return 1;
  }
  MPI_Comm mpi_comm = *(MPI_Comm*) mpi_comm_ptr;

  // initialize a communicator for the driver
  int icomm = library_initialize();
  communicator* driver_comm;
  ret = get_communicator(codes.current_key, icomm, &driver_comm);
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) driver_comm->method_data;
  libd->connected_code = (int)codes.size;

  // allocate data that is shared between the driver and the plugin
  libd->shared_state = malloc(sizeof(plugin_shared_state));
  libd->shared_state->plugin_argc = 0;
  libd->shared_state->plugin_argv_allocated = 0;
  libd->shared_state->buf_allocated = 0;
  libd->shared_state->driver_code_id = codes.current_key;
  libd->shared_state->engine_language = MDI_LANGUAGE_C;
  libd->shared_state->python_interpreter_initialized = 0;
  libd->shared_state->driver_codes_ptr = &codes;
  libd->shared_state->driver_version[0] = MDI_MAJOR_VERSION;
  libd->shared_state->driver_version[1] = MDI_MINOR_VERSION;
  libd->shared_state->driver_version[2] = MDI_PATCH_VERSION;

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

  // Parse plugin command-line options
  library_parse_options(options, libd);

  libd->shared_state->mpi_comm_ptr = &libd->mpi_comm;
  libd->shared_state->driver_node_callback = libd->driver_node_callback;
  libd->shared_state->driver_mdi_comm = driver_comm->id;
  libd->shared_state->driver_activate_code = library_activate_code;
  libd->shared_state->driver_callback_obj = libd->driver_callback_obj;



  /*************************************************/
  /*************** BEGIN PLUGIN MODE ***************/
  /*************************************************/

  ret = library_load_init(plugin_name, mpi_comm_ptr, libd, 0);
  if ( ret != 0 ) {
    return ret;
  }

  ret = mdi_debug("[MDI:library_launch_plugin] Finished call to library_load_init\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: third mdi_debug failed");
    return ret;
  }

  // store a couple of values from libd, before we delete it
  int is_python = libd->is_python;
#ifdef _WIN32
  HINSTANCE plugin_handle = libd->plugin_handle;
#else
  void* plugin_handle = libd->plugin_handle;
#endif

  libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr, libd->shared_state->driver_code_id );

  // Delete the driver's communicator to the engine
  // This will also delete the engine code and its communicator
  delete_communicator(libd->shared_state->driver_code_id, comm);

  ret = mdi_debug("[MDI:library_launch_plugin] Finished call to delete_communicator\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: fourth mdi_debug failed");
    return ret;
  }

  if (is_python == 0 ) {
  // Close the plugin library
#ifdef _WIN32
    FreeLibrary( plugin_handle );
#else
    ret = dlclose( plugin_handle );
    if ( ret != 0 ) {
      mdi_error("Plugin dlclose failed");
      return 1;
    }
#endif
  }

  ret = mdi_debug("[MDI:library_launch_plugin] Finished closing the plugin\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: fifth mdi_debug failed");
    return ret;
  }

  /*************************************************/
  /**************** END PLUGIN MODE ****************/
  /*************************************************/



  ret = mdi_debug("[MDI:library_launch_plugin] Finished\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_launch_plugin: sixth mdi_debug failed");
    return ret;
  }

  return 0;
}



/*! \brief Open an MDI plugin in the background
 *
 */
int library_open_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                          MDI_Comm* mdi_comm_ptr) {
  int ret;
  ret = mdi_debug("[MDI:library_open_plugin] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_open_plugin: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_open_plugin: get_current_code failed");
    return 1;
  }

  // if necessary, convert the mpi_comm_ptr to C format
  MPI_Comm mpi_comm;
  if ( this_code->language == MDI_LANGUAGE_FORTRAN ) {
    MPI_Fint* f_comm_ptr = (MPI_Fint*) mpi_comm_ptr;
    mpi_comm = MPI_Comm_f2c( *f_comm_ptr );
  }
  else {
    mpi_comm = *(MPI_Comm*) mpi_comm_ptr;
  }

  // initialize a communicator for the driver
  int icomm = library_initialize();
  communicator* driver_comm;
  ret = get_communicator(codes.current_key, icomm, &driver_comm);
  if ( ret != 0 ) {
    mdi_error("Error in library_open_plugin: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) driver_comm->method_data;
  libd->connected_code = (int)codes.size;

  MDI_Comm comm;
  ret = MDI_Accept_Communicator(&comm);
  if ( ret != 0 || comm == MDI_COMM_NULL ) {
    mdi_error("MDI unable to create communicator for plugin");
    return -1;
  }

  // Set the driver callback function to be used by this plugin instance
  libd->driver_callback_obj = NULL;
  libd->driver_node_callback = NULL;

  // Set the mpi communicator associated with this plugin instance
  libd->mpi_comm = mpi_comm;

  // allocate data that is shared between the driver and the plugin
  libd->shared_state = malloc(sizeof(plugin_shared_state));
  libd->shared_state->plugin_argc = 0;
  libd->shared_state->plugin_argv_allocated = 0;
  libd->shared_state->buf_allocated = 0;
  libd->shared_state->driver_code_id = codes.current_key;
  libd->shared_state->engine_language = MDI_LANGUAGE_C;
  libd->shared_state->python_interpreter_initialized = 0;
  libd->shared_state->driver_codes_ptr = &codes;
  libd->shared_state->driver_version[0] = MDI_MAJOR_VERSION;
  libd->shared_state->driver_version[1] = MDI_MINOR_VERSION;
  libd->shared_state->driver_version[2] = MDI_PATCH_VERSION;

  // Parse plugin command-line options
  library_parse_options(options, libd);

  libd->shared_state->mpi_comm_ptr = &libd->mpi_comm;
  libd->shared_state->driver_node_callback = libd->driver_node_callback;
  libd->shared_state->driver_mdi_comm = driver_comm->id;
  libd->shared_state->driver_activate_code = library_activate_code;
  libd->shared_state->driver_callback_obj = libd->driver_callback_obj;

  // Assign the global command-line options variables to the values for this plugin  
  //plugin_argc = libd->shared_state->plugin_argc;
  //plugin_argv = libd->shared_state->plugin_argv;
  //plugin_unedited_options = libd->plugin_unedited_options;

  //
  // Get the path to the plugin
  // Note: Eventually, should probably replace this code with libltdl
  //
  char* plugin_path = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );

  // Get the name of the plugin's init function
  char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
  snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_open_%s", plugin_name);

  /*************************************************/
  /*************** BEGIN PLUGIN MODE ***************/
  /*************************************************/

  library_load_init(plugin_name, &mpi_comm, libd, 1);
  if ( ret != 0 ) {
    free( plugin_path );
    free( plugin_init_name );
    return ret;
  }

  /*************************************************/
  /**************** END PLUGIN MODE ****************/
  /*************************************************/

  // Set the driver as the active code
  libd->shared_state->driver_activate_code(
                  libd->shared_state->driver_codes_ptr,
                  libd->shared_state->driver_code_id );

  // Delete the driver's communicator to the engine
  // This will also delete the engine code and its communicator
  //delete_communicator(driver_code_id, comm);

  // free memory from loading the plugin's initialization function
  free( plugin_path );
  free( plugin_init_name );

  *mdi_comm_ptr = comm;
  return 0;
}

int library_close_plugin(MDI_Comm mdi_comm) {
  int ret;
  ret = mdi_debug("[MDI:library_close_plugin] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_close_plugin: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_close_plugin: get_current_code failed");
    return 1;
  }
  communicator* this_comm;
  ret = get_communicator(this_code->id, mdi_comm, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in library_close_plugin: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) this_comm->method_data;

  if (libd->is_python == 0 ) {
  // Close the plugin library
#ifdef _WIN32
    FreeLibrary( libd->plugin_handle );
#else
    dlclose( libd->plugin_handle );
#endif
  }

  // Delete the driver's communicator to the engine
  // This will also delete the engine code and its communicator
  delete_communicator(codes.current_key, mdi_comm);
  
  return 0;
}


/*! \brief Perform initialization of a communicator for library-based communication
 *
 */
int library_initialize() {
  int ret;
  ret = mdi_debug("[MDI:library_initialize] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_initialize: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_initialize: get_current_code failed");
    return 1;
  }

  MDI_Comm comm_id;
  ret = new_communicator(this_code->id, MDI_LINK, &comm_id);
  if ( ret != 0 ) {
    mdi_error("Error in library_initialize: new_communicator failed");
    return 1;
  }
  communicator* new_comm;
  ret = get_communicator(this_code->id, comm_id, &new_comm);
  if ( ret != 0 ) {
    mdi_error("Error in library_initialize: get_communicator failed");
    return ret;
  }
  new_comm->delete = communicator_delete_lib;
  new_comm->send = library_send;
  new_comm->recv = library_recv;

  // set the MDI version number of the new communicator
  new_comm->mdi_version[0] = MDI_MAJOR_VERSION;
  new_comm->mdi_version[1] = MDI_MINOR_VERSION;
  new_comm->mdi_version[2] = MDI_PATCH_VERSION;
  new_comm->name_length = MDI_NAME_LENGTH;
  new_comm->command_length = MDI_COMMAND_LENGTH;

  // allocate the method data
  library_data* libd = malloc(sizeof(library_data));

  // initialize the libd data
  libd->execute_on_send = 0;
  libd->mpi_comm = MPI_COMM_NULL;

  new_comm->method_data = libd;

  // if this is an engine, go ahead and set the driver as the connected code
  if ( strcmp(this_code->role, "ENGINE") == 0 ) {

    libd->shared_state = (plugin_shared_state*) this_code->shared_state_from_driver;
    libd->shared_state->engine_mdi_comm = new_comm->id;
    libd->shared_state->delete_engine = library_delete_engine;
    libd->shared_state->engine_activate_code = library_activate_code;
    libd->shared_state->lib_execute_command = library_execute_command;
    libd->shared_state->engine_code_id = codes.current_key;
    libd->shared_state->engine_codes_ptr = &codes;
    libd->shared_state->execute_builtin = general_builtin_command;
    libd->shared_state->engine_nodes = (void*)this_code->nodes;
    libd->shared_state->engine_version[0] = MDI_MAJOR_VERSION;
    libd->shared_state->engine_version[1] = MDI_MINOR_VERSION;
    libd->shared_state->engine_version[2] = MDI_PATCH_VERSION;
    this_code->plugin_argc_ptr = &libd->shared_state->plugin_argc;
    this_code->plugin_argv_ptr = &libd->shared_state->plugin_argv;
    this_code->plugin_unedited_options_ptr = &libd->shared_state->plugin_unedited_options;

    // if the plugin has already called MDI_Set_execute_command_func, set these values for the library
    // otherwise, these values will be set when MDI_Set_execute_command_func is called
    if ( this_code->called_set_execute_command_func ) {
      libd->shared_state->execute_command_wrapper = this_code->execute_command_wrapper;
      libd->shared_state->execute_command = this_code->execute_command;
      libd->shared_state->execute_command_obj = this_code->execute_command_obj;
    }

    // set the engine's mpi communicator
    libd->mpi_comm = *(MPI_Comm*)libd->shared_state->mpi_comm_ptr;
    this_code->intra_MPI_comm = libd->mpi_comm;

    // check whether MPI has been initialized
    int mpi_init_flag;
    if ( MPI_Initialized(&mpi_init_flag) ) {
      mdi_error("Error in MDI_Init: MPI_Initialized failed");
      return 1;
    }

    // Set the engine's MPI rank
    if ( mpi_init_flag == 1 ) {
      MPI_Comm_rank( this_code->intra_MPI_comm, &this_code->intra_rank );
    }
    else {
      this_code->intra_rank = 0;
    }
      
    libd->shared_state->intra_rank = this_code->intra_rank;

  }

  return new_comm->id;
}


/*! \brief Set the driver as the current code
 *
 */
int library_set_driver_current(MDI_Comm comm) {
  int ret;
  ret = mdi_debug("[MDI:library_set_driver_current] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_set_driver_current: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_set_driver_current: get_current_code failed");
    return 1;
  }
  communicator* this_comm;
  ret = get_communicator(codes.current_key, comm, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in library_set_driver_current: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) this_comm->method_data;

  // set the driver as the active code
  libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr,
                                            libd->shared_state->driver_code_id );

  return 0;
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
  int ret;
  ret = mdi_debug("[MDI:library_set_command] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_set_command: mdi_debug failed");
    return ret;
  }

  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in library_set_command: get_communicator failed");
    return ret;
  }

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;
  //int iengine = libd->connected_code;

  // get the matching engine communicator
  //MDI_Comm engine_comm_handle = library_get_matching_handle(comm);
  //communicator* engine_comm = get_communicator(iengine, engine_comm_handle);

  // set the command
  //library_data* engine_lib = (library_data*) engine_comm->method_data;
  //snprintf(engine_lib->command, MDI_COMMAND_LENGTH_, "%s", command);
  snprintf(libd->shared_state->command, MDI_COMMAND_LENGTH_, "%s", command);

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
  ret = mdi_debug("[MDI:library_execute_command] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_execute_command: mdi_debug failed");
    return ret;
  }

  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in library_execute_command: get_communicator failed");
    return ret;
  }

  // get the engine code to which this communicator connects
  library_data* libd = (library_data*) this->method_data;

  MDI_Comm engine_comm_handle = libd->shared_state->engine_mdi_comm;

  // set the current code to the engine
  libd->shared_state->engine_activate_code( libd->shared_state->engine_codes_ptr,
                                            libd->shared_state->engine_code_id );

  // check if this command corresponds to one of MDI's standard built-in commands
  int builtin_flag;
  ret = libd->shared_state->execute_builtin(libd->shared_state->command,
                                            engine_comm_handle,
                                            &builtin_flag);

  if ( builtin_flag == 0 ) {
    // call execute_command now
    void* class_obj = libd->shared_state->execute_command_obj;
    ret = libd->shared_state->execute_command_wrapper(libd->shared_state->command, engine_comm_handle, class_obj);
  }

  // set the current code to the driver
  libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr,
                                            libd->shared_state->driver_code_id );

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
  ret = mdi_debug("[MDI:library_send] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_send: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_send: get_current_code failed");
    return 1;
  }
  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in library_send: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) this->method_data;

  // get the rank of this process on the engine
  /*
  int engine_rank = 0;
  if ( this_code->is_library ) {
    engine_rank = this_code->intra_rank;
  }
  else {
    engine_rank = other_code->intra_rank;
  }
  */
  int engine_rank = libd->shared_state->intra_rank;

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
      if ( libd->shared_state->buf_allocated != 0 ) {
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
      libd->shared_state->buf = malloc( msg_bytes );
      libd->shared_state->buf_allocated = 1;

      // copy the header into libd->buf
      memcpy(libd->shared_state->buf, buf, nheader_actual * sizeof(int));

    }
    else if ( msg_flag == 2 ) { // message body

      // confirm that libd->buf has been allocated
      int has_header = 1;
      if ( libd->shared_state->buf_allocated == 0 ) {
        // libd->buf has not been allocated, which means there is no header
        has_header = 0;
        libd->shared_state->buf = malloc( datasize * count );
        libd->shared_state->buf_allocated = 1;
      }

      int offset = 0;
      if ( has_header == 1 ) {
        offset = nheader_actual * sizeof(int);
      }

      // copy the body into libd->buf
      memcpy((char*)libd->shared_state->buf + offset, buf, datasize * count);

    }
    else {

      mdi_error("MDI library unknown msg_flag in send\n");
      return 1;

    }

  }

  // check whether the recipient code should now execute its command
  if ( msg_flag == 2 && libd->execute_on_send ) {
    // have the recipient code execute its command
    libd->shared_state->engine_activate_code( libd->shared_state->engine_codes_ptr, libd->shared_state->engine_code_id );
    libd->shared_state->lib_execute_command(libd->shared_state->engine_mdi_comm);
    libd->shared_state->driver_activate_code( libd->shared_state->driver_codes_ptr, libd->shared_state->driver_code_id );

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
  ret = mdi_debug("[MDI:library_recv] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_recv: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_recv: get_current_code failed");
    return 1;
  }
  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in library_recv: get_communicator failed");
    return ret;
  }
  library_data* libd = (library_data*) this->method_data;

  //MDI_Comm other_comm_handle = library_get_matching_handle(comm);
  //communicator* other_comm = get_communicator(libd->connected_code, other_comm_handle);
  //library_data* other_lib = (library_data*) other_comm->method_data;

  // only recv from rank 0 of the engine
  /*
  int engine_rank = 0;
  if ( this_code->is_library ) {
    engine_rank = this_code->intra_rank;
  }
  else {
    engine_rank = other_code->intra_rank;
  }
  */
  int engine_rank = libd->shared_state->intra_rank;
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
  if ( libd->shared_state->buf_allocated != 1 ) {
    mdi_error("MDI send buffer is not allocated");
    return 1;
  }

  // receive message header information
  // only do this if communicating with MDI version 1.1 or higher
  if ( ( this->mdi_version[0] > 1 ||
	 ( this->mdi_version[0] == 1 && this->mdi_version[1] >= 1 ) )
       && this_code->ipi_compatibility != 1 ) {

    if ( msg_flag == 1 ) { // message header

      memcpy(buf, libd->shared_state->buf, count * datasize);

    }
    else if ( msg_flag == 2 ) { // message body

      int nheader = 4;
      int offset = nheader * sizeof(int);
      memcpy(buf, (char*)libd->shared_state->buf + offset, count * datasize);

      // free the memory of libd->buf
      free( libd->shared_state->buf );
      libd->shared_state->buf_allocated = 0;

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
  int ret;

  ret = mdi_debug("[MDI:communicator_delete_lib] Communicator: %p\n", comm);
  if ( ret != 0 ) {
    mdi_error("Error in communicator_delete_lib: mdi_debug failed");
    return ret;
  }

  communicator* this_comm = (communicator*) comm;
  code* this_code;
  ret = get_code(this_comm->code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in communicator_delete_lib: get_code failed");
    return ret;
  }
  library_data* libd = (library_data*) this_comm->method_data;

  // if this is the driver, delete the engine code and shared state
  if ( this_code->is_library == 0 ) {

    ret = mdi_debug("[MDI:communicator_delete_lib] This is a driver.\n");
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: second mdi_debug failed");
      return ret;
    }

    // set the engine as the active code
    ret = libd->shared_state->engine_activate_code(
                    libd->shared_state->engine_codes_ptr,
                    libd->shared_state->engine_code_id );
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: engine_activate_code failed");
      return ret;
    }

    ret = mdi_debug("[MDI:communicator_delete_lib] The driver is deleting the engine code\n", comm);
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: third mdi_debug failed");
      return ret;
    }

    // delete the engine code
    ret = libd->shared_state->delete_engine(
                    libd->shared_state->engine_code_id );
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: delete_engine failed");
      return ret;
    }

    // set the driver as the active code
    ret = libd->shared_state->driver_activate_code(
                    libd->shared_state->driver_codes_ptr,
                    libd->shared_state->driver_code_id );
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: driver_activate_code failed");
      return ret;
    }

    ret = mdi_debug("[MDI:communicator_delete_lib] The driver has finished deleting the engine code\n", comm);
    if ( ret != 0 ) {
      mdi_error("Error in communicator_delete_lib: fourth mdi_debug failed");
      return ret;
    }

    if ( libd->shared_state->plugin_argv_allocated ) {
      free( libd->shared_state->plugin_argv );
      free( libd->shared_state->plugin_options );
      free( libd->shared_state->plugin_unedited_options );
    }

    // delete the method-specific information
    if ( libd->shared_state->buf_allocated ) {
      free( libd->shared_state->buf );
    }

    // delete the shared state, but only on the driver side
    free( libd->shared_state );

  }

  free( libd );

  ret = mdi_debug("[MDI:communicator_delete_lib] Finished\n");
  if ( ret != 0 ) {
    mdi_error("Error in communicator_delete_lib: fifth mdi_debug failed");
    return ret;
  }

  return 0;
}


/*! \brief Function to delete all of the engine's state
 */
int library_delete_engine(size_t code_id) {
  int ret;

  ret = mdi_debug("[MDI:library_delete_engine] Code ID: %lu\n", code_id);
  if ( ret != 0 ) {
    mdi_error("Error in library_delete_engine: mdi_debug failed");
    return ret;
  }

  delete_code(code_id);
  if ( codes.size == 0 ) {
    free(codes.data);
    codes.initialized = 0;
    codes.current_key = 0;
  }
  return 0;
}


/*! \brief Function to set the plugin's state
 */
int library_set_state(void* state) {
  int ret;
  ret = mdi_debug("[MDI:library_set_state] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in library_set_state: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in library_set_state: get_current_code failed");
    return 1;
  }
  this_code->shared_state_from_driver = state;

  plugin_shared_state* shared_state = (plugin_shared_state*) state;

  ret = MDI_Init_with_argv(&shared_state->plugin_argc, 
                           &shared_state->plugin_argv);
  if ( ret != 0 ) {
    return ret;
  }

  return 0;
}


/*! \brief Function to set the active code
 */
int library_activate_code(void* codes_in, int code_id) {

  vector* codes_vec = (vector*) codes_in;
  codes_vec->current_key = code_id;

  return 0;
}
