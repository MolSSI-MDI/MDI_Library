/*! \file
 *
 * \brief Global structures used by MDI
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
  #include <winsock2.h>
  #include <windows.h>
#else
  #include <unistd.h>
#endif
#include <stdint.h>
#include <stdarg.h>
#include "mdi_global.h"

/*! \brief Vector containing all codes that have been initiailized on this rank
 * Typically, this will only include a single code, unless the communication method is LIBRARY */
vector codes = { .initialized = 0 };


/*! \brief Initialize memory allocation for a vector structure
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       v
 *                   Pointer to the vector structure for which the memory will be allocated
 * \param [in]       stride
 *                   Stride of the vector
 */
int vector_init(vector* v, size_t stride) {
  //initialize the vector with the given stride
  v->data = malloc(0);
  if (!v->data) {
    mdi_error("Could not initialize vector");
    return 1;
  }

  v->size = 0;
  v->capacity = 0;
  v->stride = stride;
  v->current_key = -1;
  v->initialized = 1;

  return 0;
}

/*! \brief Append a new element to the end of the vector
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       v
 *                   Pointer to the vector to which the element will be appended
 * \param [in]       element
 *                   Pointer to the element that will be appended to the vector
 */
int vector_push_back(vector* v, void* element) {
  //grow the vector
  if (v->size >= v->capacity) {
    size_t new_capacity;
    if ( v->capacity > 0 ) {
      new_capacity = 2 * v->capacity;
    }
    else {
      new_capacity = 1;
    }
    void* new_data = malloc( v->stride * new_capacity );
    memcpy(new_data, v->data, v->size * v->stride);
    free(v->data);
    v->data = new_data;
    v->capacity = new_capacity;
  }

  //add the new data to the vector
  memcpy( v->data + (v->size * v->stride), (unsigned char*)element, v->stride );
  v->size++;

  return 0;
}

/*! \brief Remove an element from a vector
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       v
 *                   Pointer to the vector from which the element will be removed
 * \param [in]       index
 *                   Index of the element that will be removed from the vector
 */
int vector_delete(vector* v, int index) {
  int ret;

  ret = mdi_debug("[MDI:vector_delete] Vector, index: %p %d\n", v, index);
  if ( ret != 0 ) { return ret; }


  // copy the data from the last element to the element that is being deleted
  // don't do this if the element being deleted is the last element
  if ( index + 1 != v->size ) {
    memcpy( v->data + (index * v->stride), v->data + ( (v->size - 1) * v->stride ), v->stride );
  }
  v->size--;

  // shrink the vector
  if (v->size <= v->capacity / 2) {
    size_t new_capacity = v->capacity / 2;
    void* new_data = malloc( v->stride * new_capacity );
    memcpy(new_data, v->data, v->stride * v->size );
    free(v->data);
    v->data = new_data;
    v->capacity = new_capacity;
  }

  return 0;
}

/*! \brief Free all data associated with a vector
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       v
 *                   Pointer to the vector that will be freed
 */
int vector_free(vector* v) {
  int ret;

  ret = mdi_debug("[MDI:vector_free] Vector: %p\n", v);
  if ( ret != 0 ) { return ret; }

  free(v->data);
  free(v);
  return 0;
}

/*! \brief Return a pointer to an element of a vector
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       index
 *                   Index of the element within the vector
 */
int vector_get(vector* v, int index, void** element) {
  if (index < 0 || index >= v->size) {
    mdi_error("Vector accessed out-of-bounds");
    return 1;
  }
  *element = ( void* )( v->data + (index * v->stride) );
  return 0;
}

/*! \brief Determine the index of a node within a vector of nodes
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       node_name
 *                   Name of the node
 */
int get_node_index(vector* v, const char* node_name, int* node_index) {
  int ret;
  int inode;
  int correct_node = -1;
  for ( inode = 0; inode < v->size; inode++ ) {
    node* this_node;
    ret = vector_get(v, inode, (void**)&this_node);
    if ( ret != 0 ) {
      mdi_error("Error in get_node_index: vector_get failed");
      return 1;
    }
    if ( strcmp( node_name, this_node->name ) == 0 ) {
      correct_node = inode;
    }
  }
  *node_index = correct_node;
  return 0;
}

/*! \brief Determine the index of a command within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       command_name
 *                   Name of the command
 */
int get_command_index(node* n, const char* command_name, int* command_index) {
  int ret;
  int icommand;
  int correct_command = -1;
  char* this_command;

  for ( icommand = 0; icommand < n->commands->size; icommand++ ) {
    ret = vector_get( n->commands, icommand, (void**)&this_command );
    if ( ret != 0 ) {
      mdi_error("Error in get_node_index: vector_get failed");
      return 1;
    }
    if ( strcmp( command_name, this_command ) == 0 ) {
      correct_command = icommand;
    }
  }
  *command_index = correct_command;
  return 0;
}

/*! \brief Determine the index of a callback within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       callback_name
 *                   Name of the callback
 */
int get_callback_index(node* n, const char* callback_name, int* callback_index) {
  int ret;
  int icallback;
  int correct_callback = -1;
  char* this_callback;

  for ( icallback = 0; icallback < n->callbacks->size; icallback++ ) {
    ret = vector_get( n->callbacks, icallback, (void**)&this_callback );
    if ( ret != 0 ) {
      mdi_error("Error in get_node_index: vector_get failed");
      return 1;
    }
    if ( strcmp( callback_name, this_callback ) == 0 ) {
      correct_callback = icallback;
    }
  }
  *callback_index = correct_callback;
  return 0;
}


/*! \brief Determine the index of a callback within a node
 */
int free_node_vector(vector* v) {
  int ret;
  int inode = 0;
  size_t nnodes = v->size;

  for ( inode = 0; inode < nnodes; inode++ ) {
    node* this_node;
    ret = vector_get(v, inode, (void**)&this_node );
    if ( ret != 0 ) {
      mdi_error("Error in free_node_vector: vector_get failed");
      return ret;
    }

    // free the "commands" and "callbacks" vectors for this node
    vector_free(this_node->commands);
    vector_free(this_node->callbacks);
  }

  // free this node vector
  vector_free(v);

  return 0;
}


/*! \brief Create a new code structure and add it to the list of codes
 * Returns the index of the new code
 */
int new_code(size_t* code_id) {
  int ret;

  code* new_code = malloc(sizeof(code));
  new_code->returned_comms = 0;
  new_code->next_comm = 1;
  new_code->intra_MPI_comm = MPI_COMM_WORLD;
  new_code->language = MDI_LANGUAGE_C;
  new_code->language_on_destroy = NULL;
  new_code->selected_method_id = 0;
  new_code->initialized_mpi = 0;
  new_code->ipi_compatibility = 0;
  new_code->plugin_argc_ptr = NULL;
  new_code->plugin_argv_ptr = NULL;
  new_code->plugin_unedited_options_ptr = NULL;
  new_code->tcp_initialized = 0;
  new_code->mpi_initialized = 0;
  new_code->test_initialized = 0;
  new_code->shared_state_from_driver = NULL;
  new_code->tcp_socket = -1;
  new_code->port = -1;
  new_code->hostname = NULL;
  new_code->debug_mode = 0;
  new_code->execute_command = NULL;

  // initialize the name and role strings
  int ichar;
  for (ichar=0; ichar < MDI_NAME_LENGTH_; ichar++) {
    new_code->name[ichar] = '\0';
    new_code->role[ichar] = '\0';
  }

  // initialize the character buffer for the plugin path
  new_code->plugin_path = malloc(PLUGIN_PATH_LENGTH * sizeof(char));
  for (ichar=0; ichar < PLUGIN_PATH_LENGTH; ichar++) {
    new_code->plugin_path[ichar] = '\0';
  }

  new_code->is_library = 0;
  new_code->id = (int)codes.size;
  new_code->called_set_execute_command_func = 0;
  new_code->intra_rank = 0;
  new_code->world_rank = -1;
  new_code->world_size = -1;

  // initialize the node vector
  vector* node_vec = malloc(sizeof(vector));
  ret = vector_init(node_vec, sizeof(node));
  if ( ret != 0 ) {
    mdi_error("Error in new_code: could not initialize node vector");
    return ret;
  }
  new_code->nodes = node_vec;

  // initialize the comms vector
  vector* comms_vec = malloc(sizeof(vector));
  vector_init(comms_vec, sizeof(communicator));
  if ( ret != 0 ) {
    mdi_error("Error in new_code: could not initialize comms vector");
    return ret;
  }
  new_code->comms = comms_vec;

  // initialize the methods vector
  vector* methods_vec = malloc(sizeof(vector));
  vector_init(methods_vec, sizeof(method));
  if ( ret != 0 ) {
    mdi_error("Error in new_code: could not initialize methods vector");
    return ret;
  }
  new_code->methods = methods_vec;

  // add the new code to the global vector of codes
  element elem;
  elem.id = new_code->id;
  elem.data = new_code;
  ret = vector_push_back( &codes, &elem );
  if ( ret != 0 ) {
    mdi_error("Error in new_code: could not add code to global codes vector");
    return ret;
  }

  // return the index of the new code
  *code_id = codes.size - 1;
  
  return 0;
}


/*! \brief Get a code from a code handle
 * Returns a pointer to the code
 */
int get_code(size_t code_id, code** ret_code) {
  int ret;

  // Search through all of the codes for the one that matches code_id
  int icode;
  for (icode = 0; icode < codes.size; icode++ ) {

    element* this_element;

    ret = vector_get( &codes, icode, (void**)&this_element );
    if ( ret != 0 ) {
      mdi_error("Error in get_code: vector_get failed");
      return ret;
    }
    if ( this_element->id == code_id ) {
      *ret_code = this_element->data;
      return 0;
    }

  }
  mdi_error("Code not found");
  return 1;
}


/*! \brief Get the currently active code
 * Returns a pointer to the code
 */
int get_current_code(code** this_code_ptr) {
  int ret;
  ret = get_code(codes.current_key, this_code_ptr);
  if ( ret != 0 ) {
    mdi_error("Error in get_current_code: get_code failed");
    return ret;
  }
  return 0;
}


/*! \brief Delete a code
 * Returns 0 on success
 */
int delete_code(size_t code_id) {
  int ret;
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  ret = mdi_debug("[MDI:delete_code] Code ID: %lu\n", code_id);
  if ( ret != 0 ) { return ret; }

  // Call the langauge-specific destructor
  if ( this_code->language_on_destroy != NULL ) {
    if ( this_code->language_on_destroy(code_id) != 0 ) {
      mdi_error("Language-specific code deletion function failed");
      return 1;
    }
  }

  // Search through all of the codes for the one that matches code_id
  int icode;
  int code_index;
  int code_found = 0;
  for (icode = 0; icode < codes.size; icode++ ) {
    element* check_code;

    ret = vector_get( &codes, icode, (void**)&check_code );
    if ( ret != 0 ) {
      mdi_error("Error in delete_code: vector_get failed");
      return ret;
    }
    if ( check_code->id == code_id ) {
      code_found = 1;
      code_index = icode;

      // stop searching
      icode = (int)codes.size;
    }
  }
  if ( code_found != 1 ) {
    mdi_error("Code not found during delete");
    return 1;
  }

  // delete the plugin path
  free( this_code->plugin_path );

  // delete the node vector
  free_node_vector(this_code->nodes);

  // delete the methods vector
  free_methods_vector(this_code->methods);

  // delete the comms vector
  int icomm;
  size_t ncomms = this_code->comms->size;
  for (icomm = 0; icomm < ncomms; icomm++) {
    communicator* this_comm;
    ret = vector_get( this_code->comms, (int)this_code->comms->size - 1, (void**)&this_comm );
    if ( ret != 0 ) {
      mdi_error("Error in delete_code: vector_get for comms vector failed");
      return ret;
    }
    delete_communicator(code_id, this_comm->id);
  }
  vector_free( this_code->comms );

  // delete the data for this code from the global vector of codes
  vector_delete(&codes, code_index);

  // delete the data for the code structure
  free( this_code );

  return 0;
}


/*! \brief Create a new method structure and add it to the vector of methods
 * Returns the handle of the new method
 */
int new_method(size_t code_id, int method_id, int* id_ptr) {
  int ret;

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  method new_method;
  new_method.id = (int)this_code->methods->size;
  new_method.method_id = method_id;

  vector_push_back( this_code->methods, &new_method );

  *id_ptr = new_method.id;
  return 0;
}


/*! \brief Get a method from a method handle
 * Returns a pointer to the method
 */
int get_method(size_t code_id, int method_id, method** method_ptr) {
  int ret;

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in get_method: get_code failed");
    return ret;
  }

  // Search through all of the codes for the one that matches code_id
  int imethod;
  for (imethod = 0; imethod < this_code->methods->size; imethod++ ) {
    method* this_method;
    ret = vector_get( this_code->methods, imethod, (void**)&this_method );
    if ( ret != 0 ) {
      mdi_error("Error in get_method: vector_get failed");
      return ret;
    }
    if ( this_method->method_id == method_id ) {
      *method_ptr = this_method;
      return 0;
    }
  }
  mdi_error("Method not supported for this build of the MDI Library");
  return 1;
}


/*! \brief Delete a method
 * Returns 0 on success
 */
int delete_method(size_t code_id, int method_id) {
  int ret;

  ret = mdi_debug("[MDI:delete_method] Code ID, Method ID: %lu %d\n", code_id, method_id);
  if ( ret != 0 ) { return ret; }

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  method* this_method;
  ret = get_method(code_id, method_id, &this_method);
  if ( ret != 0 ) {
    mdi_error("Error in enable_plug_support: delete_method failed");
    return ret;
  }

  // Search through all of the methods for the one that matches method
  int imethod;
  int method_index;
  int method_found = 0;
  for (imethod = 0; imethod < this_code->methods->size; imethod++ ) {
    method* check_method;
    ret = vector_get(this_code->methods, imethod, (void**)&check_method );
    if ( ret != 0 ) {
      mdi_error("Error in delete_method: vector_get failed");
      return ret;
    }
    if ( check_method->method_id == method_id ) {
      method_found = 1;
      method_index = imethod;

      // stop searching
      imethod = (int)this_code->methods->size;
    }
  }
  if ( method_found != 1 ) {
    mdi_error("Method not found during delete");
    return 1;
  }

  // delete the data for this method from the global vector of methods
  vector_delete(this_code->methods, method_index);

  return 0;
}



/*! \brief Free the memory associated with a methods vector
 */
int free_methods_vector(vector* v) {
  /*
  int imethod = 0;
  size_t nmethods = v->size;
  for ( imethod = 0; imethod < nmethods; imethod++ ) {
    method* this_method = vector_get(v, imethod);

    // free the "commands" and "callbacks" vectors for this node
    vector_free(this_node->commands);
    vector_free(this_node->callbacks);
  }
  */

  // free this node vector
  vector_free(v);

  return 0;
}



/*! \brief Create a new communicator structure and add it to the list of communicators
 * Returns the handle of the new communicator
 */
int new_communicator(size_t code_id, int method, MDI_Comm_Type* comm_id_ptr) {
  int ret;

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  communicator new_comm;
  new_comm.method_id = method;
  new_comm.is_accepted = 0;
  vector* node_vec = malloc(sizeof(vector));
  vector_init(node_vec, sizeof(node));
  new_comm.nodes = node_vec;
  new_comm.id = this_code->next_comm;
  new_comm.code_id = code_id;

  // For version 1.4.0 of the MDI Library and above, will receive this from the other code
  new_comm.name_length = 12;
  new_comm.command_length = 12;

  new_comm.mdi_version[0] = 0;
  new_comm.mdi_version[1] = 0;
  new_comm.mdi_version[2] = 0;
  this_code->next_comm++;

  new_comm.delete = communicator_delete;

  vector_push_back( this_code->comms, &new_comm );

  *comm_id_ptr = new_comm.id;
  return 0;
}


/*! \brief Get a communicator from a communicator handle
 * Returns a pointer to the communicator
 */
int get_communicator(size_t code_id, MDI_Comm_Type comm_id, communicator** comm_ptr) {
  int ret;

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  // Search through all of the communicators for the one that matches comm_id
  int icomm;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm;
    ret = vector_get( this_code->comms, icomm, (void**)&comm );
    if ( ret != 0 ) {
      mdi_error("Error in get_communicator: vector_get failed");
      return ret;
    }
    if ( comm->id == comm_id ) {
      *comm_ptr = comm;
      return 0;
    }
  }
  mdi_error("Communicator not found");
  return 1;
}


/*! \brief Delete a communicator
 *
 * The function returns \p 0 on a success.
 */
int delete_communicator(size_t code_id, MDI_Comm_Type comm_id) {
  int ret;

  // get the code
  code* this_code;
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code failed");
    return ret;
  }

  communicator* this_comm;
  ret = get_communicator(code_id, comm_id, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_communicator failed");
    return ret;
  }

  // Search through all of the communicators for the one that matches comm_id
  size_t icomm;
  size_t comm_index;
  int comm_found = 0;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm;
    ret = vector_get( this_code->comms, (int)icomm, (void**)&comm );
    if ( ret != 0 ) {
      mdi_error("Error in delete_communicator: vector_get failed");
      return ret;
    }
    if ( comm->id == comm_id ) {
      comm_found = 1;
      comm_index = icomm;

      // stop searching
      icomm = this_code->comms->size;
    }
  }
  if ( comm_found != 1 ) {
    mdi_error("Communicator not found during delete"); 
    return 1;
  }

  // do any method-specific deletion operations
  this_comm->delete(this_comm);

  // in case this communicator's delete function modified the code / comms vectors, update the pointers
  ret = get_code(code_id, &this_code);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: get_code after delete failed");
    return ret;
  }
  ret = get_communicator(code_id, comm_id, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in delete_code: second get_communicator failed");
    return ret;
  }

  // delete the node vector
  free_node_vector(this_comm->nodes);

  // delete the data for this communicator from the code's vector of communicators
  vector_delete(this_code->comms, (int)comm_index);

  return 0;
}


/*! \brief Dummy function for method-specific deletion operations for communicator deletion
 */
int communicator_delete(void* comm) {
  return 0;
}


/*! \brief Print error message and exit
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
int file_exists(const char* file_name, int* flag) {
#ifdef _WIN32
  DWORD dwAttrib = GetFileAttributes(file_name);
  if ( dwAttrib != INVALID_FILE_ATTRIBUTES && !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY) ) {
    *flag = 1;
  }
  else {
    *flag = 0;
  }
#else
  if ( access( file_name, F_OK ) == 0 ) {
    *flag = 1;
  }
  else {
    *flag = 0;
  }
#endif
  return 0;
}


/*! \brief Print error message
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
void mdi_error(const char* message) {
  fprintf( stderr, "%s\n", message );
  fflush(stdout);
}


/*! \brief Print warning message
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
void mdi_warning(const char* message) {
  fprintf( stderr, "MDI warning: %s\n", message );
}


/*! \brief Print a debug message
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
int mdi_debug(const char *message, ...)
{

#if _MDI_DEBUG == 1

  va_list args;
  va_start(args, message);
  vprintf(message, args);
  va_end(args);
  fflush(stdout);

#endif

  return 0;

}


/*! \brief Get information about an MDI datatype
 *
 * \param [in]       datatype
 *                   MDI datatype.
 * \param [out]      size
 *                   Size of the MDI datatype.
 * \param [out]      base
 *                   Base type of an MDI datatype.
 */
int datatype_info(MDI_Datatype_Type datatype, size_t* size, MDI_Datatype_Type* base) {
  if ( datatype == MDI_INT_ ) {
    *size = sizeof(int);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_INT8_T_ ) {
    *size = sizeof(int8_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_INT16_T_ ) {
    *size = sizeof(int16_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_INT32_T_ ) {
    *size = sizeof(int32_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_INT64_T_ ) {
    *size = sizeof(int64_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_UINT8_T_ ) {
    *size = sizeof(uint8_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_UINT16_T_ ) {
    *size = sizeof(uint16_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_UINT32_T_ ) {
    *size = sizeof(uint32_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_UINT64_T_ ) {
    *size = sizeof(uint64_t);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_DOUBLE_ ) {
    *size = sizeof(double);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_CHAR_ ) {
    *size = sizeof(char);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_FLOAT_ ) {
    *size = sizeof(float);
    *base = MDI_INT_;
  }
  else if ( datatype == MDI_BYTE_ ) {
    *size = sizeof(char);
    *base = MDI_INT_;
  }
  else {
    mdi_error("Unrecognized datatype in datatype_info.");
    return 1;
  }
  return 0;
}


/*! \brief Get the MPI datatype that corresponds to an MDI datatype
 *
 * \param [in]       datatype
 *                   MDI datatype.
 * \param [out]      mpitype
 *                   MPI datatype.
 */
int datatype_mpitype(MDI_Datatype_Type datatype, MPI_Datatype* mpitype) {
  if ( datatype == MDI_INT_ ) {
    *mpitype = MPI_INT;
  }
  else if ( datatype == MDI_INT8_T_ ) {
    *mpitype = MPI_INT8_T;
  }
  else if ( datatype == MDI_INT16_T_ ) {
    *mpitype = MPI_INT16_T;
  }
  else if ( datatype == MDI_INT32_T_ ) {
    *mpitype = MPI_INT32_T;
  }
  else if ( datatype == MDI_INT64_T_ ) {
    *mpitype = MPI_INT64_T;
  }
  else if ( datatype == MDI_UINT8_T_ ) {
    *mpitype = MPI_UINT8_T;
  }
  else if ( datatype == MDI_UINT16_T_ ) {
    *mpitype = MPI_UINT16_T;
  }
  else if ( datatype == MDI_UINT32_T_ ) {
    *mpitype = MPI_UINT32_T;
  }
  else if ( datatype == MDI_UINT64_T_ ) {
    *mpitype = MPI_UINT64_T;
  }
  else if ( datatype == MDI_DOUBLE_ ) {
    *mpitype = MPI_DOUBLE;
  }
  else if ( datatype == MDI_CHAR_ ) {
    *mpitype = MPI_CHAR;
  }
  else if ( datatype == MDI_FLOAT_ ) {
    *mpitype = MPI_FLOAT;
  }
  else if ( datatype == MDI_BYTE_ ) {
    *mpitype = MPI_BYTE;
  }
  else {
    mdi_error("Unrecognized datatype in datatype_mpitype.");
    return 1;
  }
  return 0;
}


/*! \brief Convert a buffer from one datatype to another */
int convert_buf_datatype(void* recvbuf_in, MDI_Datatype_Type recvtype,
			 void* sendbuf_in, MDI_Datatype_Type sendtype,
			 int count) {
  int ii;

  if ( sendtype == MDI_INT_ ) {
    int* sendbuf = (int*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }
    
  }
  else if ( sendtype == MDI_INT8_T_ ) {
    int8_t* sendbuf = (int8_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_INT16_T_ ) {
    int16_t* sendbuf = (int16_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_INT32_T_ ) {
    int32_t* sendbuf = (int32_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_INT64_T_ ) {
    int64_t* sendbuf = (int64_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_UINT8_T_ ) {
    uint8_t* sendbuf = (uint8_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_UINT64_T_ ) {
    uint16_t* sendbuf = (uint16_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_UINT64_T_ ) {
    uint32_t* sendbuf = (uint32_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_UINT64_T_ ) {
    uint64_t* sendbuf = (uint64_t*) sendbuf_in;

    if ( recvtype == MDI_INT_ ) {
      int* recvbuf = (int*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT8_T_ ) {
      int8_t* recvbuf = (int8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT16_T_ ) {
      int16_t* recvbuf = (int16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT32_T_ ) {
      int32_t* recvbuf = (int32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_INT64_T_ ) {
      int64_t* recvbuf = (int64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (int64_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT8_T_ ) {
      uint8_t* recvbuf = (uint8_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint8_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT16_T_ ) {
      uint16_t* recvbuf = (uint16_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint16_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT32_T_ ) {
      uint32_t* recvbuf = (uint32_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint32_t) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_UINT64_T_ ) {
      uint64_t* recvbuf = (uint64_t*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (uint64_t) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_DOUBLE_ ) {
    double* sendbuf = (double*) sendbuf_in;

    if ( recvtype == MDI_DOUBLE_ ) {
      double* recvbuf = (double*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (double) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_FLOAT_ ) {
      float* recvbuf = (float*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (float) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else if ( sendtype == MDI_FLOAT_ ) {
    float* sendbuf = (float*) sendbuf_in;

    if ( recvtype == MDI_DOUBLE_ ) {
      double* recvbuf = (double*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (double) sendbuf[ii];
      }
    }
    else if ( recvtype == MDI_FLOAT_ ) {
      float* recvbuf = (float*) recvbuf_in;
      for (ii=0; ii < count; ii++) {
        recvbuf[ii] = (float) sendbuf[ii];
      }
    }
    else {
      mdi_error("Unrecognized datatype in convert_buf_datatype.");
      return 1;
    }

  }
  else {
    mdi_error("Unrecognized datatype in convert_buf_datatype.");
    return 1;
  }

  return 0;
}
