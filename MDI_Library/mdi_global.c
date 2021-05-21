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
#include "mdi_global.h"

/*! \brief Vector containing all codes that have been initiailized on this rank
 * Typically, this will only include a single code, unless the communication method is LIBRARY */
vector codes;

/*! \brief Index of the active code */
int current_code = 0;

/*! \brief Flag for whether MDI is running in i-PI compatibility mode */
int ipi_compatibility = 0;

/*! \brief Flag for whether MDI has been previously initialized */
int is_initialized = 0;

/*! \brief Flag for whether MDI called MPI_Init */
int initialized_mpi = 0;

/*! \brief Flag for whether MDI is currently operating in plugin mode */
int plugin_mode = 0;

/*! \brief Internal copy of MPI_COMM_WORLD, used when MDI initializes MPI */
MPI_Comm mdi_mpi_comm_world;

/*! \brief Pointer to the MPI_Comm over which a Python plugin should run.
 * Only used for Python plugins */
void* python_plugin_mpi_world_ptr = NULL;

/*! \brief Unedited command-line options for currently running plugin */
char* plugin_unedited_options = NULL;

/*! \brief Command-line options for currently running plugin */
char* plugin_options = NULL;

/*! \brief Argument count for plugin command-line options */
int plugin_argc = 0;

/*! \brief Argument vector for plugin command-line options */
char** plugin_argv = NULL;

/*! \brief Python callback pointer for MPI_Recv */
int (*mpi4py_recv_callback)(void*, int, int, int, MDI_Comm_Type);

/*! \brief Python callback pointer for MPI_Send */
int (*mpi4py_send_callback)(void*, int, int, int, MDI_Comm_Type);

/*! \brief Python callback pointer for gathering names */
int (*mpi4py_gather_names_callback)(void*, void*);

/*! \brief Python callback pointer for MPI_Comm_split */
int (*mpi4py_split_callback)(int, int, MDI_Comm_Type, int);

/*! \brief Python callback pointer for MPI_Comm_rank */
int (*mpi4py_rank_callback)(int);

/*! \brief Python callback pointer for MPI_Comm_size */
int (*mpi4py_size_callback)(int);

/*! \brief Python callback pointer for MPI_Comm_barrier */
int (*mpi4py_barrier_callback)(int);

/*! \brief Size of MPI_COMM_WORLD */
int world_size = -1;

/*! \brief Rank of this process within MPI_COMM_WORLD */
int world_rank = -1;

/*! \brief Initialize memory allocation for a vector structure
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
    perror("Could not initialize vector");
    exit(-1);
  }

  v->size = 0;
  v->capacity = 0;
  v->stride = stride;

  return 0;
}

/*! \brief Append a new element to the end of the vector
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
 * \param [in]       v
 *                   Pointer to the vector from which the element will be removed
 * \param [in]       index
 *                   Index of the element that will be removed from the vector
 */
int vector_delete(vector* v, int index) {
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
 * \param [in]       v
 *                   Pointer to the vector that will be freed
 */
int vector_free(vector* v) {
  free(v->data);
  free(v);
  return 0;
}

/*! \brief Return a pointer to an element of a vector
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       index
 *                   Index of the element within the vector
 */
void* vector_get(vector* v, int index) {
  if (index < 0 || index >= v->size) {
    mdi_error("Vector accessed out-of-bounds");
    return NULL;
  }
  return ( void* )( v->data + (index * v->stride) );
}

/*! \brief Determine the index of a node within a vector of nodes
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       node_name
 *                   Name of the node
 */
int get_node_index(vector* v, const char* node_name) {
  int inode;
  int node_index = -1;
  for ( inode = 0; inode < v->size; inode++ ) {
    node* this_node = vector_get(v, inode);
    if ( strcmp( node_name, this_node->name ) == 0 ) {
      node_index = inode;
    }
  }
  return node_index;
}

/*! \brief Determine the index of a command within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       command_name
 *                   Name of the command
 */
int get_command_index(node* n, const char* command_name) {
  int icommand;
  int command_index = -1;
  for ( icommand = 0; icommand < n->commands->size; icommand++ ) {
    if ( strcmp( command_name, vector_get( n->commands, icommand ) ) == 0 ) {
      command_index = icommand;
    }
  }
  return command_index;
}

/*! \brief Determine the index of a callback within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       callback_name
 *                   Name of the callback
 */
int get_callback_index(node* n, const char* callback_name) {
  int icallback;
  int callback_index = -1;
  for ( icallback = 0; icallback < n->callbacks->size; icallback++ ) {
    if ( strcmp( callback_name, vector_get( n->callbacks, icallback ) ) == 0 ) {
      callback_index = icallback;
    }
  }
  return callback_index;
}


/*! \brief Determine the index of a callback within a node
 */
int free_node_vector(vector* v) {
  int inode = 0;
  size_t nnodes = v->size;
  for ( inode = 0; inode < nnodes; inode++ ) {
    node* this_node = vector_get(v, inode);

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
int new_code() {
  code new_code;
  new_code.returned_comms = 0;
  new_code.next_comm = 1;
  new_code.intra_MPI_comm = MPI_COMM_WORLD;
  new_code.language = MDI_LANGUAGE_C;

  // initialize the character buffer for the plugin path
  new_code.plugin_path = malloc(PLUGIN_PATH_LENGTH * sizeof(char));
  new_code.plugin_path[0] = '\0';

  // initialize the node vector
  vector* node_vec = malloc(sizeof(vector));
  vector_init(node_vec, sizeof(node));
  new_code.nodes = node_vec;

  // initialize the comms vector
  vector* comms_vec = malloc(sizeof(vector));
  vector_init(comms_vec, sizeof(communicator));
  new_code.comms = comms_vec;

  new_code.is_library = 0;
  new_code.id = (int)codes.size;
  new_code.called_set_execute_command_func = 0;
  new_code.intra_rank = 0;
  if (world_rank != -1) {
    // The Python wrapper has called MDI_Set_World_Rank to set this value
    new_code.intra_rank = world_rank;
  }

  // Set the MPI callbacks
  //new_code.mdi_mpi_recv = MPI_Recv;
  //int (*mpi4py_recv_callback)(void*, int, int, MDI_Comm_Type);

  // add the new code to the global vector of codes
  vector_push_back( &codes, &new_code );

  // return the index of the new code
  return (int)codes.size - 1;
}


/*! \brief Get a code from a code handle
 * Returns a pointer to the code
 */
code* get_code(int code_id) {
  // Search through all of the codes for the one that matches code_id
  int icode;
  for (icode = 0; icode < codes.size; icode++ ) {
    code* this_code = vector_get(&codes, icode);
    if ( this_code->id == code_id ) {
      return this_code;
    }
  }
  mdi_error("Code not found");
  return NULL;
}


/*! \brief Delete a code
 * Returns 0 on success
 */
int delete_code(int code_id) {
  code* this_code = get_code(code_id);

  // Search through all of the codes for the one that matches code_id
  int icode;
  int code_index;
  int code_found = 0;
  for (icode = 0; icode < codes.size; icode++ ) {
    code* code = vector_get(&codes, icode);
    if ( code->id == code_id ) {
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

  // delete the comms vector
  int icomm;
  size_t ncomms = this_code->comms->size;
  for (icomm = 0; icomm < ncomms; icomm++) {
    communicator* this_comm = vector_get( this_code->comms, (int)this_code->comms->size - 1 );
    delete_communicator(code_id, this_comm->id);
  }
  vector_free( this_code->comms );

  // delete the data for this code from the global vector of codes
  vector_delete(&codes, code_index);

  return 0;
}


/*! \brief Create a new communicator structure and add it to the list of communicators
 * Returns the handle of the new communicator
 */
int new_communicator(int code_id, int method) {
  code* this_code = get_code(code_id);

  communicator new_comm;
  new_comm.method = method;
  vector* node_vec = malloc(sizeof(vector));
  vector_init(node_vec, sizeof(node));
  new_comm.nodes = node_vec;
  new_comm.id = this_code->next_comm;
  new_comm.code_id = code_id;
  new_comm.mdi_version[0] = 0;
  new_comm.mdi_version[1] = 0;
  new_comm.mdi_version[2] = 0;
  this_code->next_comm++;

  new_comm.delete = communicator_delete;

  vector_push_back( this_code->comms, &new_comm );

  return new_comm.id;
}


/*! \brief Get a communicator from a communicator handle
 * Returns a pointer to the communicator
 */
communicator* get_communicator(int code_id, MDI_Comm_Type comm_id) {
  code* this_code = get_code(code_id);

  // Search through all of the communicators for the one that matches comm_id
  int icomm;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm = vector_get(this_code->comms, icomm);
    if ( comm->id == comm_id ) {
      return comm;
    }
  }
  mdi_error("Communicator not found");
  return NULL;
}


/*! \brief Delete a communicator
 * Returns 0 on success
 */
int delete_communicator(int code_id, MDI_Comm_Type comm_id) {
  code* this_code = get_code(code_id);
  communicator* this_comm = get_communicator(code_id, comm_id);

  // Search through all of the communicators for the one that matches comm_id
  size_t icomm;
  size_t comm_index;
  int comm_found = 0;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm = vector_get(this_code->comms, (int)icomm);
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
  this_code = get_code(code_id);
  this_comm = get_communicator(code_id, comm_id);

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
int file_exists(const char* file_name) {
#ifdef _WIN32
  DWORD dwAttrib = GetFileAttributes(file_name);
  if ( dwAttrib != INVALID_FILE_ATTRIBUTES && !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY) ) {
    return 1;
  }
  else {
    return 0;
  }
#else
  if ( access( file_name, F_OK ) == 0 ) {
    return 1;
  }
  else {
    return 0;
  }
#endif
}


/*! \brief Print error message
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
void mdi_error(const char* message) {
  fprintf( stderr, "%s\n", message );
}


/*! \brief Print warning message
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
void mdi_warning(const char* message) {
  fprintf( stderr, "MDI warning: %s\n", message );
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
