/*! \file
 *
 * \brief Global structures used by MDI
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
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
    int new_capacity;
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
  memcpy( v->data + (index * v->stride), v->data + ( (v->size - 1) * v->stride ), v->stride );
  v->size--;

  // shrink the vector
  if (v->size <= v->capacity / 2) {
    int new_capacity = v->capacity / 2;
    void* new_data = malloc( v->stride * new_capacity );
    memcpy(new_data, v->data, v->size * v->stride);
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
  int nnodes = v->size;
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

  // initialize the node vector
  vector* node_vec = malloc(sizeof(vector));
  vector_init(node_vec, sizeof(node));
  new_code.nodes = node_vec;

  // initialize the comms vector
  vector* comms_vec = malloc(sizeof(vector));
  vector_init(comms_vec, sizeof(communicator));
  new_code.comms = comms_vec;

  new_code.is_python = 0;
  new_code.is_library = 0;
  new_code.id = codes.size;
  new_code.intra_rank = 0;

  // Set the MPI callbacks
  //new_code.mdi_mpi_recv = MPI_Recv;
  //int (*mpi4py_recv_callback)(void*, int, int, MDI_Comm_Type);

  // add the new code to the global vector of codes
  vector_push_back( &codes, &new_code );

  // return the index of the new code
  return codes.size - 1;
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
      icode = codes.size;
    }
  }
  if ( code_found != 1 ) {
    mdi_error("Code not found during delete");
    return 1;
  }

  // delete the node vector
  free_node_vector(this_code->nodes);

  // delete the comms vector
  int icomm;
  int ncomms = this_code->comms->size;
  for (icomm = 0; icomm < ncomms; icomm++) {
    communicator* this_comm = vector_get( this_code->comms, this_code->comms->size - 1 );
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
  int icomm;
  int comm_index;
  int comm_found = 0;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm = vector_get(this_code->comms, icomm);
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

  // delete the node vector
  free_node_vector(this_comm->nodes);

  // delete the data for this communicator from the code's vector of communicators
  vector_delete(this_code->comms, comm_index);

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
void mdi_error(const char* message) {
  if ( errno == 0 || errno == ENOTTY ) {
    fprintf( stderr, message );
    fprintf( stderr, "\n" );
  }
  else {
    perror(message);
  }
  //exit(1);
}
