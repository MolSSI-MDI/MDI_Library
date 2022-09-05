/*! \file
 *
 * \brief MPI communication implementation
 */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "mdi.h"
#include "mdi_mpi.h"
#include "mdi_global.h"


/*! \brief Set the size of MPI_COMM_WORLD
 *
 * \param [in]       world_size_in
 *                   Size of MPI_COMM_WORLD
 */
int set_world_size(int world_size_in) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in set_world_size: get_current_code failed");
    return 1;
  }

  this_code->world_size = world_size_in;
  return 0;
}


/*! \brief Set the rank of this process in MPI_COMM_WORLD
 *
 * \param [in]       world_rank_in
 *                   Rank of this process within MPI_COMM_WORLD
 */
int set_world_rank(int world_rank_in) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in set_world_rank: get_current_code failed");
    return 1;
  }

  this_code->world_rank = world_rank_in;
  this_code->intra_rank = world_rank_in;
  return 0;
}


/*! \brief Enable support for the TCP method */
int enable_mpi_support(int code_id) {
  int ret;
  int method_id;
  ret = new_method(code_id, MDI_MPI, &method_id);
  if ( ret != 0 ) {
    mdi_error("Error in enable_mpi_support: new_method failed");
    return ret;
  }
  method* this_method;
  ret = get_method(code_id, MDI_MPI, &this_method);
  if ( ret != 0 ) {
    mdi_error("Error in enable_mpi_support: get_method failed");
    return ret;
  }
  this_method->on_selection = mpi_on_selection;
  this_method->on_accept_communicator = mpi_on_accept_communicator;
  this_method->on_send_command = mpi_on_send_command;
  this_method->after_send_command = mpi_after_send_command;
  this_method->on_recv_command = mpi_on_recv_command;
  return 0;
}


/*! \brief Callback when the end-user selects MPI as the method */
int mpi_on_selection() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_on_selection: get_current_code failed");
    return 1;
  }

  int mpi_initialized = 0;

  if ( this_code->mpi_initialized == 1 ) {
    mdi_error("MDI_Init called after MPI was already initialized");
    return 1;
  }
  this_code->mpi_initialized = 1;

  // ensure MPI has been initialized
  int mpi_init_flag = 0;

  ret = MPI_Initialized(&mpi_init_flag);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init: MPI_Initialized failed");
    return ret;
  }

  if ( mpi_init_flag == 0 ) {

    // initialize MPI
    int mpi_argc = 0;
    char** mpi_argv;
    ret = MPI_Init( &mpi_argc, &mpi_argv );
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init: MPI_Init failed");
      return ret;
    }

    // confirm that MPI is now initialized
    // if it isn't, that indicates that the MPI stubs are being used
    ret = MPI_Initialized(&mpi_init_flag);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init: MPI_Initialized failed");
      return ret;
    }
    if ( mpi_init_flag == 0 ) {
      mdi_error("Error in MDI_Init: Failed to initialize MPI. Check that the MDI Library is linked to an MPI library.");
      return 1;
    }

    this_code->initialized_mpi = 1;
  }

  // get the appropriate MPI communicator to use
  MPI_Comm mpi_communicator;
  ret = MPI_Initialized(&mpi_init_flag);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init: MPI_Initialized failed");
    return ret;
  }
  if ( mpi_init_flag == 0 ) {
    mpi_communicator = 0;
  }
  else {
    if ( this_code->language == MDI_LANGUAGE_PYTHON && ( ! this_code->initialized_mpi ) ) {
      mpi_communicator = 0;
    }
    else {
      mpi_communicator = MPI_COMM_WORLD;
      MPI_Comm_rank(mpi_communicator, &this_code->world_rank);
      MPI_Comm_size(mpi_communicator, &this_code->world_size);
      this_code->intra_rank = this_code->world_rank;
    }
  }

  // determine whether the intra-code MPI communicator should be split
  int use_mpi4py = 0;
  if ( this_code->language == MDI_LANGUAGE_PYTHON && ( ! this_code->initialized_mpi ) ) {
    use_mpi4py = 1;
  }

  // split intra-communicators for each code
  if ( strcmp(this_code->role, "DRIVER") == 0 ) {
    mpi_identify_codes("", use_mpi4py, mpi_communicator);
    mpi_initialized = 1;
  }
  else if ( strcmp(this_code->role,"ENGINE") == 0 ) {
    mpi_identify_codes(this_code->name, use_mpi4py, mpi_communicator);
    mpi_initialized = 1;
  }
  else {
    mdi_error("Error in MDI_Init: Role not recognized");
    return 1;
  }

  return 0;
}



/*! \brief Callback when the MPI method must accept a communicator */
int mpi_on_accept_communicator() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_on_accept_communicator: get_current_code failed");
    return 1;
  }

  // If MDI hasn't returned some connections, do that now
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    communicator* comm_obj;
    ret = get_communicator(codes.current_key, this_code->returned_comms, &comm_obj);
    if ( ret != 0 ) {
      mdi_error("Error in mpi_on_accept_communicator: get_communicator failed");
      return ret;
    }
    comm_obj->is_accepted = 1;
    return this_code->returned_comms;
  }

  // unable to accept any connections
  return MDI_COMM_NULL;
}



/*! \brief Callback when the MPI method must send a command */
int mpi_on_send_command(const char* command, MDI_Comm comm, int* skip_flag) {
  return 0;
}



/*! \brief Callback after the MPI method has received a command */
int mpi_after_send_command(const char* command, MDI_Comm comm) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_after_send_command: get_current_code failed");
    return 1;
  }

  // if the command was "EXIT", delete this communicator
  if ( strcmp( command, "EXIT" ) == 0 ) {
    delete_communicator(codes.current_key, comm);
  }

  // if MDI called MPI_Init, and there are no more communicators, call MPI_Finalize now
  if ( this_code->initialized_mpi == 1 ) {
    if ( this_code->comms->size == 0 ) {
      MPI_Finalize();
    }
  }
  
  return 0;
}



/*! \brief Callback when the MPI method must receive a command */
int mpi_on_recv_command(MDI_Comm comm) {
  return 0;
}



/*! \brief Identify groups of processes belonging to the same codes
 *
 * If use_mpi4py == 0, this function will call MPI_Comm_split to create an intra-code communicator for each code.
 *
 * \param [in]       code_name
 *                   MDI name of the code associated with this process, indicated by the user with the -name 
 *                   runtime option.
 * \param [in]       use_mpi4py
 *                   Flag to indicate whether MPI_Comm_split should be called in order to create an intra-code
 *                   communicator for each code.
 *                   The intra-code communicators are created only if use_mpi4py == 0.
 *                   Should normally be set to 0, unless the code associated with this process is a Python code.
 *                   In that case, the Python wrapper code will do the split instead.
 */
int mpi_identify_codes(const char* code_name, int use_mpi4py, MPI_Comm world_comm) {
  int i, j, ret;
  int driver_rank;
  int nunique_names = 0;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_identify_codes: get_current_code failed");
    return 1;
  }

  // get the number of processes
  if ( use_mpi4py == 0 ) {
    MPI_Comm_size(world_comm, &this_code->world_size);
  }
  else {
    int comm_flag = 0;
    this_code->world_size = this_code->mpi4py_size_callback(comm_flag);
  }

  // get the rank of this process
  if ( use_mpi4py == 0 ) {
    MPI_Comm_rank(world_comm, &this_code->world_rank);
  }
  else {
    int comm_flag = 0;
    this_code->world_rank = this_code->mpi4py_rank_callback(comm_flag);
  }

  // determine the MDI version of each code
  int* my_version = malloc( sizeof(int) * 5 );
  int* all_versions = malloc( sizeof(int) * 5 * this_code->world_size );
  my_version[0] = MDI_MAJOR_VERSION;
  my_version[1] = MDI_MINOR_VERSION;
  my_version[2] = MDI_PATCH_VERSION;
  my_version[3] = MDI_COMMAND_LENGTH;
  my_version[4] = MDI_NAME_LENGTH;
  if ( use_mpi4py == 0 ) {
    MPI_Allgather(my_version, 5, MPI_INT, all_versions, 5, 
        MPI_INT, world_comm);
  }
  else {
    ret = this_code->mpi4py_allgather_callback(my_version, all_versions);
    if ( ret != 0 ) {
      mdi_error("Error in mpi4py_allgather_callback");
      return ret;
    }
  }

  // create a list of the name lengths of each rank
  int* name_lengths = malloc( sizeof(int) * this_code->world_size );
  int* name_displs = malloc( sizeof(int) * this_code->world_size );
  name_displs[0] = 0;
  for (i=0; i < this_code->world_size; i++) {
      name_lengths[i] = all_versions[i*5 + 4];
      if ( i > 0 ) {
          name_displs[i] = name_displs[i-1] + name_lengths[i-1];
      }
  }
  int name_array_length = name_displs[this_code->world_size-1] + name_lengths[this_code->world_size-1];

  // create the name of this process
  char* buffer = malloc( sizeof(char) * MDI_NAME_LENGTH );
  int ichar;
  for (ichar=0; ichar < MDI_NAME_LENGTH; ichar++) {
    buffer[ichar] = '\0';
  }
  for (ichar=0; ichar < MDI_NAME_LENGTH && ichar < strlen(code_name); ichar++) {
    buffer[ichar] = code_name[ichar];
  }

  char* names = malloc(sizeof(char) * name_array_length);
  for (ichar=0; ichar < name_array_length; ichar++) {
    names[ichar] = '\0';
  }

  char* unique_names = NULL;
  unique_names = (char*)malloc(sizeof(char) * this_code->world_size*MDI_NAME_LENGTH);
  for (ichar=0; ichar < this_code->world_size*MDI_NAME_LENGTH; ichar++) {
    unique_names[ichar] = '\0';
  }

  // gather the name of the code associated with each rank
  if ( use_mpi4py == 0 ) {
    //MPI_Allgather(buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
	//	  MPI_CHAR, world_comm);
    MPI_Allgatherv(buffer, MDI_NAME_LENGTH, MPI_CHAR, names,
        name_lengths, name_displs,
        MPI_CHAR, world_comm);
  }
  else {
    ret = this_code->mpi4py_gather_names_callback(buffer, names, name_lengths, name_displs);
    if ( ret != 0 ) {
      mdi_error("Error in mpi4py_gather_names_callback");
      return ret;
    }
  }

  // determine which rank corresponds to rank 0 of the driver
  char* name = malloc( sizeof(char) * MDI_NAME_LENGTH );
  driver_rank = -1;
  for (i=0; i<this_code->world_size; i++) {
    if ( driver_rank == -1 ) {
      memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(name, "") == 0 ) {
	driver_rank = i;
      }
    }
  }
  if ( driver_rank == -1 ) {
    mdi_error("Unable to identify driver when attempting to connect via MPI"); 
    return 1;
  }

  //create communicators
  char* prev_name = malloc( sizeof(char) * MDI_NAME_LENGTH );
  char* my_name = malloc( sizeof(char) * MDI_NAME_LENGTH );
  int mpi_code_rank = 0;
  for (i=0; i<this_code->world_size; i++) {
    memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

    int found = 0;
    for (j=0; j<i; j++) {
      memcpy( prev_name, &names[j*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(name, prev_name) == 0 ) {
	found = 1;
      }
    }

    // check if this rank is the first instance of a new production code
    if ( found == 0 && strcmp(name,"") != 0 ) {
      // add this code's name to the list of unique names
      memcpy( &unique_names[nunique_names*MDI_NAME_LENGTH], name, MDI_NAME_LENGTH );
      nunique_names++;
      memcpy( my_name, &names[this_code->world_rank*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(my_name, name) == 0 ) {
        mpi_code_rank = nunique_names;
      }

      // if this rank is a member of either the driver or the engine, create a new communicator
      MDI_Comm comm_id = MDI_COMM_NULL;
      if ( strcmp(my_name, "") == 0 || strcmp(my_name, name) == 0 ) {
        ret = new_communicator(this_code->id, MDI_MPI, &comm_id);
        if ( ret != 0 ) {
          mdi_error("Error in mpi_identify_codes: new_communicator failed");
          return 1;
        }
        communicator* new_comm;
        ret = get_communicator(this_code->id, comm_id, &new_comm);
        if ( ret != 0 ) {
          mdi_error("Error in mpi_identify_codes: get_communicator failed");
          return ret;
        }

        // set the communicator's version numbers

        new_comm->mdi_version[0] = all_versions[0]; // MDI_MAJOR_VERSION
        new_comm->mdi_version[1] = all_versions[1]; // MDI_MINOR_VERSION
        new_comm->mdi_version[2] = all_versions[2]; // MDI_PATCH_VERSION
        new_comm->command_length = all_versions[3]; // MDI_COMMAND_LENGTH
        new_comm->name_length = all_versions[4]; // MDI_NAME_LENGTH
      }

      // create an MPI communicator for inter-code communication
      MPI_Comm new_mpi_comm;
      int color = 0;
      int key = 0;
      if ( this_code->world_rank == driver_rank ) {
        color = 1;
      }
      else if ( this_code->world_rank == i ) {
        color = 1;
        key = 1;
      }
      if ( use_mpi4py == 0 ) {
        MPI_Comm_split(world_comm, color, key, &new_mpi_comm);
      }
      else {
        int comm_flag = 0; // indicates that this is for creating the inter-code communicator
        this_code->mpi4py_split_callback(color, key, comm_id, comm_flag);
      }

      // create an MDI communicator for communication between the driver and engine
      // only done if this is a rank on either the driver or the engine
      if ( strcmp(my_name, "") == 0 || strcmp(my_name, name) == 0 ) {
        communicator* new_comm;
        ret = get_communicator(this_code->id, comm_id, &new_comm);
        if ( ret != 0 ) {
          mdi_error("Error in mpi_identify_codes: second get_communicator failed");
          return ret;
        }
        new_comm->delete = communicator_delete_mpi;
        new_comm->send = mpi_send;
        new_comm->recv = mpi_recv;

        // allocate the method data
        mpi_method_data* method_data = malloc(sizeof(mpi_method_data));
        method_data->mpi_comm = new_mpi_comm;
        method_data->mpi_rank = key;
        method_data->use_mpi4py = use_mpi4py;
        new_comm->method_data = method_data;
      }
    }

  }

  // create the intra-code communicators
  if ( use_mpi4py == 0 ) {
    MPI_Comm_split(world_comm, mpi_code_rank, this_code->world_rank, &this_code->intra_MPI_comm);
  }
  else {
    this_code->mpi4py_split_callback(mpi_code_rank, this_code->world_rank, 0, 1);
  }

  // get the intra-code rank
  if ( use_mpi4py == 0 ) {
    MPI_Comm_rank(this_code->intra_MPI_comm, &this_code->intra_rank);
  }
  else {
    this_code->intra_rank = this_code->mpi4py_rank_callback(1);
  }

  // Barrier
  if ( use_mpi4py == 0 ) {
    MPI_Barrier(world_comm);
  }
  else {
    this_code->mpi4py_barrier_callback(0);
  }

  free( buffer );
  free( names );
  free( unique_names );
  free( name );
  free( prev_name );
  free( my_name );
  free( my_version );
  free( all_versions );
  free( name_lengths );
  free( name_displs );

  return 0;
}


/*! \brief Update a pointer to MPI_COMM_WORLD to instead point to the intra-code MPI communicator
 *
 * \param [in, out]  world_comm
 *                   On input, the MPI communicator that spans all of the codes.
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 */
int mpi_update_world_comm(void* world_comm) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_update_world_comm: get_current_code failed");
    return 1;
  }

  MPI_Comm* world_comm_ptr = (MPI_Comm*) world_comm;
  *world_comm_ptr = this_code->intra_MPI_comm;
  return 0;
}


/*! \brief Send data through an MDI connection, using MPI
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
int mpi_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_send: get_current_code failed");
    return 1;
  }

  // only send from rank 0
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_send: get_communicator failed");
    return ret;
  }
  mpi_method_data* method_data = (mpi_method_data*) this->method_data;

  // determine the byte size of the data type being sent
  MPI_Datatype mpi_type;
  ret = datatype_mpitype(datatype, &mpi_type);
  if ( ret != 0 ) { return ret; }

  // send the data
  if ( method_data->use_mpi4py == 0 ) {
    MPI_Send((void*)buf, count, mpi_type, (method_data->mpi_rank+1)%2, 0, method_data->mpi_comm);
  }
  else {
    this_code->mpi4py_send_callback( (void*)buf, count, datatype, (method_data->mpi_rank+1)%2, this->id );
  }

  return 0;
}


/*! \brief Receive data through an MDI connection, using MPI
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
int mpi_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_recv: get_current_code failed");
    return 1;
  }

  // only recv from rank 0
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in mpi_recv: get_communicator failed");
    return ret;
  }
  mpi_method_data* method_data = (mpi_method_data*) this->method_data;

  // determine the byte size of the data type being sent
  MPI_Datatype mpi_type;
  ret = datatype_mpitype(datatype, &mpi_type);
  if ( ret != 0 ) { return ret; }

  // receive the data
  if ( method_data->use_mpi4py == 0 ) {
    MPI_Recv(buf, count, mpi_type, (method_data->mpi_rank+1)%2, 0, method_data->mpi_comm, MPI_STATUS_IGNORE);
  }
  else {
    this_code->mpi4py_recv_callback( buf, count, datatype, (method_data->mpi_rank+1)%2, this->id );
  }

  return 0;
}


/*! \brief Function for MPI-specific deletion operations for communicator deletion
 */
int communicator_delete_mpi(void* comm) {
  communicator* this_comm = (communicator*) comm;
  mpi_method_data* method_data = (mpi_method_data*) this_comm->method_data;

  // delete the method-specific information
  free( method_data );

  return 0;
}
