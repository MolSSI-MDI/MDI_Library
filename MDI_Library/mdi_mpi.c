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

/*! \brief MPI communicator corresponding to all processes created by the same code as this process */
MPI_Comm intra_MPI_comm = 0;

/*! \brief Rank of this process within its associated code */
//int intra_rank = 0;

/*! \brief Order of this code within all codes represented by MPI_COMM_WORLD */
int mpi_code_rank = 0;

/*! \brief Size of MPI_COMM_WORLD */
int world_size = -1;

/*! \brief Rank of this process within MPI_COMM_WORLD */
int world_rank = -1;

/*! \brief Set the size of MPI_COMM_WORLD
 *
 * \param [in]       world_size_in
 *                   Size of MPI_COMM_WORLD
 */
int set_world_size(int world_size_in) {
  world_size = world_size_in;
  return 0;
}

/*! \brief Set the rank of this process in MPI_COMM_WORLD
 *
 * \param [in]       world_rank_in
 *                   Rank of this process within MPI_COMM_WORLD
 */
int set_world_rank(int world_rank_in) {
  world_rank = world_rank_in;
  return 0;
}

/*! \brief Identify groups of processes belonging to the same codes
 *
 * If do_split == 1, this function will call MPI_Comm_split to create an intra-code communicator for each code.
 *
 * \param [in]       code_name
 *                   MDI name of the code associated with this process, indicated by the user with the -name 
 *                   runtime option.
 * \param [in]       do_split
 *                   Flag to indicate whether MPI_Comm_split should be called in order to create an intra-code
 *                   communicator for each code.
 *                   The intra-code communicators are created only if do_split == 1.
 *                   Should normally be set to 1, unless the code associated with this process is a Python code.
 *                   In that case, the Python wrapper code will do the split instead.
 */
int mpi_identify_codes(const char* code_name, int do_split, MPI_Comm world_comm) {
  int i, j;
  int driver_rank;
  int nunique_names = 0;
  code* this_code = get_code(current_code);

  // get the number of processes
  if ( do_split == 1 ) {
    MPI_Comm_size(world_comm, &world_size);
  }
  else {
    int comm_flag = 0;
    world_size = mpi4py_size_callback(comm_flag);
  }

  // get the rank of this process
  if ( do_split == 1 ) {
    MPI_Comm_rank(world_comm, &world_rank);
  }
  else {
    int comm_flag = 0;
    world_rank = mpi4py_rank_callback(comm_flag);
  }

  //create the name of this process
  char* buffer = malloc( sizeof(char) * MDI_NAME_LENGTH );
  strcpy(buffer, code_name);

  char* names = NULL;
  names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

  char* unique_names = NULL;
  unique_names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

  // gather the name of the code associated with each rank
  if ( do_split == 1 ) {
    MPI_Allgather(buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
		  MPI_CHAR, world_comm);
  }
  else {
    mpi4py_gather_names_callback(buffer, names);
  }

  // determine which rank corresponds to rank 0 of the driver
  char* name = malloc( sizeof(char) * MDI_NAME_LENGTH );
  driver_rank = -1;
  for (i=0; i<world_size; i++) {
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
  for (i=0; i<world_size; i++) {
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
      memcpy( my_name, &names[world_rank*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(my_name, name) == 0 ) {
	mpi_code_rank = nunique_names;
      }

      // if this is rank 0 on either the driver or the engine, create a new communicator
      MDI_Comm comm_id = MDI_NULL_COMM;
      if ( world_rank == driver_rank || world_rank == i ) {
	comm_id = new_communicator(this_code->id, MDI_MPI);
      }

      // create an MPI communicator for inter-code communication
      MPI_Comm new_mpi_comm;
      int color = 0;
      int key = 0;
      if ( world_rank == driver_rank ) {
	color = 1;
      }
      else if ( world_rank == i ) {
	color = 1;
	key = 1;
      }
      if ( do_split == 1 ) {
	MPI_Comm_split(world_comm, color, key, &new_mpi_comm);
      }
      else {
	int comm_flag = 0; // indicates that this is for creating the inter-code communicator
	mpi4py_split_callback(color, key, comm_id, comm_flag);
      }

      // create an MDI communicator for communication between the driver and engine
      // only done if this is rank 0 on either the driver or the engine
      if ( world_rank == driver_rank || world_rank == i ) {
	communicator* new_comm = get_communicator(this_code->id, comm_id);
	new_comm->mpi_comm = new_mpi_comm;
	new_comm->mpi_rank = key;
      }
    }

  }

  // create the intra-code communicators
  if ( do_split == 1 ) {
    MPI_Comm_split(world_comm, mpi_code_rank, world_rank, &intra_MPI_comm);
  }
  else {
    mpi4py_split_callback(mpi_code_rank, world_rank, 0, 1);
  }

  // get the intra-code rank
  if ( do_split == 1 ) {
    MPI_Comm_rank(intra_MPI_comm, &this_code->intra_rank);
  }
  else {
    this_code->intra_rank = mpi4py_rank_callback(1);
  }

  // Barrier
  if ( do_split == 1 ) {
    MPI_Barrier(world_comm);
  }
  else {
    mpi4py_barrier_callback(0);
  }

  // communicate the version number between codes
  int icomm;
  for ( icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* this_comm = vector_get(this_code->comms, icomm);
    if (this_comm->method == MDI_MPI) {
      // only communicate the version number if not using i-PI compatibility mode
      if ( ipi_compatibility != 1 ) {
	int version[3];
	version[0] = MDI_MAJOR_VERSION;
	version[1] = MDI_MINOR_VERSION;
	version[2] = MDI_PATCH_VERSION;
	mpi_send(&version[0], 3, MDI_INT, this_comm->id);
	mpi_recv(&this_comm->mdi_version[0], 3, MDI_INT, this_comm->id);
      }
    }
  }

  free( buffer );
  free( names );
  free( unique_names );
  free( name );
  free( prev_name );
  free( my_name );

  return 0;
}


/*! \brief Update a pointer to MPI_COMM_WORLD to instead point to the intra-code MPI communicator
 *
 * \param [in, out]  world_comm
 *                   On input, the MPI communicator that spans all of the codes.
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 */
int mpi_update_world_comm(void* world_comm) {
  MPI_Comm* world_comm_ptr = (MPI_Comm*) world_comm;
  *world_comm_ptr = intra_MPI_comm;
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
 */
int mpi_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  // only send from rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  communicator* this = get_communicator(current_code, comm);

  // determine the datatype of the send buffer
  MPI_Datatype mpi_type;
  if (datatype == MDI_INT) {
    mpi_type = MPI_INT;
  }
  else if (datatype == MDI_DOUBLE) {
    mpi_type = MPI_DOUBLE;
  }
  else if (datatype == MDI_CHAR) {
    mpi_type = MPI_CHAR;
  }
  else if ( datatype == MDI_INT_NUMPY ) {
    if ( this_code->is_python == 0 ) {
      mdi_error("MDI_INT_NUMPY datatype only permitted if using Python");
      return 1;
    }
    mpi_type = MPI_INT;
  }
  else if ( datatype == MDI_DOUBLE_NUMPY ) {
    if ( this_code->is_python == 0 ) {
      mdi_error("MDI_DOUBLE_NUMPY datatype only permitted if using Python");
      return 1;
    }
    mpi_type = MPI_DOUBLE;
  }
  else {
    mdi_error("MDI data type not recognized in mpi_send");
    return 1;
  }

  // send the data
  if ( this_code->is_python == 0 ) {
    MPI_Send((void*)buf, count, mpi_type, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else {
    mpi4py_send_callback( (void*)buf, count, datatype, (this->mpi_rank+1)%2, this->id );
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
 */
int mpi_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  // only recv from rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  communicator* this = get_communicator(current_code, comm);

  // determine the datatype of the receive buffer
  MPI_Datatype mpi_type;
  if (datatype == MDI_INT) {
    mpi_type = MPI_INT;
  }
  else if (datatype == MDI_DOUBLE) {
    mpi_type = MPI_DOUBLE;
  }
  else if (datatype == MDI_CHAR) {
    mpi_type = MPI_CHAR;
  }
  else if ( datatype == MDI_INT_NUMPY ) {
    if ( this_code->is_python == 0 ) {
      mdi_error("MDI_INT_NUMPY datatype only permitted if using Python");
      return 1;
    }
    mpi_type = MPI_INT;
  }
  else if ( datatype == MDI_DOUBLE_NUMPY ) {
    if ( this_code->is_python == 0 ) {
      mdi_error("MDI_DOUBLE_NUMPY datatype only permitted if using Python");
      return 1;
    }
    mpi_type = MPI_DOUBLE;
  }
  else {
    mdi_error("MDI data type not recognized in mpi_send");
    return 1;
  }

  // receive the data
  if ( this_code->is_python == 0 ) {
    MPI_Recv(buf, count, mpi_type, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else {
    mpi4py_recv_callback( buf, count, datatype, (this->mpi_rank+1)%2, this->id );
  }

  return 0;
}
