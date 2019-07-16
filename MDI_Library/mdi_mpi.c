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
int intra_rank = 0;

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
  //int world_rank;

  // get the number of processes
  MPI_Comm_size(world_comm, &world_size);

  // get the rank of this process
  MPI_Comm_rank(world_comm, &world_rank);

  //create the name of this process
  char buffer[MDI_NAME_LENGTH];
  strcpy(buffer, code_name);

  char* names = NULL;
  names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

  char* unique_names = NULL;
  unique_names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

  MPI_Allgather(buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
		MPI_CHAR, world_comm);

  // determine which rank corresponds to rank 0 of the driver
  driver_rank = -1;
  for (i=0; i<world_size; i++) {
    if ( driver_rank == -1 ) {
      char name[MDI_NAME_LENGTH];
      memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(name, "") == 0 ) {
	driver_rank = i;
      }
    }
  }
  if ( driver_rank == -1 ) {
    mdi_error("Unable to identify driver when attempting to connect via MPI");
  }

  //create communicators
  for (i=0; i<world_size; i++) {
    char name[MDI_NAME_LENGTH];
    memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

    int found = 0;
    for (j=0; j<i; j++) {
      char prev_name[MDI_NAME_LENGTH];
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
      char my_name[MDI_NAME_LENGTH];
      memcpy( my_name, &names[world_rank*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
      if ( strcmp(my_name, name) == 0 ) {
	mpi_code_rank = nunique_names;
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
      MPI_Comm_split(world_comm, color, key, &new_mpi_comm);

      // create an MDI communicator for communication between the driver and engine
      if ( world_rank == driver_rank || world_rank == i ) {
	communicator new_comm;
	new_comm.method = MDI_MPI;
	new_comm.mpi_comm = new_mpi_comm;
	new_comm.mpi_rank = key;
	vector_push_back( &communicators, &new_comm );

	// communicate the version number between codes
	// only do this if not using i-PI compatibility mode
	if ( ipi_compatibility != 1 ) {
	  communicator* comm = vector_get(&communicators, communicators.size-1);
	  mpi_send(&MDI_VERSION, 1, MDI_DOUBLE, communicators.size);
	  mpi_recv(&comm->mdi_version, 1, MDI_DOUBLE, communicators.size);
	}
      }
    }

  }

  if ( do_split == 1 ) {

    // create the intra-code communicators
    MPI_Comm_split(world_comm, mpi_code_rank, world_rank, &intra_MPI_comm);
    MPI_Comm_rank(intra_MPI_comm, &intra_rank);

    MPI_Barrier(world_comm);

  }

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
  communicator* this = vector_get(&communicators, comm-1);

  if (datatype == MDI_INT) {
    MPI_Send((void*)buf, count, MPI_INT, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_DOUBLE) {
    MPI_Send((void*)buf, count, MPI_DOUBLE, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_CHAR) {
    MPI_Send((void*)buf, count, MPI_CHAR, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else {
    mdi_error("MDI data type not recognized in mpi_send");
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
  communicator* this = vector_get(&communicators, comm-1);

  if (datatype == MDI_INT) {
    MPI_Recv(buf, count, MPI_INT, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else if (datatype == MDI_DOUBLE) {
    MPI_Recv(buf, count, MPI_DOUBLE, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else if (datatype == MDI_CHAR) {
    MPI_Recv(buf, count, MPI_CHAR, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else {
    mdi_error("MDI data type not recognized in mpi_recv");
  }

  return 0;
}
