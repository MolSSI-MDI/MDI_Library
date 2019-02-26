/*! \file
 *
 * \brief Class definition for top-level manager of MDI operations
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "mdi.h"
#include "method.h"
#include "communicator.h"

using namespace std;

MethodMPI::MethodMPI() {
  this->intra_MPI_comm = 0;
  this->intra_rank = 0;
  this->mpi_code_rank = 0;
}


int MethodMPI::gather_names(const char* hostname_ptr, bool do_split) {

   int i, j, icomm;
   int driver_rank;
   int nunique_names = 0;
   int world_rank;

   // get the number of processes
   int world_size;
   MPI_Comm_size(MPI_COMM_WORLD, &world_size);

   // get the rank of this process
   MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

   //create the name of this process
   char buffer[MDI_NAME_LENGTH];
   int str_end;
   strcpy(buffer, hostname_ptr);

   char* names = NULL;
   names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   char* unique_names = NULL;
   unique_names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   MPI_Allgather(&buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
              MPI_CHAR, MPI_COMM_WORLD);

   if (world_rank == 0) {
     for (i=0; i<world_size; i++) {
       char* ptr1 = &names[i*MDI_NAME_LENGTH];
     }
   }

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
     perror("Unable to identify driver when attempting to connect via MPI");
   }

   //if (world_rank == 0) {

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
	   this->mpi_code_rank = nunique_names;
	 }

         // create a communicator to handle communication with this production code
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
         MPI_Comm_split(MPI_COMM_WORLD, color, key, &new_mpi_comm);

	 if ( world_rank == driver_rank || world_rank == i ) {
	   Communicator* new_communicator = new CommunicatorMPI( MDI_MPI, new_mpi_comm, key );
	 }
       }
     }

     if ( do_split ) {

       // create the intra-code communicators
       MPI_Comm_split(MPI_COMM_WORLD, this->mpi_code_rank, world_rank, &this->intra_MPI_comm);
       MPI_Comm_rank(this->intra_MPI_comm, &this->intra_rank);

       MPI_Barrier(MPI_COMM_WORLD);

     }

   return 0;

}


int MethodMPI::split_mpi_communicator(void* world_comm) {
  MPI_Comm* world_comm_ptr = (MPI_Comm*) world_comm;
  *world_comm_ptr = this->intra_MPI_comm;
  return 0;
}
