/*! \file
 *
 * \brief Class definition for top-level manager of MDI operations
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>
#include "mdi.h"
#include "method.h"
#include "communicator.h"

using namespace MDI_STUBS;
using namespace std;


static int sigint_sockfd;
void sigint_handler(int dummy) {
  close(sigint_sockfd);
}


MethodTCP::MethodTCP() {
  this->tcp_socket = -1;
}


int MethodTCP::MDI_Listen_TCP(int port) {
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Could not create socket");
    return -1;
  }

  // ensure that the socket is closed on sigint
  sigint_sockfd = sockfd;
  signal(SIGINT, sigint_handler);

  // create the socket address
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  // enable reuse of the socket
  ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse_value, sizeof(int));
  if (ret < 0) {
    perror("Could not reuse socket");
    return -1;
  }

  // bind the socket
  ret = bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
  if (ret < 0) {
    perror("Could not bind socket");
    return -1;
  }

  // start listening (the second argument is the backlog size)
  ret = listen(sockfd, 20);
  if (ret < 0) {
    perror("Could not listen");
    return -1;
  }

  //return sockfd;
  this->tcp_socket = sockfd;

  return 0;
}


int MethodTCP::MDI_Request_Connection_TCP(int port, char* hostname_ptr) {
  int ret, sockfd;

  struct sockaddr_in driver_address;
  struct hostent* host_ptr;

  // get the address of the host
  host_ptr = gethostbyname((char*) hostname_ptr);
  if (host_ptr == NULL) {
    perror("Error in gethostbyname");
    return -1;
  }
  if (host_ptr->h_addrtype != AF_INET) {
    perror("Unkown address type");
    return -1;
  }

  bzero((char *) &driver_address, sizeof(driver_address));
  driver_address.sin_family = AF_INET;
  driver_address.sin_addr.s_addr = 
    ((struct in_addr *)host_ptr->h_addr_list[0])->s_addr;
  driver_address.sin_port = htons(port);

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Could not create socket");
    return -1;
  }

  // connect to the driver
  // if the connection is refused, try again
  //   this allows the production code to start before the driver
  int try_connect = 1;
  while (try_connect == 1) {
    ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr));
    if (ret < 0 ) {
      if ( errno == ECONNREFUSED ) {

	// close the socket, so that a new one can be created
	ret = close(sockfd);
	if (ret < 0) {
	  perror("Could not close socket");
	  return -1;
	}

	// create the socket
	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) {
	  perror("Could not create socket");
	  return -1;
	}

      }
      else { // only error out for errors other than "connection refused"
	perror("Could not connect to the driver");
	return -1;
      }

    }
    else {
      try_connect = 0;
    }
  }

  Communicator* new_communicator = new CommunicatorTCP( MDI_TCP, sockfd );

  return 0;
}


int MethodTCP::On_Accept_Communicator() {
  int connection;

  connection = accept(this->tcp_socket, NULL, NULL);
  if (connection < 0) {
    perror("Could not accept connection");
    exit(-1);
  }

  Communicator* new_communicator = new CommunicatorTCP( MDI_TCP, connection );

  return 0;
}







MethodMPI::MethodMPI() {
  this->intra_MPI_comm = 0;
  this->intra_rank = 0;
  this->mpi_code_rank = 0;
}


int MethodMPI::gather_names(const char* hostname_ptr, bool do_split) {

   int i, j;
   int driver_rank;
   int nunique_names = 0;
   int world_rank;

   // get the number of processes
   int world_size;
   MPI_Comm_size(MPI_COMM_WORLD, &world_size);

   // get the rank of this process
   MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

   //create the name of this process
   //char buffer[MDI_NAME_LENGTH];
   char* buffer = new char[MDI_NAME_LENGTH];
   strcpy(buffer, hostname_ptr);

   char* names = NULL;
   names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   char* unique_names = NULL;
   unique_names = (char*)malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   MPI_Allgather(buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
              MPI_CHAR, MPI_COMM_WORLD);
   delete[] buffer;

   // determine which rank corresponds to rank 0 of the driver
   driver_rank = -1;
   for (i=0; i<world_size; i++) {
     if ( driver_rank == -1 ) {
       //char name[MDI_NAME_LENGTH];
       char* name = new char[MDI_NAME_LENGTH];
       memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
       if ( strcmp(name, "") == 0 ) {
	 driver_rank = i;
       }
       delete[] name;
     }
   }
   if ( driver_rank == -1 ) {
     perror("Unable to identify driver when attempting to connect via MPI");
   }

   //if (world_rank == 0) {

     //create communicators
     for (i=0; i<world_size; i++) {
       //char name[MDI_NAME_LENGTH];
       char* name = new char[MDI_NAME_LENGTH];
       memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

       int found = 0;
       for (j=0; j<i; j++) {
	 //char prev_name[MDI_NAME_LENGTH];
	 char* prev_name = new char[MDI_NAME_LENGTH];
	 memcpy( prev_name, &names[j*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
	 if ( strcmp(name, prev_name) == 0 ) {
	   found = 1;
	 }
	 delete[] prev_name;
       }

       // check if this rank is the first instance of a new production code
       if ( found == 0 && strcmp(name,"") != 0 ) {
	 // add this code's name to the list of unique names
	 memcpy( &unique_names[nunique_names*MDI_NAME_LENGTH], name, MDI_NAME_LENGTH );
	 nunique_names++;
	 //char my_name[MDI_NAME_LENGTH];
	 char* my_name = new char[MDI_NAME_LENGTH];
	 memcpy( my_name, &names[world_rank*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
	 if ( strcmp(my_name, name) == 0 ) {
	   this->mpi_code_rank = nunique_names;
	 }
	 delete[] my_name;

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

       delete[] name;
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
