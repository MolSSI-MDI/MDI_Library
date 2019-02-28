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
#include "mdi_manager.h"
#include "communicator.h"

using namespace MDI_STUBS;
using namespace std;

MDIManager::MDIManager(const char* options, void* world_comm) {
  this->method_tcp = new MethodTCP();
  this->method_mpi = new MethodMPI();
  this->returned_comms = 0;

  char* strtol_ptr;
  int i;

  bool mpi_initialized = false;

  // values acquired from the input options
  char* role;
  char* method;
  char* name;
  char* hostname;
  int port;
  char* language = ((char*)"");
  int has_role = 0;
  int has_method = 0;
  int has_name = 0;
  int has_hostname = 0;
  int has_port = 0;

  // get the MPI rank
  MPI_Comm mpi_communicator;
  int mpi_rank = 0;
  if ( world_comm == NULL ) {
    mpi_communicator = 0;
    mpi_rank = 0;
  }
  else {
    mpi_communicator = *(MPI_Comm*) world_comm;
    MPI_Comm_rank(mpi_communicator, &mpi_rank);
  }

  // calculate argc
  char* argv_line = strdup(options);
  char* token = strtok(argv_line, " ");
  int argc = 0;
  while (token != NULL) {
    argc++;
    token = strtok(NULL," ");
  }

  // calculate argv
  //char* argv[argc];
  char** argv = new char*[argc];
  argv_line = strdup(options);
  token = strtok(argv_line, " ");
  for (i=0; i<argc; i++) {
    argv[i] = token;
    token = strtok(NULL," ");
  }

  // read options
  int iarg = 0;
  while (iarg < argc) {

    //-role
    if (strcmp(argv[iarg],"-role") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -role option");
      }
      role = argv[iarg+1];
      has_role = 1;
      iarg += 2;
    }
    //-method
    else if (strcmp(argv[iarg],"-method") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -method option");
      }
      method = argv[iarg+1];
      has_method = 1;
      iarg += 2;
    }
    //-name
    else if (strcmp(argv[iarg],"-name") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -name option");
      }
      name = argv[iarg+1];
      has_name = 1;
      iarg += 2;
    }
    //-hostname
    else if (strcmp(argv[iarg],"-hostname") == 0){
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -hostname option");
      }
      hostname = argv[iarg+1];
      has_hostname = 1;
      iarg += 2;
    }
    //-port
    else if (strcmp(argv[iarg],"-port") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from -port option");
      }
      port = strtol( argv[iarg+1], &strtol_ptr, 10 );
      has_port = 1;
      iarg += 2;
    }
    //_language
    else if (strcmp(argv[iarg],"_language") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Argument missing from _language option");
      }
      language = argv[iarg+1];
      iarg += 2;
    }
    else {
      mdi_error("Unrecognized option");
    }
  }

  // ensure the -role option was provided
  if ( has_role == 0 ) {
    mdi_error("Error in MDI_Init: -role option not provided");
  }

  // ensure the -name option was provided
  if ( has_name == 0 ) {
    mdi_error("Error in MDI_Init: -name option not provided");
  }

  // ensure the -method option was provided
  if ( has_method == 0 ) {
    mdi_error("Error in MDI_Init: -method option not provided");
  }

  // determine whether the intra-code MPI communicator should be split by gather_names
  bool do_split = true;
  if ( strcmp(language, "Python") == 0 ) {
    do_split = false;
  }

  if ( strcmp(role, "DRIVER") == 0 ) {
    // initialize this code as a driver

    if ( strcmp(method, "MPI") == 0 ) {
      this->method_mpi->gather_names("", do_split);
      mpi_initialized = true;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	this->method_tcp->MDI_Listen_TCP(port);
      }
    }
    else {
      mdi_error("Error in MDI_Init: method not recognized");
    }

  }
  else if ( strcmp(role,"ENGINE") == 0 ) {
    // initialize this code as an engine

    if ( strcmp(method, "MPI") == 0 ) {
      this->method_mpi->gather_names(name, do_split);
      mpi_initialized = true;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_hostname == 0 ) {
	mdi_error("Error in MDI_Init: -hostname option not provided");
      }
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	this->method_tcp->MDI_Request_Connection_TCP(port, hostname);
      }
    }
    
  }
  else {
    mdi_error("Error in MDI_Init: role not recognized");
  }

  // set the MPI communicator correctly
  if ( mpi_initialized ) {
    if ( do_split ) {
      this->method_mpi->split_mpi_communicator(world_comm);
    }
  }

  delete[] argv;
  free( argv_line );

}


int MDIManager::accept_communicator() {
  // if MDI hasn't returned some connections, do that now
  if ( this->returned_comms < communicators.size() ) {
    this->returned_comms++;
    return this->returned_comms;
  }

  // check for any production codes connecting via TCP
  if ( this->method_tcp->tcp_socket > 0 ) {

    //accept a connection via TCP
    this->method_tcp->On_Accept_Communicator();

    // if MDI hasn't returned some connections, do that now
    if ( this->returned_comms < communicators.size() ) {
      this->returned_comms++;
      return (MDI_Comm)this->returned_comms;
    }

  }

  // unable to accept any connections
  return MDI_NULL_COMM;
}


int MDIManager::send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  if ( this->method_mpi->intra_rank != 0 ) {
    mdi_error("Called MDI_Send with incorrect rank");
  }

  Communicator* send_comm = communicators[comm-1];
  send_comm->send(buf, count, datatype);

  return 0;
}


int MDIManager::recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  if ( this->method_mpi->intra_rank != 0 ) {
    mdi_error("Called MDI_Recv with incorrect rank");
  }

  Communicator* recv_comm = communicators[comm-1];
  recv_comm->recv(buf, count, datatype);

  return 0;
}


int MDIManager::send_command(const char* buf, MDI_Comm comm) {
  if ( this->method_mpi->intra_rank != 0 ) {
    mdi_error("Called MDI_Send_Command with incorrect rank");
  }
  int count = MDI_COMMAND_LENGTH;
  //char command[MDI_COMMAND_LENGTH];
  char* command = new char[MDI_COMMAND_LENGTH];

  strcpy(command, buf);
  int ret = this->send( command, count, MDI_CHAR, comm );
  delete[] command;
  return ret;
}


int MDIManager::recv_command(char* buf, MDI_Comm comm) {
  if ( this->method_mpi->intra_rank != 0 ) {
    mdi_error("Called MDI_Recv_Command with incorrect rank");
  }
  int count = MDI_COMMAND_LENGTH;
  int datatype = MDI_CHAR;

  return this->recv( buf, count, datatype, comm );
}
