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
#include "method.h"

//this is the number of communicator handles that have been returned by MDI_Accept_Connection()
static int returned_comms = 0;

//name of the driver/engine
static char* name;

int manager_init(const char* options, void* world_comm) {
  returned_comms = 0;
  vector_init(&communicators, sizeof(communicator));

  char* strtol_ptr;
  int i;

  int mpi_initialized = 0;

  // values acquired from the input options
  char* role;
  char* method;
  name = malloc(MDI_NAME_LENGTH+1);
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
  char* argv[argc];
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
      if ( strlen(argv[iarg+1]) > MDI_NAME_LENGTH ) {
	mdi_error("Name argument length exceeds MDI_NAME_LENGTH");
      }
      strcpy(name, argv[iarg+1]);
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
  int do_split = 1;
  if ( strcmp(language, "Python") == 0 ) {
    do_split = 0;
  }

  if ( strcmp(role, "DRIVER") == 0 ) {
    // initialize this code as a driver

    if ( strcmp(method, "MPI") == 0 ) {
      gather_names("", do_split);
      mpi_initialized = 1;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	MDI_Listen_TCP(port);
      }
    }
    else {
      mdi_error("Error in MDI_Init: method not recognized");
    }

  }
  else if ( strcmp(role,"ENGINE") == 0 ) {
    // initialize this code as an engine

    if ( strcmp(method, "MPI") == 0 ) {
      gather_names(name, do_split);
      mpi_initialized = 1;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_hostname == 0 ) {
	mdi_error("Error in MDI_Init: -hostname option not provided");
      }
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
      }
      if ( mpi_rank == 0 ) {
	MDI_Request_Connection_TCP(port, hostname);
      }
    }
    
  }
  else {
    mdi_error("Error in MDI_Init: role not recognized");
  }

  // set the MPI communicator correctly
  if ( mpi_initialized == 1 ) {
    if ( do_split == 1 ) {
      split_mpi_communicator(world_comm);
    }
  }

  //delete[] argv;
  free( argv_line );

  return 0;
}


int manager_accept_communicator() {
  // if MDI hasn't returned some connections, do that now
  if ( returned_comms < communicators.size ) {
    returned_comms++;
    return returned_comms;
  }

  // check for any production codes connecting via TCP
  if ( tcp_socket > 0 ) {

    //accept a connection via TCP
    On_Accept_Communicator();

    // if MDI hasn't returned some connections, do that now
    if ( returned_comms < communicators.size ) {
      returned_comms++;
      return (MDI_Comm)returned_comms;
    }

  }

  // unable to accept any connections
  return MDI_NULL_COMM;
}


int manager_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  if ( intra_rank != 0 ) {
    mdi_error("Called MDI_Send with incorrect rank");
  }

  communicator_send(buf, count, datatype, comm);

  return 0;
}


int manager_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  if ( intra_rank != 0 ) {
    mdi_error("Called MDI_Recv with incorrect rank");
  }

  communicator_recv(buf, count, datatype, comm);

  return 0;
}


int manager_send_command(const char* buf, MDI_Comm comm) {
  if ( intra_rank != 0 ) {
    mdi_error("Called MDI_Send_Command with incorrect rank");
  }
  int count = MDI_COMMAND_LENGTH;
  char command[MDI_COMMAND_LENGTH];

  strcpy(command, buf);
  int ret = manager_send( command, count, MDI_CHAR, comm );
  return ret;
}


int manager_recv_command(char* buf, MDI_Comm comm) {
  if ( intra_rank != 0 ) {
    mdi_error("Called MDI_Recv_Command with incorrect rank");
  }
  int count = MDI_COMMAND_LENGTH;
  int datatype = MDI_CHAR;

  //return manager_recv( buf, count, datatype, comm );
  int ret = manager_recv( buf, count, datatype, comm );

  // check if this command corresponds to one of MDI's standard built-in commands
  if ( strcmp( buf, "<NAME" ) == 0 ) {
    MDI_Send(name, MDI_NAME_LENGTH, MDI_CHAR, comm);
    return manager_recv_command(buf, comm);
  }

  return ret;
}
