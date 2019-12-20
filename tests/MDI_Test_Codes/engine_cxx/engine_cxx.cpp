#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "mdi.h"

bool exit_signal = false;

int execute_command(const char* command, MDI_Comm comm, void* class_obj) {
  if ( strcmp(command, "EXIT") == 0 ) {
    exit_signal = true;
  }
  else if ( strcmp(command, "<NATOMS") == 0 ) {
    int natoms = 10;
    MDI_Send(&natoms, 1, MDI_INT, comm);
  }
  else {
    throw std::runtime_error("Unrecognized command.");
  }

  return 0;
}

int main(int argc, char **argv) {

  // Initialize the MPI environment
  MPI_Comm world_comm;
  MPI_Init(&argc, &argv);

  // Read through all the command line options
  int iarg = 1;
  bool initialized_mdi = false;
  while ( iarg < argc ) {

    if ( strcmp(argv[iarg],"-mdi") == 0 ) {

      // Ensure that the argument to the -mdi option was provided
      if ( argc-iarg < 2 ) {
	throw std::runtime_error("The -mdi argument was not provided.");
      }

      // Initialize the MDI Library
      world_comm = MPI_COMM_WORLD;
      int ret = MDI_Init(argv[iarg+1], &world_comm);
      if ( ret != 0 ) {
	throw std::runtime_error("The MDI library was not initialized correctly.");
      }
      initialized_mdi = true;
      iarg += 2;

    }
    else {
      throw std::runtime_error("Unrecognized option.");
    }

  }
  if ( not initialized_mdi ) {
    throw std::runtime_error("The -mdi command line option was not provided.");
  }

  // Set the list of supported commands
  MDI_Register_Node("@GLOBAL");
  MDI_Register_Command("@GLOBAL","EXIT");
  MDI_Register_Command("@GLOBAL","<NATOMS");

  // Create the execute_command pointer
  int (*generic_command)(const char*, MDI_Comm, void*) = execute_command;
  void* engine_obj;
  MDI_Set_Execute_Command_Func(generic_command, engine_obj);

  // Connect to the driver
  MDI_Comm comm;
  MDI_Accept_Communicator(&comm);

  // Respond to the driver's commands
  char* command = new char[MDI_COMMAND_LENGTH];
  while( not exit_signal ) {

    MDI_Recv_Command(command, comm);

    execute_command(command, comm, NULL);

  }
  delete [] command;

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
