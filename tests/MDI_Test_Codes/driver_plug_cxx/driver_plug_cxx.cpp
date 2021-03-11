#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "mdi.h"

int execute_at_node(void* mpi_comm_ptr, MDI_Comm mdi_comm, void* class_object) {
  MPI_Comm mpi_comm = *(MPI_Comm*) mpi_comm_ptr;
  int my_rank;
  MPI_Comm_rank(mpi_comm, &my_rank);

  // Determine the name of the engine
  char* engine_name = new char[MDI_NAME_LENGTH];
  MDI_Send_Command("<NAME", mdi_comm);
  MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, mdi_comm);

  if ( my_rank == 0 ) {
    std::cout << " Engine name: " << engine_name << std::endl;
  }

  // Send the "EXIT" command to the engine
  MDI_Send_Command("EXIT", mdi_comm);

  return 0;
}

int main(int argc, char **argv) {

  // Initialize the MPI environment
  MPI_Comm world_comm;
  MPI_Init(&argc, &argv);

  // Number of ranks that will run the driver
  // This is the number of ranks that will NOT run plugin instances
  int driver_nranks = -1;

  // Number of ranks running EACH plugin instance
  int plugin_nranks = -1;

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
      MDI_MPI_get_world_comm(&world_comm);
      if ( ret != 0 ) {
	throw std::runtime_error("The MDI library was not initialized correctly.");
      }
      initialized_mdi = true;
      iarg += 2;

    }
    else if ( strcmp(argv[iarg],"-driver_nranks") == 0 ) {

      // Ensure that the argument to the -mdi option was provided
      if ( argc-iarg < 2 ) {
	throw std::runtime_error("The -driver_nranks argument was not provided.");
      }

      // Set driver_nranks
      char* strtol_ptr;
      driver_nranks = strtol( argv[iarg+1], &strtol_ptr, 10 );
      iarg += 2;

    }
    else if ( strcmp(argv[iarg],"-plugin_nranks") == 0 ) {

      // Ensure that the argument to the -mdi option was provided
      if ( argc-iarg < 2 ) {
	throw std::runtime_error("The -plugin_nranks argument was not provided.");
      }

      // Set driver_nranks
      char* strtol_ptr;
      plugin_nranks = strtol( argv[iarg+1], &strtol_ptr, 10 );
      iarg += 2;

    }
    else {
      throw std::runtime_error("Unrecognized option.");
    }

  }
  if ( not initialized_mdi ) {
    throw std::runtime_error("The -mdi command line option was not provided.");
  }

  // Verify the value of driver_nranks
  if ( driver_nranks < 0 ) {
    throw std::runtime_error("Invalid value for driver_nranks [0, inf).");
  }

  // Verify the value of plugin_nranks
  if ( plugin_nranks <= 0 ) {
    throw std::runtime_error("Invalid value for plugin_nranks (0, inf).");
  }

  // Verify that the value of driver_nranks and plugin_nranks is consistent with world_size
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  if ( (world_size - driver_nranks) % plugin_nranks != 0 ) {
    throw std::runtime_error("Invalid values for driver_nranks and plugin_nranks: world_size - driver_nranks must be divisible by plugin_nranks.");
  }

  // Initialize an instance of the engine library
  MDI_Launch_plugin("engine_cxx", "", &world_comm, execute_at_node, nullptr);

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
