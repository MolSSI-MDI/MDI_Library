#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "engine_lib_cxx_cxx.h"
#include "mdi.h"

int main(int argc, char **argv) {
  int ret;

  // Initialize the MPI environment
  MPI_Comm world_comm;
  MPI_Init(&argc, &argv);

  // Initialize MDI
  ret = MDI_Init(&argc, &argv);
  if ( ret != 0 ) {
    throw std::runtime_error("The MDI library was not initialized correctly.");
  }

  // Confirm that MDI was initialized successfully
  int initialized_mdi;
  ret = MDI_Initialized(&initialized_mdi);
  if ( ret != 0 ) {
    throw std::runtime_error("MDI_Initialized failed.");
  }
  if ( ! initialized_mdi ) {
    throw std::runtime_error("MDI not initialized: did you provide the -mdi option?.");
  }

  // Get the correct MPI intra-communicator for this code
  ret = MDI_MPI_get_world_comm(&world_comm);
  if ( ret != 0 ) {
    throw std::runtime_error("MDI_MPI_get_world_comm failed.");
  }

  // Initialize an instance of the engine library
  engine_lib_cxx_create(world_comm);

  // Connect to the engine
  MDI_Comm comm;
  MDI_Accept_communicator(&comm);

  // Determine the name of the engine
  char* engine_name = new char[MDI_NAME_LENGTH];
  MDI_Send_command("<NAME", comm);
  MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);

  std::cout << " Engine name: " << engine_name << std::endl;

  // Send the "EXIT" command to the engine
  MDI_Send_command("EXIT", comm);

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
