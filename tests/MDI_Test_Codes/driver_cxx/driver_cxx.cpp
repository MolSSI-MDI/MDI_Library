#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
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
  int world_rank;
  MPI_Comm_rank(world_comm, &world_rank);

  // Confirm that the code is being run as a driver
  int role;
  MDI_Get_role(&role);
  if ( role != MDI_DRIVER ) {
    throw std::runtime_error("Must run driver_cxx as a DRIVER");
  }

  // Check for a new communicator
  int new_comm_flag = 0;
  while ( new_comm_flag != 1 ) {
    MDI_Check_for_communicator(&new_comm_flag);
    MPI_Bcast(&new_comm_flag, 1, MPI_INT, 0, world_comm);
  }

  /*
  if ( new_comm_flag != 1 ) {
    throw std::runtime_error("Couldn't find a new communicator");
  }
  */

  // Connect to the engine
  MDI_Comm comm;
  MDI_Accept_communicator(&comm);

  // Confirm that there is no additional communicator
  MDI_Check_for_communicator(&new_comm_flag);
  MPI_Bcast(&new_comm_flag, 1, MPI_INT, 0, world_comm);
  if ( new_comm_flag != 0 ) {
    throw std::runtime_error("After accepting the connection, there is still a communicator");
  }

  // Confirm that the engine has the @DEFAULT node
  int exists = 1;
  MDI_Check_node_exists("@DEFAULT", comm, &exists);
  if ( exists != 1 ) {
    throw std::runtime_error("The engine does not have the @DEFAULT node.");
  }

  // Confirm that the engine supports the EXIT command
  MDI_Check_command_exists("@DEFAULT", "EXIT", comm, &exists);
  if ( exists != 1 ) {
    throw std::runtime_error("The engine does not support the EXIT command.");
  }

  // Determine the name of the engine
  char* engine_name = new char[MDI_NAME_LENGTH];
  MDI_Send_command("<NAME", comm);
  MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);

  if ( world_rank == 0 ) {
    std::cout << " Engine name: " << engine_name << std::endl;
  }
  delete[] engine_name;

  // Send the "EXIT" command to the engine
  MDI_Send_command("EXIT", comm);

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
