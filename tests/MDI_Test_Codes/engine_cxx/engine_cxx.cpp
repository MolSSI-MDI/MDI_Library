#include <iostream>
#include <stdexcept>
#include <string.h>
#include <stdint.h>
#include "mdi.h"
#include "engine_cxx.h"

bool exit_signal = false;


int initialize_mdi(MDI_Comm* comm_ptr) {
  // Confirm that the code is being run as an engine
  int role;
  MDI_Get_role(&role);
  if ( role != MDI_ENGINE ) {
    throw std::runtime_error("Must run engine_cxx as an ENGINE");
  }

  // Set the list of supported commands
  MDI_Register_node("@DEFAULT");
  MDI_Register_command("@DEFAULT","EXIT");
  MDI_Register_command("@DEFAULT","<NATOMS");
  MDI_Register_command("@DEFAULT","<COORDS");
  MDI_Register_command("@DEFAULT","<FORCES");
  MDI_Register_command("@DEFAULT","<FORCES_B");
  MDI_Register_node("@FORCES");
  MDI_Register_command("@FORCES","EXIT");
  MDI_Register_command("@FORCES","<FORCES");
  MDI_Register_command("@FORCES",">FORCES");
  MDI_Register_callback("@FORCES",">FORCES");

  // Connect to the driver
  MDI_Accept_communicator(comm_ptr);

  // Create the execute_command pointer
  int (*generic_command)(const char*, MDI_Comm, void*) = execute_command;
  void* engine_obj;
  MDI_Set_execute_command_func(generic_command, engine_obj);

  return 0;
}


int respond_to_commands(MDI_Comm comm, MPI_Comm mpi_world_comm) {
  // Respond to the driver's commands
  char* command = new char[MDI_COMMAND_LENGTH];
  while( not exit_signal ) {

    MDI_Recv_command(command, comm);
    MPI_Bcast(command, MDI_COMMAND_LENGTH, MPI_CHAR, 0, mpi_world_comm);

    execute_command(command, comm, NULL);

  }
  delete [] command;

  return 0;
}


int execute_command(const char* command, MDI_Comm comm, void* class_obj) {
  // set dummy molecular information
  int64_t natoms = 10;
  double coords[3*natoms];
  for (int icoord = 0; icoord < 3 * natoms; icoord++) {
    coords[icoord] = 0.1 * double(icoord);
  }
  double forces[3*natoms];
  for (int icoord = 0; icoord < 3 * natoms; icoord++) {
    forces[icoord] = 0.01 * double(icoord);
  }

  if ( strcmp(command, "EXIT") == 0 ) {
    exit_signal = true;
  }
  else if ( strcmp(command, "<NATOMS") == 0 ) {
    MDI_Send(&natoms, 1, MDI_INT64_T, comm);
  }
  else if ( strcmp(command, "<COORDS") == 0 ) {
    MDI_Send(&coords, 3 * natoms, MDI_DOUBLE, comm);
  }
  else if ( strcmp(command, "<FORCES") == 0 ) {
    MDI_Send(&forces, 3 * natoms, MDI_DOUBLE, comm);
  }
  else if ( strcmp(command, "<FORCES_B") == 0 ) {
    MDI_Send(&forces, 3 * natoms * sizeof(double), MDI_BYTE, comm);
  }
  else {
    throw std::runtime_error("Unrecognized command.");
  }

  return 0;
}


int MDI_Plugin_init_engine_cxx() {
  // Get the command-line arguments for this plugin instance
  int mdi_argc;
  if ( MDI_Plugin_get_argc(&mdi_argc) ) {
    throw std::runtime_error("MDI_Plugin_get_argc failed.");
  }
  char** mdi_argv;
  if ( MDI_Plugin_get_argv(&mdi_argv) ) {
    throw std::runtime_error("MDI_Plugin_get_argv failed.");
  }

  // Call MDI_Init
  MDI_Init(&mdi_argc, &mdi_argv);

  // Get the MPI intra-communicator for this code
  MPI_Comm mpi_world_comm = MPI_COMM_WORLD;
  MDI_MPI_get_world_comm(&mpi_world_comm);

  // Perform one-time operations required to establish a connection with the driver
  MDI_Comm comm;
  initialize_mdi(&comm);

  // Set the execute_command callback
  void* engine_obj;
  MDI_Set_execute_command_func(execute_command, engine_obj);

  // Respond to commands from the driver
  respond_to_commands(comm, mpi_world_comm);

  return 0;
}


int main(int argc, char **argv) {
  int ret;

  // Initialize the MPI environment
  MPI_Comm mpi_world_comm;
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
  ret = MDI_MPI_get_world_comm(&mpi_world_comm);
  if ( ret != 0 ) {
    throw std::runtime_error("MDI_MPI_get_world_comm failed.");
  }

  // Perform one-time operations required to establish a connection with the driver
  MDI_Comm comm;
  initialize_mdi(&comm);

  // Respond to commands from the driver
  respond_to_commands(comm, mpi_world_comm);

  // Synchronize all MPI ranks
  MPI_Barrier(mpi_world_comm);
  MPI_Finalize();

  return 0;
}
