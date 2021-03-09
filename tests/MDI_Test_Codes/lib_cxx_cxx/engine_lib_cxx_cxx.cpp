#include <mpi.h>
#include <iostream>
#include <stdexcept>
#include <string.h>
#include "mdi.h"
#include "engine_lib_cxx_cxx.h"

int MDI_Plugin_init() {
  // Initialize MDI
  MPI_Comm world_comm = MPI_COMM_WORLD;
  int ret = MDI_Init("-role ENGINE -method LINK -name MM -driver_name driver", &world_comm);
  MDI_MPI_get_world_comm(&world_comm);

  /////////////////////
  MDI_Comm comm;
  MDI_Accept_Communicator(&comm);
  /////////////////////

  // Set the execute_command callback
  void* engine_obj;
  MDI_Set_Execute_Command_Func(execute_command, engine_obj);

  /////////////////////
  char* command = new char[MDI_COMMAND_LENGTH];
  MDI_Recv_Command(command, comm);
  delete [] command;
  /////////////////////
  
  return 0;
}

int execute_command(const char* command, MDI_Comm comm, void* class_obj) {
  return 0;
}
