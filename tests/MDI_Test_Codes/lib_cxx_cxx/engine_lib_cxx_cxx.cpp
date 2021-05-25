#include <mpi.h>
#include <iostream>
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdi.h"
#include "engine_lib_cxx_cxx.h"

int engine_lib_cxx_create(MPI_Comm mpi_comm) {
  MPI_Comm world_comm = mpi_comm;

  // Call MDI_Init
  int argc_mdi = 2;
  int options_length = 256;
  char* options = (char*) malloc( options_length * sizeof(char) );
  snprintf(options, options_length, "-role ENGINE -method LINK -name MM");
  char* mdi_arg = (char*) malloc( options_length * sizeof(char) );
  snprintf(mdi_arg, options_length, "-mdi");
  char** argv_mdi = (char**) malloc( argc_mdi * sizeof(char*) );
  argv_mdi[0] = mdi_arg;
  argv_mdi[1] = options;
  MDI_Init(&argc_mdi, &argv_mdi);
  free(options);
  free(mdi_arg);
  free(argv_mdi);

  // Get the MPI intra-communicator for this code
  MDI_MPI_set_world_comm(&world_comm);

  // Set the execute_command callback
  void* engine_obj;
  MDI_Set_Execute_Command_Func(execute_command, engine_obj);

  return 0;
}

int execute_command(const char* command, MDI_Comm comm, void* class_obj) {
  return 0;
}
