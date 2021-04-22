#ifdef _WIN32
#define _hypot hypot
#include <cmath>
#endif
#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "mdi.h"
#include "Python.h"
#include <sstream>


void launch_python_code( const char* engine_name, const char* options, MPI_Comm engine_comm ) {
  // Open the Python script for the Engine
  FILE* engine_script = fopen(engine_name, "r");

  if( engine_script ) {

    // Check if MPI_Comm is a pointer
    bool is_pointer = std::is_pointer<MPI_Comm>::value;

    // Set the command-line arguments for the engine
    int argc_py = 4;
    const char *argv_py[ argc_py ];
    argv_py[0] = engine_name;
    argv_py[1] = options;
    if ( is_pointer ) {
      argv_py[2] = "pointer";
    }
    else {
      argv_py[2] = "int";
    }
    std::ostringstream engine_comm_address;
    engine_comm_address << engine_comm;
    argv_py[3] = engine_comm_address.str().c_str();
    wchar_t** wargv_py = new wchar_t*[argc_py];
    for(int i = 0; i < argc_py; i++) {
      wargv_py[i] = Py_DecodeLocale(argv_py[i], nullptr);
    }
    PySys_SetArgv(argc_py, wargv_py);

    // Run the Python script
    PyRun_SimpleFile( engine_script, engine_name);

    delete [] wargv_py;
  }
  
  // Close the script
  fclose( engine_script );
}


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

  // Get the number of processes in MPI_COMM_WORLD
  int world_size;
  MPI_Comm_size(world_comm, &world_size);

  // Get the rank of this process in MPI_COMM_WORLD
  int my_rank;
  MPI_Comm_rank(world_comm, &my_rank);

  // Split the communicator into sub-comms for the engine instances
  int color = my_rank % 2;
  int key = my_rank;
  MPI_Comm sub_comm;
  MPI_Comm_split(world_comm, color, key, &sub_comm);

  // Variable to hold the name of the engine
  char* engine_name = new char[MDI_NAME_LENGTH];

  // Initialize the Python interpreter
  Py_Initialize();

  /*****************************************************/
  /*************** Engine Instance 1 *******************/
  /*****************************************************/
  if ( my_rank % 2 == 0 ) {

    // Launch the engine
    launch_python_code("engine_lib_cxx_py.py",
	       "-name instance1 -role ENGINE -method LINK",
	       sub_comm);

    // Connect to the engine
    MDI_Comm comm;
    MDI_Accept_communicator(&comm);

    // Determine the name of the engine
    MDI_Send_command("<NAME", comm);
    MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);

    // Send the "EXIT" command to the engine
    MDI_Send_command("EXIT", comm);
    
  }
  /*****************************************************/
  /*************** Engine Instance 2 *******************/
  /*****************************************************/
  else {

    // Launch the engine
    launch_python_code("engine_lib_cxx_py.py",
	       "-name instance2 -role ENGINE -method LINK",
	       sub_comm);

    // Connect to the engine
    MDI_Comm comm;
    MDI_Accept_communicator(&comm);

    // Determine the name of the engine
    MDI_Send_command("<NAME", comm);
    MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);

    // Send the "EXIT" command to the engine
    MDI_Send_command("EXIT", comm);

  }

  // Print the names of the engines on rank 0
  if ( world_size > 1 ) {
    char* all_names = new char[world_size * MDI_NAME_LENGTH];
    MPI_Gather( engine_name, MDI_NAME_LENGTH, MPI_CHAR,
		all_names, MDI_NAME_LENGTH, MPI_CHAR,
		0, world_comm );
    if ( my_rank == 0 ) {
      std::cout << " Instance name: " << &all_names[0] << std::endl;
      std::cout << " Instance name: " << &all_names[MDI_NAME_LENGTH] << std::endl;
    }
    delete [] all_names;
  }
  else {
    std::cout << " Instance name: " << &engine_name[0] << std::endl;
  }

  delete [] engine_name;

  // Finalize the Python interpreter
  Py_Finalize();

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
