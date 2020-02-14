#include <iostream>
#include <stdexcept>
#include <string.h>
#include "mdi.h"

int main(int argc, char **argv) {

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
      int ret = MDI_Init(argv[iarg+1], NULL);
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

  // Confirm that the code is being run as a driver
  int role;
  MDI_Get_Role(&role);
  if ( role != MDI_DRIVER ) {
    throw std::runtime_error("Must run driver_cxx as a DRIVER");
  }

  // Connect to the engine
  MDI_Comm comm;
  MDI_Accept_Communicator(&comm);

  // Confirm that the engine has the @DEFAULT node
  int exists;
  MDI_Check_Node_Exists("@DEFAULT", comm, &exists);
  if ( exists != 1 ) {
    throw std::runtime_error("The engine does not have the @DEFAULT node.");
  }

  // Confirm that the engine supports the EXIT command
  MDI_Check_Command_Exists("@DEFAULT", "EXIT", comm, &exists);
  if ( exists != 1 ) {
    throw std::runtime_error("The engine does not support the EXIT command.");
  }

  // Determine the name of the engine
  char* engine_name = new char[MDI_NAME_LENGTH];
  MDI_Send_Command("<NAME", comm);
  MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);

  std::cout << " Engine name: " << engine_name << std::endl;

  // Send the "EXIT" command to the engine
  MDI_Send_Command("EXIT", comm);

  return 0;
}
