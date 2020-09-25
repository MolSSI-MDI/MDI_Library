#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "ipi_sockets.h"
#include <cstdlib>

bool exit_signal = false;
int ipi_msglen = 12;
int natoms = 0;

int execute_command(const char* command, int sockfd, void* class_obj) {
  printf("Command: %s\n",command);

  // set dummy molecular information
  int int_bytes = sizeof(int);
  int double_bytes = sizeof(double);
  int ncell = 9;
  int ncell_bytes = 9 * sizeof(double);
  int nstatus_bytes = ipi_msglen * sizeof(char);
  double cell[ncell];
  double celli[ncell];

  if ( strcmp(command, "EXIT") == 0 ) {

    exit_signal = true;

  }
  else if ( strcmp(command, "INIT") == 0 ) {

  }
  else if ( strcmp(command, "STATUS") == 0 ) {

    const char* status = "READY       ";
    writebuffer(&sockfd, status, &nstatus_bytes);

  }
  else if ( strcmp(command, "POSDATA") == 0 ) {

    readbuffer(&sockfd, (char*) cell, &ncell_bytes);
    readbuffer(&sockfd, (char*) celli, &ncell_bytes);
    readbuffer(&sockfd, (char*) &natoms, &int_bytes);
    double coords[3*natoms];
    int ncoords_bytes = 3 * natoms * sizeof(double);
    readbuffer(&sockfd, (char*) coords, &ncoords_bytes);

    // Print the values received via POSDATA
    for (int icell=0; icell < 3; icell++) {
      printf("   CELL: %d %f %f %f\n",icell+1, cell[3*icell+0], cell[3*icell+1], cell[3*icell+2]);
    }
    for (int icell=0; icell < 3; icell++) {
      printf("   CELLi: %d %f %f %f\n",icell+1, celli[3*icell+0], celli[3*icell+1], celli[3*icell+2]);
    }
    printf("   NATOMS: %d\n",natoms);
    for (int iatom=0; iatom < natoms; iatom++) {
      printf("   COORD: %d %f %f %f\n",iatom+1, coords[3*iatom+0], coords[3*iatom+1], coords[3*iatom+2]);
    }
  }
  else if ( strcmp(command, "GETFORCE") == 0 ) {

    const char* force_status = "FORCEREADY  ";
    double pot = -123.0;
    writebuffer(&sockfd, force_status, &nstatus_bytes);
    writebuffer(&sockfd, (char*) &pot, &double_bytes);
    writebuffer(&sockfd, (char*) &natoms, &int_bytes);

    int ncoords_bytes = 3 * natoms * sizeof(double);
    double forces[3*natoms];
    for (int icoord=0; icoord < 3*natoms; icoord++) {
      forces[icoord] = -0.1 * double(icoord);
    }
    writebuffer(&sockfd, (char*) forces, &ncoords_bytes);

    double stress[ncell];
    for (int icell=0; icell < ncell; icell++) {
      stress[icell] = -1.1 * double(icell);
    }
    writebuffer(&sockfd, (char*) stress, &ncell_bytes);

    int extra = 0;
    writebuffer(&sockfd, (char*) &extra, &int_bytes);    

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
  world_comm = MPI_COMM_WORLD;

  int port = -1;
  char* hostname = NULL;
  
  // Read through all the command line options
  int iarg = 1;
  bool initialized_mdi = false;
  while ( iarg < argc ) {

    if ( strcmp(argv[iarg],"-port") == 0 ) {

      // Ensure that the argument to the -port option was provided
      if ( argc-iarg < 2 ) {
	throw std::runtime_error("The -port argument was not provided.");
      }

      port = atoi(argv[iarg+1]);

      iarg += 2;

    }
    if ( strcmp(argv[iarg],"-hostname") == 0 ) {

      // Ensure that the argument to the -hostname option was provided
      if ( argc-iarg < 2 ) {
	throw std::runtime_error("The -hostname argument was not provided.");
      }

      hostname = argv[iarg+1];

      iarg += 2;

    }
    else {
      throw std::runtime_error("Unrecognized option.");
    }

  }

  // Confirm that the port option was provided
  if ( port <= 0 ) {
    throw std::runtime_error("The port command-line argument was not provided, or is not valid.");
  }

  // Confirm that the hostname option was provided
  if ( hostname == NULL ) {
    throw std::runtime_error("The hostname command-line argument was not provided, or is not valid.");
  }
  printf("Hostname: %s\n",hostname);

  int sockfd = 0;
  int inet = 1;
  //const char* hostname = "localhost";
  open_socket(&sockfd, &inet, &port, hostname);

  // Respond to the driver's commands
  char* command = new char[ipi_msglen];
  while( not exit_signal ) {

    // Receive a command from the driver
    readbuffer(&sockfd, command, &ipi_msglen);

    // Execute the command
    execute_command(command, sockfd, NULL);

  }
  delete [] command;

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  close_socket(&sockfd);
  printf("Engine has closed the socket and will now exit.\n");

  return 0;
}
