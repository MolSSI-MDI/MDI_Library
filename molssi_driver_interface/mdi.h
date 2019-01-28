/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init: Initializes a socket and sets it to listen
   MDI_Open: Opens a socket and requests a connection with a specified host
   MDI_Accept_Connection: Accepts an incoming connection request
   MDI_Send: Sends data through the socket
   MDI_Recv: Receives data from the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH over the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH over the
      socket
*/

#ifndef MDI_LIBRARY
#define MDI_LIBRARY

#include <mpi.h>

// length of an MDI command in characters
extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
extern const int MDI_NAME_LENGTH;

// MDI data types
extern const int MDI_INT;
extern const int MDI_DOUBLE;
extern const int MDI_CHAR;

// MDI communication types
extern const int MDI_TCP;
extern const int MDI_MPI;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
extern const double MDI_METER_TO_BOHR;
extern const double MDI_ANGSTROM_TO_BOHR;

// time
extern const double MDI_SECOND_TO_AUT;
extern const double MDI_PICOSECOND_TO_AUT;

// force
extern const double MDI_NEWTON_TO_AUF;

// energy
extern const double MDI_JOULE_TO_HARTREE;
extern const double MDI_KJ_TO_HARTREE;
extern const double MDI_KJPERMOL_TO_HARTREE;
extern const double MDI_KCALPERMOL_TO_HARTREE;
extern const double MDI_EV_TO_HARTREE;
extern const double MDI_RYDBERG_TO_HARTREE;
extern const double MDI_KELVIN_TO_HARTREE;

int MDI_Init(const char* options, void* data, MPI_Comm* world_comm);
int MDI_Request_Connection(const char* method, void* options, void* world_comm);
int MDI_Accept_Connection();
int MDI_Send(const char*, int, int, int);
int MDI_Recv(char*, int, int, int);
int MDI_Send_Command(const char*, int);
int MDI_Recv_Command(char*, int);
int MDI_MPI_Comm(MPI_Comm*);

#endif
