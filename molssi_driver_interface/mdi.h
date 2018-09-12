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

// length of an MDI command in characters
extern const int MDI_COMMAND_LENGTH;

// MDI data types
extern const int MDI_INT;
extern const int MDI_DOUBLE;
extern const int MDI_CHAR;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
extern const int MDI_METER_TO_BOHR;
extern const int MDI_ANGSTROM_TO_BOHR;

// time
extern const int MDI_SECOND_TO_AUT;
extern const int MDI_PICOSECOND_TO_AUT;

// force
extern const int MDI_NEWTON_TO_AUF;

// energy
extern const int MDI_JOULE_TO_HARTREE;
extern const int MDI_KJ_TO_HARTREE;
extern const int MDI_KJPERMOL_TO_HARTREE;
extern const int MDI_KCALPERMOL_TO_HARTREE;
extern const int MDI_EV_TO_HARTREE;
extern const int MDI_RYDBERG_TO_HARTREE;
extern const int MDI_KELVIN_TO_HARTREE;

int MDI_Init(int*);
int MDI_Open(int*, int*, int*, const char*);
int MDI_Accept_Connection(int*, int*);
int MDI_Send(const char*, int*, int*, int*);
int MDI_Recv(char*, int*, int*, int*);
int MDI_Send_Command(const char*, int*);
int MDI_Recv_Command(char*, int*);

int launch_server(const char*);

#endif
