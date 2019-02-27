/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init: Initializes a socket and sets it to listen
   MDI_Accept_Communicator: Accepts a new MDI communicator
   MDI_Send: Sends data through the socket
   MDI_Recv: Receives data from the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH over the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH over the
      socket
*/

#ifndef MDI_LIBRARY
#define MDI_LIBRARY

#ifdef __cplusplus
namespace MDI_STUBS { }
extern "C" {
#endif

// type of an MDI communicator handle
typedef int MDI_Comm;

// type of an MDI datatype handle
typedef int MDI_Datatype;

// length of an MDI command in characters
extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
extern const int MDI_NAME_LENGTH;

// value of a null communicator
extern const MDI_Comm MDI_NULL_COMM;

// MDI data types
extern const int MDI_INT;
extern const int MDI_DOUBLE;
extern const int MDI_CHAR;
extern const int MDI_INT_NUMPY;
extern const int MDI_DOUBLE_NUMPY;

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

int MDI_Init(const char* options, void* world_comm);
int MDI_Accept_Communicator();
int MDI_Send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int MDI_Recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int MDI_Send_Command(const char* buf, MDI_Comm comm);
int MDI_Recv_Command(char* buf, MDI_Comm comm);
double MDI_Conversion_Factor(char* in_unit, char* out_unit);

// only used internally by MDI
void mdi_error(const char* message);
int MDI_Get_MPI_Code_Rank();
void MDI_Set_MPI_Intra_Rank(int rank);

#ifdef __cplusplus
}
#endif

#endif
