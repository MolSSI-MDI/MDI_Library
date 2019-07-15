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
//namespace MDI_STUBS { }
extern "C" {
#endif

// type of an MDI communicator handle
__declspec(dllexport) typedef int MDI_Comm;

// type of an MDI datatype handle
__declspec(dllexport) typedef int MDI_Datatype;

// MDI version number
__declspec(dllexport) extern const double MDI_VERSION;

// length of an MDI command in characters
__declspec(dllexport) extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
__declspec(dllexport) extern const int MDI_NAME_LENGTH;

// value of a null communicator
__declspec(dllexport) extern const MDI_Comm MDI_NULL_COMM;

// MDI data types
__declspec(dllexport) extern const int MDI_INT;
__declspec(dllexport) extern const int MDI_DOUBLE;
__declspec(dllexport) extern const int MDI_CHAR;
__declspec(dllexport) extern const int MDI_INT_NUMPY;
__declspec(dllexport) extern const int MDI_DOUBLE_NUMPY;

// MDI communication types
__declspec(dllexport) extern const int MDI_TCP;
__declspec(dllexport) extern const int MDI_MPI;
__declspec(dllexport) extern const int MDI_TEST;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
__declspec(dllexport) extern const double MDI_METER_TO_BOHR;
__declspec(dllexport) extern const double MDI_ANGSTROM_TO_BOHR;

// time
__declspec(dllexport) extern const double MDI_SECOND_TO_AUT;
__declspec(dllexport) extern const double MDI_PICOSECOND_TO_AUT;

// force
__declspec(dllexport) extern const double MDI_NEWTON_TO_AUF;

// energy
__declspec(dllexport) extern const double MDI_JOULE_TO_HARTREE;
__declspec(dllexport) extern const double MDI_KJ_TO_HARTREE;
__declspec(dllexport) extern const double MDI_KJPERMOL_TO_HARTREE;
__declspec(dllexport) extern const double MDI_KCALPERMOL_TO_HARTREE;
__declspec(dllexport) extern const double MDI_EV_TO_HARTREE;
__declspec(dllexport) extern const double MDI_RYDBERG_TO_HARTREE;
__declspec(dllexport) extern const double MDI_KELVIN_TO_HARTREE;

__declspec(dllexport) int MDI_Init(const char* options, void* world_comm);
__declspec(dllexport) int MDI_Accept_Communicator();
__declspec(dllexport) int MDI_Send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
__declspec(dllexport) int MDI_Recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
__declspec(dllexport) int MDI_Send_Command(const char* buf, MDI_Comm comm);
__declspec(dllexport) int MDI_Recv_Command(char* buf, MDI_Comm comm);
__declspec(dllexport) double MDI_Conversion_Factor(char* in_unit, char* out_unit);

// only used internally by MDI
__declspec(dllexport) void mdi_error(const char* message);
__declspec(dllexport) int MDI_Get_MPI_Code_Rank();
__declspec(dllexport) void MDI_Set_MPI_Intra_Rank(int rank);

#ifdef __cplusplus
}
#endif

#endif
