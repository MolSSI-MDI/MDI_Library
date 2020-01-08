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

// ensure that symbols are exported to Windows .dll files
#ifdef _WIN32
  #define DllExport   __declspec( dllexport )
#else
  #define DllExport 
#endif

// type of an MDI communicator handle
typedef int MDI_Comm;

// type of an MDI datatype handle
typedef int MDI_Datatype;

// MDI version numbers
DllExport extern const int MDI_MAJOR_VERSION;
DllExport extern const int MDI_MINOR_VERSION;
DllExport extern const int MDI_PATCH_VERSION;

// length of an MDI command in characters
DllExport extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
DllExport extern const int MDI_NAME_LENGTH;

// value of a null communicator
DllExport extern const MDI_Comm MDI_NULL_COMM;

// MDI data types
DllExport extern const int MDI_INT;
DllExport extern const int MDI_DOUBLE;
DllExport extern const int MDI_CHAR;
DllExport extern const int MDI_INT_NUMPY;
DllExport extern const int MDI_DOUBLE_NUMPY;

// MDI communication types
DllExport extern const int MDI_TCP;
DllExport extern const int MDI_MPI;
DllExport extern const int MDI_LIB;
DllExport extern const int MDI_TEST;

// MDI role types
DllExport extern const int MDI_DRIVER;
DllExport extern const int MDI_ENGINE;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

DllExport int MDI_Init(const char* options, void* world_comm);
DllExport int MDI_Accept_Communicator(MDI_Comm* comm);
DllExport int MDI_Send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
DllExport int MDI_Recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
DllExport int MDI_Send_Command(const char* buf, MDI_Comm comm);
DllExport int MDI_Recv_Command(char* buf, MDI_Comm comm);
DllExport int MDI_Conversion_Factor(const char* in_unit, const char* out_unit, double* conv);
DllExport int MDI_Get_Role(int* role);

// functions for managing Nodes, Commands, and Callbacks
DllExport int MDI_Register_Node(const char* node_name);
DllExport int MDI_Check_Node_Exists(const char* node_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NNodes(MDI_Comm comm, int* nnodes);
DllExport int MDI_Get_Node(int index, MDI_Comm comm, char* name);
DllExport int MDI_Register_Command(const char* node_name, const char* command_name);
DllExport int MDI_Check_Command_Exists(const char* node_name, const char* command_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NCommands(const char* node_name, MDI_Comm comm, int* ncommands);
DllExport int MDI_Get_Command(const char* node_name, int index, MDI_Comm comm, char* name);
DllExport int MDI_Register_Callback(const char* node_name, const char* callback_name);
DllExport int MDI_Check_Callback_Exists(const char* node_name, const char* callback_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NCallbacks(const char* node_name, MDI_Comm comm, int* ncallbacks);
DllExport int MDI_Get_Callback(const char* node_name, int index, MDI_Comm comm, char* name);

// functions for managing callback functions (used only with the LIBRARY communication method)
DllExport int MDI_Set_Execute_Command_Func(int (*generic_command)(const char*, MDI_Comm, void*), void* class_object);

// functions for managing callback functions for mpi4py
DllExport int MDI_Set_Mpi4py_Recv_Callback(int (*mpi4py_recv)(void*, int, int, int, MDI_Comm));
DllExport int MDI_Set_Mpi4py_Send_Callback(int (*mpi4py_send)(void*, int, int, int, MDI_Comm));
DllExport int MDI_Set_Mpi4py_Gather_Names_Callback(int (*mpi4py_gather_names)(void*, void*));
DllExport int MDI_Set_Mpi4py_Split_Callback(int (*mpi4py_split)(int, int, MDI_Comm, int));
DllExport int MDI_Set_Mpi4py_Rank_Callback(int (*mpi4py_rank)(int));
DllExport int MDI_Set_Mpi4py_Size_Callback(int (*mpi4py_size)(int));
DllExport int MDI_Set_Mpi4py_Barrier_Callback(int (*mpi4py_barrier)(int));

// only used internally by MDI
DllExport void mdi_error(const char* message);
DllExport void MDI_Set_World_Size(int world_size_in);
DllExport void MDI_Set_World_Rank(int world_rank_in);
DllExport int MDI_Get_Current_Code();

#ifdef __cplusplus
}
#endif

#endif
