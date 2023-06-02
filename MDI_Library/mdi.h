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

typedef int (*MDI_Driver_node_callback_t)(void*, int, void*);
typedef int (*MDI_Driver_node_callback_f90_t)(void*, int, void*);
typedef int (*MDI_Execute_command_callback_t)(void*, MDI_Comm, void*);

// MDI version numbers
DllExport extern const int MDI_MAJOR_VERSION;
DllExport extern const int MDI_MINOR_VERSION;
DllExport extern const int MDI_PATCH_VERSION;

// length of an MDI command in characters
DllExport extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
DllExport extern const int MDI_NAME_LENGTH;

// length of an MDI label in characters
DllExport extern const int MDI_LABEL_LENGTH;

// value of a null communicator
DllExport extern const MDI_Comm MDI_COMM_NULL;

// MDI data types
DllExport extern const int MDI_INT;
DllExport extern const int MDI_INT8_T;
DllExport extern const int MDI_INT16_T;
DllExport extern const int MDI_INT32_T;
DllExport extern const int MDI_INT64_T;
DllExport extern const int MDI_UINT8_T;
DllExport extern const int MDI_UINT16_T;
DllExport extern const int MDI_UINT32_T;
DllExport extern const int MDI_UINT64_T;
DllExport extern const int MDI_DOUBLE;
DllExport extern const int MDI_CHAR;
DllExport extern const int MDI_FLOAT;
DllExport extern const int MDI_BYTE;

// MDI communication types
DllExport extern const int MDI_TCP;
DllExport extern const int MDI_MPI;
DllExport extern const int MDI_LINK;
DllExport extern const int MDI_PLUGIN;
DllExport extern const int MDI_TEST;

// MDI role types
DllExport extern const int MDI_DRIVER;
DllExport extern const int MDI_ENGINE;

// functions for handling MDI communication
DllExport int MDI_Init(int* argc, char ***argv);
DllExport int MDI_Initialized(int* flag);
DllExport int MDI_Accept_Communicator(MDI_Comm* comm);
DllExport int MDI_Accept_communicator(MDI_Comm* comm);
DllExport int MDI_Send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
DllExport int MDI_Recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
DllExport int MDI_Send_Command(const char* buf, MDI_Comm comm);
DllExport int MDI_Send_command(const char* buf, MDI_Comm comm);
DllExport int MDI_Recv_Command(char* buf, MDI_Comm comm);
DllExport int MDI_Recv_command(char* buf, MDI_Comm comm);
DllExport int MDI_Conversion_Factor(const char* in_unit, const char* out_unit, double* conv);
DllExport int MDI_Conversion_factor(const char* in_unit, const char* out_unit, double* conv);
DllExport int MDI_Get_Role(int* role);
DllExport int MDI_Get_role(int* role);
DllExport int MDI_Get_method(int* role, MDI_Comm comm);
DllExport int MDI_Get_communicator(MDI_Comm* comm, int index);
DllExport int MDI_String_to_atomic_number(const char* element_symbol, int* atomic_number);

// functions for managing Nodes, Commands, and Callbacks
DllExport int MDI_Register_Node(const char* node_name);
DllExport int MDI_Register_node(const char* node_name);
DllExport int MDI_Check_Node_Exists(const char* node_name, MDI_Comm comm, int* flag);
DllExport int MDI_Check_node_exists(const char* node_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NNodes(MDI_Comm comm, int* nnodes);
DllExport int MDI_Get_nnodes(MDI_Comm comm, int* nnodes);
DllExport int MDI_Get_Node(int index, MDI_Comm comm, char* name);
DllExport int MDI_Get_node(int index, MDI_Comm comm, char* name);
DllExport int MDI_Register_Command(const char* node_name, const char* command_name);
DllExport int MDI_Register_command(const char* node_name, const char* command_name);
DllExport int MDI_Check_Command_Exists(const char* node_name, const char* command_name, MDI_Comm comm, int* flag);
DllExport int MDI_Check_command_exists(const char* node_name, const char* command_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NCommands(const char* node_name, MDI_Comm comm, int* ncommands);
DllExport int MDI_Get_ncommands(const char* node_name, MDI_Comm comm, int* ncommands);
DllExport int MDI_Get_Command(const char* node_name, int index, MDI_Comm comm, char* name);
DllExport int MDI_Get_command(const char* node_name, int index, MDI_Comm comm, char* name);
DllExport int MDI_Register_Callback(const char* node_name, const char* callback_name);
DllExport int MDI_Register_callback(const char* node_name, const char* callback_name);
DllExport int MDI_Check_Callback_Exists(const char* node_name, const char* callback_name, MDI_Comm comm, int* flag);
DllExport int MDI_Check_callback_exists(const char* node_name, const char* callback_name, MDI_Comm comm, int* flag);
DllExport int MDI_Get_NCallbacks(const char* node_name, MDI_Comm comm, int* ncallbacks);
DllExport int MDI_Get_ncallbacks(const char* node_name, MDI_Comm comm, int* ncallbacks);
DllExport int MDI_Get_Callback(const char* node_name, int index, MDI_Comm comm, char* name);
DllExport int MDI_Get_callback(const char* node_name, int index, MDI_Comm comm, char* name);

// functions for handling MPI in combination with MDI
DllExport int MDI_MPI_get_world_comm(void* world_comm);
DllExport int MDI_MPI_set_world_comm(void* world_comm);

// functions for managing callback functions (used only with the LINK communication method)
DllExport int MDI_Launch_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                                MDI_Driver_node_callback_t driver_node_callback,
                                void* driver_callback_object);
DllExport int MDI_Open_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr, MDI_Comm* mdi_comm_ptr);
DllExport int MDI_Close_plugin(MDI_Comm mdi_comm);
DllExport int MDI_Set_Execute_Command_Func(int (*generic_command)(const char*, MDI_Comm, void*), void* class_object);
DllExport int MDI_Set_execute_command_func(int (*generic_command)(const char*, MDI_Comm, void*), void* class_object);
DllExport int MDI_Set_plugin_state(void* state);
DllExport int MDI_Set_plugin_state_internal(void* state);
DllExport int MDI_Plugin_get_argc(int* argc_ptr);
DllExport int MDI_Plugin_get_argv(char*** argv_ptr);
DllExport int MDI_Plugin_get_args(char** args_ptr);
DllExport int MDI_Plugin_get_arg(int index, char** arg_ptr);

// functions for managing callback functions for mpi4py
DllExport int MDI_Set_Mpi4py_Recv_Callback(int (*mpi4py_recv)(void*, int, int, int, MDI_Comm));
DllExport int MDI_Set_Mpi4py_Send_Callback(int (*mpi4py_send)(void*, int, int, int, MDI_Comm));
DllExport int MDI_Set_Mpi4py_Allgather_Callback(int (*mpi4py_allgather)(void*, void*));
DllExport int MDI_Set_Mpi4py_Gather_Names_Callback(int (*mpi4py_gather_names)(void*, void*, int*, int*));
DllExport int MDI_Set_Mpi4py_Split_Callback(int (*mpi4py_split)(int, int, MDI_Comm, int));
DllExport int MDI_Set_Mpi4py_Rank_Callback(int (*mpi4py_rank)(int));
DllExport int MDI_Set_Mpi4py_Size_Callback(int (*mpi4py_size)(int));
DllExport int MDI_Set_Mpi4py_Barrier_Callback(int (*mpi4py_barrier)(int));
DllExport int MDI_Set_Launch_Plugin_Callback(int (*launch_plugin)(void*, void*, void*, int));

// only used internally by MDI
DllExport int MDI_Init_code();
DllExport int MDI_Init_with_argv(int* argc, char ***argv);
DllExport int MDI_Init_with_options(const char *options);
DllExport void mdi_error(const char* message);
DllExport void MDI_Set_World_Size(int world_size_in);
DllExport void MDI_Set_World_Rank(int world_rank_in);
DllExport int MDI_Get_intra_rank(int intra_rank_out);
DllExport int MDI_Get_Current_Code();
DllExport int MDI_Get_python_plugin_mpi_world_ptr(void** python_plugin_mpi_world_ptr_ptr, void* state_in);
DllExport int MDI_Set_on_destroy_code(int (*func)(int));
DllExport int MDI_Set_plugin_language(int language, void* plugin_state);
DllExport int MDI_Set_language_execute_command(int (*execute_command)(void*, MDI_Comm, void*));
DllExport int MDI_Get_language_execute_command(MDI_Execute_command_callback_t* language_execute_command, MDI_Comm comm);
DllExport int MDI_Get_language_driver_callback(MDI_Driver_node_callback_f90_t* language_driver_callback);
DllExport int MDI_Set_language_driver_callback(MDI_Driver_node_callback_f90_t callback);

#ifdef __cplusplus
}
#endif

#endif
