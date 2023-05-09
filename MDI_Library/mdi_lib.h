/*! \file
 *
 * \brief Implementation of library-based communication
 */

#ifndef MDI_LIBRARY_IMPL
#define MDI_LIBRARY_IMPL

#include "mdi.h"
#include "mdi_global.h"

typedef int (*MDI_Plugin_init_t)( void* shared_state );

/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!! ABI WARNING !!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Changing this structure will affect the plugin ABI

*/
typedef struct plugin_shared_state_struct {
  /*! \brief Name of the next command to be executed on this code.
  This is only used by engines. */
  char command[MDI_COMMAND_LENGTH_];
  /*! \brief For future-proofing, this is a pointer to any extensions to this structure */
  void** ext;
  /*! \brief Version number of the driver */
  int driver_version[3];
  /*! \brief Version number of the engine */
  int engine_version[3];
  /*! \brief Buffer used for communication of data */
  void* buf;
  /*! \brief Argument vector for plugin command-line options */
  char** plugin_argv;
  /*! \brief Command-line options for currently running plugin */
  char* plugin_options;
  /*! \brief Unedited command-line options for currently running plugin */
  char* plugin_unedited_options;
  /*! \brief Pointer to the intra-communicator for the plugin */
  void* mpi_comm_ptr;
  /*! \brief Pointer to the class object that is used for the driver_node_callback function */
  void* driver_callback_obj;
  /*! \brief Pointer to the class object that is passed to any call to execute_command */
  void* execute_command_obj;
  /*! \brief Pointer to the original Python interpreter's dictionary.
   * Only used for Python plugins */
  void* python_interpreter_dict;
  /*! \brief Pointer to the driver's codes vector */
  void* driver_codes_ptr;
  /*! \brief Pointer to the engine's codes vector */
  void* engine_codes_ptr;
  /*! \brief Pointer to the engine's nodes vector */
  void* engine_nodes;
  /*! \brief Function pointer to the driver's library_activate_code function */
  int (*driver_activate_code)(void*, int);
  /*! \brief Function pointer to the engine's library_activate_code function */
  int (*engine_activate_code)(void*, int);
  /*! \brief Function pointer to the library execute command function */
  int (*lib_execute_command)(MDI_Comm);
  /*! \brief Function pointer to a wrapper for the language-specific execute command function */
  int (*execute_command_wrapper)(const char*, MDI_Comm_Type, void*);
  /*! \brief Function pointer to the engine's function for responding to built-in commands */
  int (*execute_builtin)(const char*, MDI_Comm_Type, int*);
  /*! \brief Function pointer to the engine's function to delete everything */
  int (*delete_engine)(size_t);
  /*! \brief Engine-side ID of the engine code */
  size_t engine_code_id;
  /*! \brief Driver-side ID of the driver code */
  size_t driver_code_id;
  /*! \brief Pointer to engine's execute_command function */
  MDI_execute_command_type execute_command;
  /*! \brief Function pointer to the driver node's callback function */
  MDI_Driver_node_callback_t driver_node_callback;
  /*! \brief Driver-side MDI communicator */
  MDI_Comm driver_mdi_comm;
  /*! \brief Engine-side MDI communicator */
  MDI_Comm engine_mdi_comm;
  /*! \brief Flag whether buf is allocated */
  int buf_allocated;
  /*! \brief Argument count for plugin command-line options */
  int plugin_argc;
  /*! \brief Flag whether plugin_argv is allocted for this code */
  int plugin_argv_allocated;
  /*! \brief MPI rank of this process within the plugin  */
  int intra_rank;
  /*! \brief Flag whether the engine is a Python code */
  int engine_language;
  /*! \brief Flag whether the Python interpreter has been initialized */
  int python_interpreter_initialized;
} plugin_shared_state;

typedef struct library_data_struct {
  /*! \brief State shared between the driver and the plugin */
  plugin_shared_state* shared_state;
  /*! \brief Handle of the code to which this communicator connects */
  int connected_code;
  /*! \brief Flag whether the next MDI_Send call should trigger execution of the engine's command */
  int execute_on_send;
  /*! \brief MPI intra-communicator for the engine */
  MPI_Comm mpi_comm;
  /*! \brief Pointer to the class object that is used for the driver_node_callback function */
  void* driver_callback_obj;
  /*! \brief Function pointer to the driver node's callback function */
  MDI_Driver_node_callback_t driver_node_callback;
#ifdef _WIN32
  /*! \brief Windows handle for the plugin library */
  HINSTANCE plugin_handle;
#else
  /*! \brief Non-windows handle for the plugin library */
  void* plugin_handle;
#endif
  /*! \brief Initialization function for the plugin library */
  MDI_Plugin_init_t plugin_init;
  /*! \brief Flag whether this communicator connects to a Python library */
  int is_python;
} library_data;

int enable_plug_support(int code_id);
int plug_on_selection();
int plug_on_accept_communicator();
int plug_on_send_command(const char* command, MDI_Comm comm, int* skip_flag);
int plug_after_send_command(const char* command, MDI_Comm comm);
int plug_on_recv_command(MDI_Comm comm);

int library_load_init(const char* plugin_name, void* mpi_comm_ptr,
                      library_data* libd, int mode);
int library_parse_options(const char* options, library_data* libd);
int library_launch_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                          MDI_Driver_node_callback_t driver_node_callback,
                          void* driver_callback_object);
int library_open_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                          MDI_Comm* mdi_comm_ptr);
int library_close_plugin(MDI_Comm mdi_comm);
int library_initialize();
int library_set_driver_current(MDI_Comm comm);
int library_set_command(const char* command, MDI_Comm comm);
int library_execute_command(MDI_Comm comm);
int library_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int library_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int library_send_msg(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int library_recv_msg(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

int library_set_state(void* state);
int library_activate_code(void* codes_in, int code_id);
int library_activate_driver(library_data* libd);
int library_activate_engine(library_data* libd);

int communicator_delete_lib(void* comm);
int library_delete_engine(size_t code_id);

#endif
