/*! \file
 *
 * \brief Implementation of library-based communication
 */

#ifndef MDI_LIBRARY_IMPL
#define MDI_LIBRARY_IMPL

#include "mdi.h"
#include "mdi_global.h"

typedef int (*MDI_Plugin_init_t)();

typedef struct library_data_struct {
  /*! \brief Handle of the code to which this communicator connects */
  int connected_code;
  /*! \brief Name of the next command to be executed on this code.
  This is only used by engines. */
  char command[MDI_COMMAND_LENGTH_];
  /*! \brief Flag whether buf is allocated */
  int buf_allocated;
  /*! \brief Flag whether the next MDI_Send call should trigger execution of the engine's command */
  int execute_on_send;
  /*! \brief MPI intra-communicator for the engine */
  MPI_Comm mpi_comm;
  /*! \brief Pointer to the class object that is used for the driver_node_callback function */
  void* driver_callback_obj;
  /*! \brief Function pointer to the driver node's callback function */
  MDI_Driver_node_callback_t driver_node_callback;
  /*! \brief Buffer used for communication of data */
  void* buf;
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
  /*! \brief Argument count for plugin command-line options */
  int plugin_argc;
  /*! \brief Argument vector for plugin command-line options */
  char** plugin_argv;
  /*! \brief Flag whether plugin_argv is allocted for this code */
  int plugin_argv_allocated;
} library_data;

int enable_plug_support();
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
int library_accept_communicator();
int library_set_driver_current();
int library_get_matching_handle(MDI_Comm comm);
int library_set_command(const char* command, MDI_Comm comm);
int library_execute_command(MDI_Comm comm);
int library_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int library_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag);
int library_send_msg(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int library_recv_msg(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

int communicator_delete_lib(void* comm);

#endif
