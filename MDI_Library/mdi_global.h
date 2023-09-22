/*! \file
 *
 * \brief Global structures used by MDI
 */

#ifndef MDI_GLOBAL
#define MDI_GLOBAL

#include <mpi.h>

#ifdef _WIN32
  #include <winsock2.h>
  #define sock_t SOCKET
#else
  #define sock_t int
#endif

#ifdef _WIN32
  #define mdi_strdup _strdup
#else
  #define mdi_strdup strdup
#endif

// Hard-coded values
#define PLUGIN_PATH_LENGTH 2048

// Defined languages
#define MDI_LANGUAGE_C 1
#define MDI_LANGUAGE_FORTRAN 2
#define MDI_LANGUAGE_PYTHON 3

// MDI version numbers
#define MDI_MAJOR_VERSION_ 1
#define MDI_MINOR_VERSION_ 4
#define MDI_PATCH_VERSION_ 22

// length of an MDI command in characters
#define MDI_COMMAND_LENGTH_ 256

// length of an MDI name in characters
#define MDI_NAME_LENGTH_ 256

// length of an MDI label in characters
#define MDI_LABEL_LENGTH_ 64

// value of a null communicator
#define MDI_COMM_NULL_ 0

// MDI data types
#define MDI_INT_ 1
#define MDI_INT8_T_ 7
#define MDI_INT16_T_ 8
#define MDI_INT32_T_ 9
#define MDI_INT64_T_ 10
#define MDI_UINT8_T_ 11
#define MDI_UINT16_T_ 12
#define MDI_UINT32_T_ 13
#define MDI_UINT64_T_ 14
#define MDI_DOUBLE_ 2
#define MDI_CHAR_ 3
#define MDI_FLOAT_ 4
#define MDI_BYTE_ 6

// MDI communication types
#define MDI_TCP_ 1
#define MDI_MPI_ 2
#define MDI_LINK_ 3
#define MDI_TEST_ 4

// MDI role types
#define MDI_DRIVER_ 1
#define MDI_ENGINE_ 2

// MDI Typedefs
typedef int MDI_Comm_Type;
typedef int MDI_Datatype_Type;
typedef int (*MDI_execute_command_type)(void*, MDI_Comm_Type, void*);

/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!! ABI WARNING !!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Changing this structure will affect the plugin ABI

*/
typedef struct dynamic_array_struct {
  /*! \brief The elements stored by this vector */
  unsigned char* data;
  /*! \brief Size of each element */
  size_t stride;
  /*! \brief Total number of elements that can be stored by this vector */
  size_t capacity;
  /*! \brief Number of elements actually stored */
  size_t size; //number of elements actually stored
  /*! \brief If the array has a currently active component, this is that value */
  size_t current_key; //number of elements actually stored
  /*! \brief Flag whether the vector has been initialized */
  int initialized;
} vector;


/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!! ABI WARNING !!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Changing this structure will affect the plugin ABI

*/
typedef struct element_struct {
  /*! \brief The actual data for this element */
  void* data;
  /*! \brief ID of this element */
  size_t id;
} element;


typedef struct method_struct {
  /*! \brief Function pointer for method initialization work */
  int (*on_selection)();
  int (*on_accept_communicator)();
  int (*on_send_command)(const char*, MDI_Comm_Type, int* skip_flag);
  int (*after_send_command)(const char*, MDI_Comm_Type);
  int (*on_recv_command)(MDI_Comm_Type);
  /*! \brief ID of this method */
  int id;
  /*! \brief Communication method */
  int method_id;
} method;

typedef struct communicator_struct {
  /*! \brief The MDI version of the connected code */
  int mdi_version[3];
  /*! \brief The nodes supported by the connected code */
  vector* nodes;
  /*! \brief Method-specific information for this communicator */
  void* method_data;
  /*! \brief Function pointer for method-specific send operations */
  int (*send)(const void*, int, MDI_Datatype_Type, MDI_Comm_Type, int);
  /*! \brief Function pointer for method-specific receive operations */
  int (*recv)(void*, int, MDI_Datatype_Type, MDI_Comm_Type, int);
  /*! \brief Function pointer for method-specific deletion operations */
  int (*delete)(void*);
  /*! \brief Handle for the id of the associated code */
  size_t code_id;
  /*! \brief For communicators using the TCP communicatiom method, the socket descriptor (WINDOWS) */
  sock_t sockfd;
  /*! \brief Communication method used by this communicator */
  int method_id;
  /*! \brief MDI_Comm handle that corresponds to this communicator */
  MDI_Comm_Type id;
  /*! \brief Indicate whether this communicator has been accepted yet */
  int is_accepted;
  /*! \brief The value of MDI_NAME_LENGTH for the connected code */
  int name_length;
  /*! \brief The value of MDI_COMMAND_LENGTH for the connected code */
  int command_length;
} communicator;

typedef struct node_struct {
  /*! \brief Name of the node */
  char name[MDI_COMMAND_LENGTH_];
  /*! \brief Vector containing all the commands supported at this node */
  vector* commands;
  /*! \brief Vector containing all the callbacks associated with this node */
  vector* callbacks;
} node;

typedef struct code_struct {
  /*! \brief Name of the driver/engine */
  char name[MDI_NAME_LENGTH_];
  /*! \brief Role of the driver/engine */
  char role[MDI_NAME_LENGTH_];
  /*! \brief Pointer to the class object that is passed to any call to execute_command */
  void* execute_command_obj;
  /*! \brief Vector containing all nodes supported by this code */
  vector* nodes;
  /*! \brief Vector containing all communicators associated with this code */
  vector* comms;
  /*! \brief Vector containing all supported methods */
  vector* methods;
  /*! \brief Path to the plugins available to this code */
  char* plugin_path;
  /*! \brief Argument count for plugin command-line options */
  int* plugin_argc_ptr;
  /*! \brief Argument vector for plugin command-line options */
  char*** plugin_argv_ptr;
  /*! \brief Unedited command-line options for currently running plugin */
  char** plugin_unedited_options_ptr;
  /*! \brief Shared plugin state, received from the driver during plugin calculations */
  void* shared_state_from_driver;
  /*! \brief Hostname of the driver */
  char* hostname;
  /*! \brief Function pointer to the actual callback for Fortran drivers */
  int (*driver_callback_f90)(void*, int, void*);
  /*! \brief Function pointer to the language-specific wrapper for the execute_command function */
  int (*execute_command_wrapper)(const char*, MDI_Comm_Type, void*);
  /*! \brief Function pointer to the generic execute_command_function */
  MDI_execute_command_type execute_command;
  /*! \brief Function pointer to the language-specific destructor function */
  int (*language_on_destroy)(int);
  /*! \brief Python callback pointer for MPI_Recv */
  int (*mpi4py_recv_callback)(void*, int, int, int, MDI_Comm_Type);
  /*! \brief Python callback pointer for MPI_Send */
  int (*mpi4py_send_callback)(void*, int, int, int, MDI_Comm_Type);
  /*! \brief Python callback pointer for the initial MPI allgather */
  int (*mpi4py_allgather_callback)(void*, void*);
  /*! \brief Python callback pointer for gathering names */
  int (*mpi4py_gather_names_callback)(void*, void*, int*, int*);
  /*! \brief Python callback pointer for MPI_Comm_split */
  int (*mpi4py_split_callback)(int, int, MDI_Comm_Type, int);
  /*! \brief Python callback pointer for MPI_Comm_rank */
  int (*mpi4py_rank_callback)(int);
  /*! \brief Python callback pointer for MPI_Comm_size */
  int (*mpi4py_size_callback)(int);
  /*! \brief Python callback pointer for MPI_Comm_barrier */
  int (*mpi4py_barrier_callback)(int);
  /*! \brief Python callback pointer for MPI_Comm_barrier */
  int (*py_launch_plugin_callback)(void*, void*, void*, int);
  /*! \brief MPI intra-communicator that spans all ranks associated with this code */
  MPI_Comm intra_MPI_comm;
  /*! \brief Socket for TCP connections */
  sock_t tcp_socket;
  /*! \brief Flag whether this code is being used as a library
  0: Not a library
  1: Is an ENGINE library, but has not connected to the driver
  2: Is an ENGINE library that has connected to the driver */
  int is_library;
  /*! \brief Size of MPI_COMM_WORLD */
  int world_size;
  /*! \brief Rank of this process within MPI_COMM_WORLD */
  int world_rank;
  /*! \brief Handle for this code */
  size_t id;
  /*! \brief The number of communicator handles that have been returned by MDI_Accept_Connection() */
  int returned_comms;
  /*! \brief The handle of the next communicator */
  int next_comm;
  /*! \brief Native language of this code */
  int language;
  /*! \brief Rank of this process within its associated code */
  int intra_rank;
  /*! \brief Flag whether this code has called set_execute_command_func */
  int called_set_execute_command_func;
  /*! \brief ID of the method being used for inter-code communication */
  int selected_method_id;
  /*! \brief Flag for whether MDI is running in i-PI compatibility mode */
  int ipi_compatibility;
  /*! \brief Flag for whether MDI called MPI_Init */
  int initialized_mpi;
  /*! \brief Flag whether this code has previously initialized TCP */
  int tcp_initialized;
  /*! \brief Flag whether this code has previously initialized MPI */
  int mpi_initialized;
  /*! \brief Flag whether this code has previously initialized TEST */
  int test_initialized;
  /*! \brief Port over which the driver will listen */
  int port;
  /*! \brief Flag for whether this code is running in debug mode */
  int debug_mode;
} code;



/*! \brief Vector containing all codes that have been initiailized on this rank. Typically, 
this will only include a single code, unless the communication method is LINK.
ALL global MDI data is stored within this vector */
extern vector codes;



int vector_init(vector* v, size_t stride);
int vector_push_back(vector* v, void* element);
int vector_get(vector* v, int index, void** element);
int vector_delete(vector* v, int index);
int vector_free(vector* v);

int get_node_index(vector* v, const char* node_name, int* node_index);
int get_command_index(node* n, const char* command_name, int* command_index);
int get_callback_index(node* n, const char* callback_name, int* callback_index);
int free_node_vector(vector* v);

int new_communicator(size_t code_id, int method, MDI_Comm_Type* comm_id_ptr);
int get_communicator(size_t code_id, MDI_Comm_Type comm_id, communicator** comm_ptr);
int delete_communicator(size_t code_id, MDI_Comm_Type comm_id);

int new_code(size_t* code_id);
int get_code(size_t code_id, code** ret_code);
int delete_code(size_t code_id);
int get_current_code(code** this_code_ptr);

int new_method(size_t code_id, int method_id, int* id_ptr);
int get_method(size_t code_id, int method_id, method** method_ptr);
int delete_method(size_t code_id, int method_id);
int free_methods_vector(vector* v);

/*! \brief Check whether a file exists */
int file_exists(const char* file_name, int* flag);

/*! \brief Dummy function for method-specific deletion operations for communicator deletion */
int communicator_delete(void* comm);

void mdi_error(const char* message);
void mdi_warning(const char* message);
int mdi_debug(const char* message, ...);

/*! \brief Get information about an MDI_Datatype */
int datatype_info(MDI_Datatype_Type datatype, size_t* size, MDI_Datatype_Type* base);

/*! \brief Get the MPI datatype that corresponds to an MDI datatype */
int datatype_mpitype(MDI_Datatype_Type datatype, MPI_Datatype* mpitype);

/*! \brief Convert a buffer from one datatype to another */
int convert_buf_datatype(void* recvbuf_in, MDI_Datatype_Type recvtype,
			 void* sendbuf_in, MDI_Datatype_Type sendtype,
			 int count);

#endif
