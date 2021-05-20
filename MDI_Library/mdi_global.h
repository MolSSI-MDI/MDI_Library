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
#define COMMAND_LENGTH 12
#define NAME_LENGTH 12
#define PLUGIN_PATH_LENGTH 2048

// Defined languages
#define MDI_LANGUAGE_C 1
#define MDI_LANGUAGE_FORTRAN 2
#define MDI_LANGUAGE_PYTHON 3

// MDI version numbers
#define MDI_MAJOR_VERSION_ 1
#define MDI_MINOR_VERSION_ 2
#define MDI_PATCH_VERSION_ 10

// length of an MDI command in characters
#define MDI_COMMAND_LENGTH_ 12

// length of an MDI name in characters
#define MDI_NAME_LENGTH_ 12

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

typedef struct dynamic_array_struct {
  /*! \brief The elements stored by this vector */
  unsigned char* data;
  /*! \brief Size of each element */
  size_t stride;
  /*! \brief Total number of elements that can be stored by this vector */
  size_t capacity;
  /*! \brief Number of elements actually stored */
  size_t size; //number of elements actually stored
} vector;

typedef struct communicator_struct {
  /*! \brief Communication method used by this communicator */
  int method;
  /*! \brief MDI_Comm handle that corresponds to this communicator */
  MDI_Comm_Type id;
  /*! \brief Handle for the id of the associated code */
  int code_id;
  /*! \brief For communicators using the TCP communicatiom method, the socket descriptor (WINDOWS) */
  sock_t sockfd;
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
} communicator;

typedef struct node_struct {
  /*! \brief Name of the node */
  char name[COMMAND_LENGTH];
  /*! \brief Vector containing all the commands supported at this node */
  vector* commands;
  /*! \brief Vector containing all the callbacks associated with this node */
  vector* callbacks;
} node;

typedef struct code_struct {
  /*! \brief Name of the driver/engine */
  char name[NAME_LENGTH];
  /*! \brief Role of the driver/engine */
  char role[NAME_LENGTH];
  /*! \brief Handle for this code */
  int id;
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
  /*! \brief MPI intra-communicator that spans all ranks associated with this code */
  MPI_Comm intra_MPI_comm;
  /*! \brief Vector containing all nodes supported by this code */
  vector* nodes;
  /*! \brief Vector containing all communicators associated with this code */
  vector* comms;
  /*! \brief Path to the plugins available to this code */
  char* plugin_path;
  /*! \brief Function pointer to the generic execute_command_function */
  int (*execute_command)(const char*, MDI_Comm_Type, void*);
  /*! \brief Pointer to the class object that is passed to any call to execute_command */
  void* execute_command_obj;
  /*! \brief Flag whether this code is being used as a library
  0: Not a library
  1: Is an ENGINE library, but has not connected to the driver
  2: Is an ENGINE library that has connected to the driver */
  int is_library;
} code;

/*! \brief Vector containing all codes that have been initiailized on this rank Typically, 
this will only include a single code, unless the communication method is LIBRARY */
extern vector codes;

/*! \brief Index of the active code */
extern int current_code;

/*! \brief Flag for whether MDI is running in i-PI compatibility mode */
extern int ipi_compatibility;

/*! \brief Flag for whether MDI has been previously initialized */
extern int is_initialized;

/*! \brief Flag for whether MDI called MPI_Init */
extern int initialized_mpi;

/*! \brief Flag for whether MDI is currently operating in plugin mode */
extern int plugin_mode;

/*! \brief Internal copy of MPI_COMM_WORLD, used when MDI initializes MPI */
extern MPI_Comm mdi_mpi_comm_world;

/*! \brief Pointer to the MPI_Comm over which a Python plugin should run.
 * Only used for Python plugins */
extern void* python_plugin_mpi_world_ptr;

/*! \brief Unedited command-line options for currently running plugin */
extern char* plugin_unedited_options;

/*! \brief Command-line options for currently running plugin */
extern char* plugin_options;

/*! \brief Argument count for plugin command-line options */
extern int plugin_argc;

/*! \brief Argument vector for plugin command-line options */
extern char** plugin_argv;

/*! \brief Python callback pointer for MPI_Recv */
extern int (*mpi4py_recv_callback)(void*, int, int, int, MDI_Comm_Type);

/*! \brief Python callback pointer for MPI_Send */
extern int (*mpi4py_send_callback)(void*, int, int, int, MDI_Comm_Type);

/*! \brief Python callback pointer for gathering names */
extern int (*mpi4py_gather_names_callback)(void*, void*);

/*! \brief Python callback pointer for MPI_Comm_split */
extern int (*mpi4py_split_callback)(int, int, MDI_Comm_Type, int);

/*! \brief Python callback pointer for MPI_Comm_rank */
extern int (*mpi4py_rank_callback)(int);

/*! \brief Python callback pointer for MPI_Comm_size */
extern int (*mpi4py_size_callback)(int);

/*! \brief Python callback pointer for MPI_Comm_barrier */
extern int (*mpi4py_barrier_callback)(int);

/*! \brief Size of MPI_COMM_WORLD */
extern int world_size;

/*! \brief Rank of this process within MPI_COMM_WORLD */
extern int world_rank;

int vector_init(vector* v, size_t stride);
int vector_push_back(vector* v, void* element);
void* vector_get(vector* v, int index);
int vector_delete(vector* v, int index);
int vector_free(vector* v);

int get_node_index(vector* v, const char* node_name);
int get_command_index(node* n, const char* command_name);
int get_callback_index(node* n, const char* callback_name);
int free_node_vector(vector* v);

int new_communicator(int code_id, int method);
communicator* get_communicator(int code_id, MDI_Comm_Type comm_id);
int delete_communicator(int code_id, MDI_Comm_Type comm_id);

int new_code();
code* get_code(int code_id);
int delete_code(int code_id);

/*! \brief Check whether a file exists */
int file_exists(const char* file_name);

/*! \brief Dummy function for method-specific deletion operations for communicator deletion */
int communicator_delete(void* comm);

void mdi_error(const char* message);
void mdi_warning(const char* message);

/*! \brief Get information about an MDI_Datatype */
int datatype_info(MDI_Datatype_Type datatype, size_t* size, MDI_Datatype_Type* base);

/*! \brief Get the MPI datatype that corresponds to an MDI datatype */
int datatype_mpitype(MDI_Datatype_Type datatype, MPI_Datatype* mpitype);

/*! \brief Convert a buffer from one datatype to another */
int convert_buf_datatype(void* recvbuf_in, MDI_Datatype_Type recvtype,
			 void* sendbuf_in, MDI_Datatype_Type sendtype,
			 int count);

#endif
