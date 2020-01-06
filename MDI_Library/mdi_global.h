/*! \file
 *
 * \brief Global structures used by MDI
 */

#ifndef MDI_GLOBAL
#define MDI_GLOBAL

#include <mpi.h>

#define COMMAND_LENGTH 12
#define NAME_LENGTH 12
typedef int MDI_Comm_Type;

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
  /*! \brief Communication method used by this communicator (either MDI_TCP or MDI_MPI) */
  int method;
  /*! \brief MPI_Comm handle that corresponds to this communicator */
  MDI_Comm_Type id;
  /*! \brief Handle for the id of the associated code */
  int code_id;
  /*! \brief For communicators using the TCP communicatiom method, the socket descriptor */
  int sockfd;
  /*! \brief For communicators using the MPI communicatiom method, the inter-code MPI 
  communicator */
  MPI_Comm mpi_comm;
  /*! \brief For communicators using the MPI communication method, the rank of this 
  process within the inter-code MPI communicator */
  int mpi_rank;
  /*! \brief The MDI version of the connected code */
  int mdi_version[3];
  /*! \brief The nodes supported by the connected code */
  vector* nodes;
  /*! \brief Method-specific information for this communicator */
  void* method_data;
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
  /*! \brief Rank of this process within its associated code */
  int intra_rank;
  /*! \brief Vector containing all nodes supported by this code */
  vector* nodes;
  /*! \brief Vector containing all communicators associated with this code */
  vector* comms;
  /*! \brief Function pointer to the generic execute_command_function */
  int (*execute_command)(const char*, MDI_Comm_Type, void*);
  /*! \brief Pointer to the class object that is passed to any call to execute_command */
  void* execute_command_obj;
  /*! \brief Flag whether this code is Python */
  int is_python;
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

/*! \brief Dummy function for method-specific deletion operations for communicator deletion */
int communicator_delete(void* comm);

void mdi_error(const char* message);

#endif
