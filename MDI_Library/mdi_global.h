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
  /*! \brief For communicators using the TCP communicatiom method, the socket descriptor */
  int sockfd;
  /*! \brief For communicators using the MPI communicatiom method, the inter-code MPI 
  communicator */
  MPI_Comm mpi_comm;
  /*! \brief For communicators using the MPI communicatiom method, the rank of this 
  process within the inter-code MPI communicator */
  int mpi_rank;
  /*! \brief The MDI version of the connected code */
  double mdi_version;
  /*! \brief The nodes supported by the connected code */
  vector* nodes;
  /*! \brief Method-specific information for this communicator */
  void* method_data;
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
  /*! \brief The number of communicator handles that have been returned by MDI_Accept_Connection() */
  int returned_comms;
  /*! \brief Vector containing all nodes supported by this code */
  vector* nodes;
  /*! \brief Vector containing all communicators associated with this code */
  vector* comms;
  /*! \brief Function pointer to the generic execute_command_function */
  int (*execute_command)(const char*, MDI_Comm_Type);
  /*! \brief Flag whether this code is being used as a library */
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

int vector_init(vector* v, size_t stride);
int vector_push_back(vector* v, void* element);
void* vector_get(vector* v, int index);

int get_node_index(vector* v, const char* node_name);
int get_command_index(node* n, const char* command_name);
int get_callback_index(node* n, const char* callback_name);

int new_code();

void mdi_error(const char* message);

#endif
