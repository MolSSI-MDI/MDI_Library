/*! \file
 *
 * \brief Global structures used by MDI
 */

#ifndef MDI_GLOBAL
#define MDI_GLOBAL

#include <mpi.h>

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
} communicator;

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

extern vector communicators;

int vector_init(vector* v, size_t stride);
int vector_push_back(vector* v, void* element);
void* vector_get(vector* v, int index);

void mdi_error(const char* message);

#endif
