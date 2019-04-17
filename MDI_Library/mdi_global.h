/*! \file
 *
 * \brief Global structures used by MDI
 */

#ifndef MDI_GLOBAL
#define MDI_GLOBAL

#include <mpi.h>

typedef struct communicator_struct {
  int method; // the type of communicator
  int sockfd; // for TCP, the socket descriptor
  MPI_Comm mpi_comm;
  int mpi_rank;
  double mdi_version;
} communicator;

typedef struct dynamic_array_struct {
  unsigned char* data;
  size_t stride; //size of each element
  size_t capacity; //total number of elements that can be contained
  size_t size; //number of elements actually stored
} vector;

extern vector communicators;

int vector_init(vector* v, size_t stride);
int vector_push_back(vector* v, void* element);
void* vector_get(vector* v, int index);

#endif
