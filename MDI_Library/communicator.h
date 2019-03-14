/*! \file
 *
 * \brief Class declaration for handling communication between connected codes
 */

#ifndef MDI_COMMUNICATOR
#define MDI_COMMUNICATOR

#include <mpi.h>
#include "mdi.h"

typedef struct communicator_struct {
  int method; // the type of communicator
  int sockfd; // for TCP, the socket descriptor
  MPI_Comm mpi_comm;
  int mpi_rank;
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

int communicator_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int communicator_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);

/*
class Communicator
{
  public:
    Communicator(int type_);
    virtual int send(const void* buf, int count, MDI_Datatype datatype) = 0;
    virtual int recv(void* buf, int count, MDI_Datatype datatype) = 0;

  private:
    int type;
};

class CommunicatorMPI : public Communicator
{
  public:
    CommunicatorMPI(int type_, int mpi_comm_, int mpi_rank_)
      : Communicator(type_) { 
      this->mpi_comm = mpi_comm_;
      this->mpi_rank = mpi_rank_;
    };
    int send(const void* buf, int count, MDI_Datatype datatype);
    int recv(void* buf, int count, MDI_Datatype datatype);

  private:
    int mpi_comm;
    int mpi_rank;
};


class CommunicatorTCP : public Communicator
{
  public:
    CommunicatorTCP(int type_, int sockfd_)
      : Communicator(type_) { 
      this->sockfd = sockfd_;
    };
    int send(const void* buf, int count, MDI_Datatype datatype);
    int recv(void* buf, int count, MDI_Datatype datatype);

  private:
    int sockfd;
};

extern std::vector <Communicator*> communicators;
*/

#endif
