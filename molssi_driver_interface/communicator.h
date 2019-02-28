/*! \file
 *
 * \brief Class declaration for handling communication between connected codes
 */

#ifndef MDI_COMMUNICATOR
#define MDI_COMMUNICATOR

#include <vector>
#include "mdi.h"

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

#endif
