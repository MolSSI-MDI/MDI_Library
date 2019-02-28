/*! \file
 *
 * \brief Class definition for handling communication between connect codes
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mdi.h"
#include "communicator.h"

using namespace MDI_STUBS;
using namespace std;

vector <Communicator*> communicators;

Communicator::Communicator(int type_) {
  this->type = type_;

  communicators.push_back(this);
}


int CommunicatorMPI::send(const void* buf, int count, MDI_Datatype datatype) {

  if (datatype == MDI_INT) {
    MPI_Send(buf, count, MPI_INT, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_DOUBLE) {
    MPI_Send(buf, count, MPI_DOUBLE, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_CHAR) {
    MPI_Send(buf, count, MPI_CHAR, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else {
    perror("MDI data type not recognized in MDI_Send");
    exit(-1);
  }

  return 0;
}


int CommunicatorMPI::recv(void* buf, int count, MDI_Datatype datatype) {

  if (datatype == MDI_INT) {
    MPI_Recv(buf, count, MPI_INT, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else if (datatype == MDI_DOUBLE) {
    MPI_Recv(buf, count, MPI_DOUBLE, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else if (datatype == MDI_CHAR) {
    MPI_Recv(buf, count, MPI_CHAR, (this->mpi_rank+1)%2, 0, this->mpi_comm, MPI_STATUS_IGNORE);
  }
  else {
    perror("MDI data type not recognized in MDI_Recv");
    exit(-1);
  }

  return 0;
}


int CommunicatorTCP::send(const void* buf, int count, MDI_Datatype datatype) {

   int n;

   // determine the byte size of the data type being sent
   int datasize;
   if (datatype == MDI_INT) {
     datasize = sizeof(int);
   }
   else if (datatype == MDI_DOUBLE) {
     datasize = sizeof(double);
   }
   else if (datatype == MDI_CHAR) {
     datasize = sizeof(char);
   }
   else {
     perror("MDI data type not recognized in MDI_Send");
     exit(-1);
   }

   n = write(this->sockfd,buf,count*datasize);
   if (n < 0) { perror("Error writing to socket: server has quit or connection broke"); exit(-1); }

   return 0;
}


int CommunicatorTCP::recv(void* buf, int count, MDI_Datatype datatype) {
   int n, nr;
   char* buf_char = static_cast<char*>(buf);

   // determine the byte size of the data type being sent
   int datasize;
   if (datatype == MDI_INT) {
     datasize = sizeof(int);
   }
   else if (datatype == MDI_DOUBLE) {
     datasize = sizeof(double);
   }
   else if (datatype == MDI_CHAR) {
     datasize = sizeof(char);
   }
   else {
     perror("MDI data type not recognized in MDI_Recv");
     exit(-1);
   }

   n = nr = read(this->sockfd,buf_char,count*datasize);

   while (nr>0 && n<count*datasize )
     {  nr=read(this->sockfd,buf_char+n,count-n); n+=nr; }

   if (n == 0) { perror("Error reading from socket: server has quit or connection broke"); exit(-1); }

   return 0;
}
