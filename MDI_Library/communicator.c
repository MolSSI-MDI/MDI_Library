/*! \file
 *
 * \brief Class definition for handling communication between connect codes
 */

#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "mdi.h"
#include "mdi_global.h"
#include "communicator.h"

int mpi_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

  if (datatype == MDI_INT) {
    MPI_Send((void*)buf, count, MPI_INT, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_DOUBLE) {
    MPI_Send((void*)buf, count, MPI_DOUBLE, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else if (datatype == MDI_CHAR) {
    MPI_Send((void*)buf, count, MPI_CHAR, (this->mpi_rank+1)%2, 0, this->mpi_comm);
  }
  else {
    mdi_error("MDI data type not recognized in mpi_send");
  }

  return 0;
}


int mpi_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

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
    mdi_error("MDI data type not recognized in mpi_recv");
  }

  return 0;
}


int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
   int n;
   communicator* this = vector_get(&communicators, comm-1);

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
     mdi_error("MDI data type not recognized in tcp_send");
   }

   n = write(this->sockfd,buf,count*datasize);
   if (n < 0) { mdi_error("Error writing to socket: server has quit or connection broke"); }

   return 0;
}


int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
   int n, nr;
   communicator* this = vector_get(&communicators, comm-1);
   //char* buf_char = (char*)(buf);

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
     perror("MDI data type not recognized in tcp_recv");
   }

   n = nr = read(this->sockfd,buf,count*datasize);

   while (nr>0 && n<count*datasize )
     {  nr=read(this->sockfd,buf+n,count-n); n+=nr; }

   if (n == 0) { mdi_error("Error reading from socket: server has quit or connection broke"); }

   return 0;
}


int communicator_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

  if ( this->method == MDI_MPI ) {
    mpi_send(buf, count, datatype, comm);
  }
  else if ( this->method == MDI_TCP ) {
    tcp_send(buf, count, datatype, comm);
  }
  else {
    mdi_error("MDI method not recognized in communicator_send");
  }

  return 0;
}


int communicator_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

  if ( this->method == MDI_MPI ) {
    mpi_recv(buf, count, datatype, comm);
  }
  else if ( this->method == MDI_TCP ) {
    tcp_recv(buf, count, datatype, comm);
  }
  else {
    mdi_error("MDI method not recognized in communicator_send");
  }

  return 0;
}
