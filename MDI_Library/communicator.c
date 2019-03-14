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
#include "communicator.h"

/*
typedef struct communicator_struct {
  int method; // the type of communicator
  int sockfd; // for TCP, the socket descriptor
  MPI_Comm mpi_comm;
  int mpi_rank;
} communicator;
*/

/*
typedef struct dynamic_array_struct {
  //void* data;
  unsigned char* data;
  size_t stride; //size of each element
  size_t capacity; //total number of elements that can be contained
  size_t size; //number of elements actually stored
} vector;
*/

int vector_init(vector* v, size_t stride) {
  //initialize the vector with the given stride
  v->data = malloc(0);
  if (!v->data) {
    perror("Could not initialize vector");
    exit(-1);
  }

  v->size = 0;
  v->capacity = 0;
  v->stride = stride;

  return 0;
}

int vector_push_back(vector* v, void* element) {
  //grow the vector
  if (v->size >= v->capacity) {
    int new_capacity;
    if ( v->capacity > 0 ) {
      new_capacity = 2 * v->capacity;
    }
    else {
      new_capacity = 1;
    }
    void* new_data = malloc( v->stride * new_capacity );
    memcpy(new_data, v->data, v->size * v->stride);
    free(v->data);
    v->data = new_data;
    v->capacity = new_capacity;
  }

  //add the new data to the vector
  memcpy( v->data + (v->size * v->stride), (unsigned char*)element, v->stride );
  v->size++;

  return 0;
}

void* vector_get(vector* v, int index) {
  if (index < 0 || index >= v->size) {
    mdi_error("Vector accessed out-of-bounds");
  }
  return ( void* )( v->data + (index * v->stride) );
}


vector communicators;








/*
vector <Communicator*> communicators;

Communicator::Communicator(int type_) {
  this->type = type_;

  communicators.push_back(this);
}
*/


int mpi_send(const void* buf, int count, MDI_Datatype datatype, communicator* this) {

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
    mdi_error("MDI data type not recognized in mpi_send");
  }

  return 0;
}


int mpi_recv(void* buf, int count, MDI_Datatype datatype, communicator* this) {

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


int tcp_send(const void* buf, int count, MDI_Datatype datatype, communicator* this) {
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
     mdi_error("MDI data type not recognized in tcp_send");
   }

   n = write(this->sockfd,buf,count*datasize);
   if (n < 0) { mdi_error("Error writing to socket: server has quit or connection broke"); }

   return 0;
}


int tcp_recv(const void* buf, int count, MDI_Datatype datatype, communicator* this) {
   int n, nr;
   char* buf_char = (char*)(buf);

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

   n = nr = read(this->sockfd,buf_char,count*datasize);

   while (nr>0 && n<count*datasize )
     {  nr=read(this->sockfd,buf_char+n,count-n); n+=nr; }

   if (n == 0) { mdi_error("Error reading from socket: server has quit or connection broke"); }

   return 0;
}


int communicator_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

  if ( this->method == MDI_MPI ) {
    mpi_send(buf, count, datatype, this);
  }
  else if ( this->method == MDI_TCP ) {
    tcp_send(buf, count, datatype, this);
  }
  else {
    mdi_error("MDI method not recognized in communicator_send");
  }

  return 0;
}


int communicator_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  communicator* this = vector_get(&communicators, comm-1);

  if ( this->method == MDI_MPI ) {
    mpi_recv(buf, count, datatype, this);
  }
  else if ( this->method == MDI_TCP ) {
    tcp_recv(buf, count, datatype, this);
  }
  else {
    mdi_error("MDI method not recognized in communicator_send");
  }

  return 0;
}
