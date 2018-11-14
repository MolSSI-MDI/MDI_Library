/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init_MPI: Initializes MPI and creates the communicators
   MDI_Init: Initializes a TCP socket and sets it to listen
   MDI_Open: Opens a socket and requests a connection with a specified host
   MDI_Accept_Connection: Accepts an incoming connection request
   MDI_Send: Sends data through the socket
   MDI_Recv: Receives data through the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH through the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH through the
      socket
*/

#include <signal.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <mpi.h>
#include "mdi.h"

// length of an MDI command in characters
const int MDI_COMMAND_LENGTH = 12;

// length of an MDI name in characters
#define MDI_NAME_LENGTH_INTERNAL 12
const int MDI_NAME_LENGTH = MDI_NAME_LENGTH_INTERNAL;

// MDI data types
const int MDI_INT    = 0;
const int MDI_DOUBLE = 1;
const int MDI_CHAR   = 2;

// MDI communication types
const int MDI_TCP    = 1;
const int MDI_MPI    = 2;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
const double MDI_METER_TO_BOHR = 1.88972612546e10;
const double MDI_ANGSTROM_TO_BOHR = 1.88972612546;

// time
const double MDI_SECOND_TO_AUT = 4.1341374575751e16;
const double MDI_PICOSECOND_TO_AUT = 4.1341374575751e4;

// force
const double MDI_NEWTON_TO_AUF = 1.213780478e7;

// energy
const double MDI_JOULE_TO_HARTREE = 2.29371265835792e17;
const double MDI_KJ_TO_HARTREE = 2.29371265835792e20;
const double MDI_KJPERMOL_TO_HARTREE = 3.80879947807451e-4;
const double MDI_KCALPERMOL_TO_HARTREE = 1.5941730215480900e-3;
const double MDI_EV_TO_HARTREE = 3.67493266806491e-2;
const double MDI_RYDBERG_TO_HARTREE = 0.5;
const double MDI_KELVIN_TO_HARTREE = 3.16681050847798e-6;

// has any MDI_Init_X function been called?
int any_initialization = 0;

// the TCP socket, initialized by MDI_Init
int tcp_socket = -1;

// the MPI rank, initialized by MDI_Init_MPI
int world_rank = -1;

typedef struct communicator_struct {
  int type; // the type of communicator
  int handle; // for TCP, the socket descriptor
              // for MPI, the MPI communicator
  char name[MDI_NAME_LENGTH_INTERNAL]; // the name of the connected program
} communicator;

typedef struct dynamic_array_struct {
  //void* data;
  unsigned char* data;
  size_t stride; //size of each element
  size_t capacity; //total number of elements that can be contained
  size_t size; //number of elements actually stored
} vector;

//this is the number of communicator handles that have been returned by MDI_Accept_Connection()
int returned_comms = 0;


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
    perror("Vector accessed out-of-bounds");
    exit(-1);
  }
  return ( void* )( v->data + (index * v->stride) );
}

vector comms;


/*----------------------------*/
/* Signal handler definitions */
/*----------------------------*/

int driver_sockfd;
void sigint_handler(int dummy) {
  close(driver_sockfd);
}


int gather_names(const char* hostname_ptr){
  int i, icomm;

   // get the number of processes
   int world_size;
   MPI_Comm_size(MPI_COMM_WORLD, &world_size);

   // get the rank of this process
   world_rank;
   MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

   //create the name of this process
   char buffer[MDI_NAME_LENGTH];
   int str_end;
   strcpy(buffer, hostname_ptr);
   /*
   strcpy(buffer, hostname_ptr);
   str_end = 0;
   for (i=0; i<MDI_NAME_LENGTH; i++) {
     if( buffer[i] == '\0' ) {
       str_end = 1;
     }
     if( str_end == 1 ) {
       buffer[i] = ' ';
     }
   }
   */

   char *names = NULL;
   if (world_rank == 0) {
     names = malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);
   }

   MPI_Gather(&buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
              MPI_CHAR, 0, MPI_COMM_WORLD);

   if (world_rank == 0) {
     for (i=0; i<world_size; i++) {
       char *ptr1 = &names[i*MDI_NAME_LENGTH];
     }
   }

   if (world_rank == 0) {

     //create communicators
     for (i=0; i<world_size; i++) {
       char name[MDI_NAME_LENGTH];
       memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

       int found = 0;
       for (icomm=0; icomm<comms.size; icomm++) {
	 communicator* comm = vector_get(&comms,icomm);
	 if ( strcmp(name, comm->name) == 0 ) {
	   found = 1;
	 }
       }

       if ( found == 0 && strcmp(name,"") != 0 ) {
	 communicator new_comm;
	 new_comm.type = MDI_MPI;
	 new_comm.handle = MPI_COMM_WORLD;
	 memcpy( new_comm.name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

	 vector_push_back( &comms, &new_comm );
       }
     }

     //print the communicators
     for (i=0; i<comms.size; i++) {
       communicator* pcomm = vector_get(&comms,i);
     }

   }

   // if this is a process from one of the production codes, create a communicator to the driver
   if ( strcmp(buffer,"") != 0 ) {
     communicator new_comm;
     new_comm.type = MDI_MPI;
     new_comm.handle = MPI_COMM_WORLD;
     vector_push_back( &comms, &new_comm );
   }


   MPI_Barrier(MPI_COMM_WORLD);

   return 0;
}


/*--------------------------*/
/* MDI function definitions */
/*--------------------------*/

int MDI_Init_MPI()
{
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

  if ( any_initialization == 0 ) {
    //create the vector for the communicators
    vector_init( &comms, sizeof(communicator) );
    any_initialization = 1;
  }

  gather_names("");

  return 0;
}


/* Initialize a socket and set it to listen */
int MDI_Init(int port)
{
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

  if ( any_initialization == 0 ) {
    //create the vector for the communicators
    vector_init( &comms, sizeof(communicator) );
    any_initialization = 1;
  }

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    perror("Could not create socket");
    return -1;
  }

  // ensure that the socket is closed on sigint
  driver_sockfd = sockfd;
  signal(SIGINT, sigint_handler);

  // create the socket address
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  // enable reuse of the socket
  ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse_value, sizeof(int));
  if (ret < 0) {
    perror("Could not reuse socket");
    return -1;
  }

  // bind the socket
  ret = bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
  if (ret < 0) {
    perror("Could not bind socket");
    return -1;
  }

  // start listening (the second argument is the backlog size)
  ret = listen(sockfd, 20);
  if (ret < 0) {
    perror("Could not listen");
    return -1;
  }

  //return sockfd;
  tcp_socket = sockfd;

  return 0;
}


/* Open a socket and request a connection with a specified host */
int MDI_Open(int inet, int port, const char* hostname_ptr)
{
   int i;
   int ret, sockfd;

   if ( any_initialization == 0 ) {
     //create the vector for the communicators
     vector_init( &comms, sizeof(communicator) );
     any_initialization = 1;
   }

   if (inet==MDI_TCP) { // create a TCP socket

     struct sockaddr_in driver_address;
     struct hostent* host_ptr;

     // get the address of the host
     host_ptr = gethostbyname((char*) hostname_ptr);
     if (host_ptr == NULL) {
       perror("Error in gethostbyname");
       return -1;
     }
     if (host_ptr->h_addrtype != AF_INET) {
       perror("Unkown address type");
       return -1;
     }

     bzero((char *) &driver_address, sizeof(driver_address));
     driver_address.sin_family = AF_INET;
     driver_address.sin_addr.s_addr = 
       ((struct in_addr *)host_ptr->h_addr_list[0])->s_addr;
     driver_address.sin_port = htons(port);

     // create the socket
     sockfd = socket(AF_INET, SOCK_STREAM, 0);
     if (sockfd < 0) {
       perror("Could not create socket");
       return -1;
     }

     // connect to the driver
     // if the connection is refused, try again
     //   this allows the production code to start before the driver
     int try_connect = 1;
     while (try_connect == 1) {
       ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr));
       if (ret < 0 ) {
         if ( errno == ECONNREFUSED ) {

           // close the socket, so that a new one can be created
           ret = close(sockfd);
           if (ret < 0) {
	     perror("Could not close socket");
             return -1;
           }

	   // create the socket
	   sockfd = socket(AF_INET, SOCK_STREAM, 0);
	   if (sockfd < 0) {
	     perror("Could not create socket");
	     return -1;
	   }

	 }
         else { // only error out for errors other than "connection refused"
           perror("Could not connect to the driver");
           return -1;
         }

       }
       else {
         try_connect = 0;
       }
     }

     communicator new_comm;
     new_comm.type = MDI_TCP;
     new_comm.handle = sockfd;
     vector_push_back( &comms, &new_comm );

   }
   else if (inet==MDI_MPI) { // create an MPI communicator

     gather_names(hostname_ptr);

   }
   else {
     perror("Connection type not recognized"); exit(-1);
     return -1;
   }

   if ( returned_comms < comms.size ) {
     returned_comms++;
     return returned_comms;
   }

   return -1;
}


/* Accept an incoming connection request */
int MDI_Accept_Connection()
{
  int connection;

  // if MDI hasn't returned some connections, do that now
  if ( returned_comms < comms.size ) {
    returned_comms++;
    return returned_comms;
  }

  // check for any production codes connecting via TCP
  if ( tcp_socket > 0 ) {
    //accept a connection via TCP
    connection = accept(tcp_socket, NULL, NULL);
    if (connection < 0) {
      perror("Could not accept connection");
      exit(-1);
    }

    communicator new_comm;
    new_comm.type = MDI_TCP;
    new_comm.handle = connection;
    vector_push_back( &comms, &new_comm );

    // if MDI hasn't returned some connections, do that now
    if ( returned_comms < comms.size ) {
      returned_comms++;
      return returned_comms;
    }
  }

  // unable to accept any connections
  return 0;
}


/* Send data through the socket */
int MDI_Send(const char* data_ptr, int len, int type, int sockfd)
{
   int n;

   // determine the byte size of the data type being sent
   int datasize;
   if (type == MDI_INT) {
     datasize = sizeof(int);
   }
   else if (type == MDI_DOUBLE) {
     datasize = sizeof(double);
   }
   else if (type == MDI_CHAR) {
     datasize = sizeof(char);
   }
   else {
     perror("MDI data type not recognized in MDI_Send");
     exit(-1);
   }

   communicator* comm = vector_get(&comms,sockfd-1);

   if ( comm->type == MDI_MPI ) {
     if (type == MDI_INT) {
       MPI_Send(data_ptr, len, MPI_INT, (world_rank+1)%2, 0, MPI_COMM_WORLD);
     }
     else if (type == MDI_DOUBLE) {
       MPI_Send(data_ptr, len, MPI_DOUBLE, (world_rank+1)%2, 0, MPI_COMM_WORLD);
     }
     else if (type == MDI_CHAR) {
       MPI_Send(data_ptr, len, MPI_CHAR, (world_rank+1)%2, 0, MPI_COMM_WORLD);
     }
     else {
       perror("MDI data type not recognized in MDI_Send");
       exit(-1);
     }
   }
   else if ( comm->type == MDI_TCP ) {
     n = write(comm->handle,data_ptr,len*datasize);
     if (n < 0) { perror("Error writing to socket: server has quit or connection broke"); exit(-1); }
   }
   else {
     perror("MDI communication type not recognized in MDI_Send");
     exit(-1);
   }

   return 0;
}


/* Receive data through the socket */
int MDI_Recv(char* data_ptr, int len, int type, int sockfd)
{
   int n, nr;

   // determine the byte size of the data type being received
   int datasize;
   if (type == MDI_INT) {
     datasize = sizeof(int);
   }
   else if (type == MDI_DOUBLE) {
     datasize = sizeof(double);
   }
   else if (type == MDI_CHAR) {
     datasize = sizeof(char);
   }
   else {
     perror("MDI data type not recognized in MDI_Recv");
     exit(-1);
   }

   communicator* comm = vector_get(&comms,sockfd-1);

   if ( comm->type == MDI_MPI ) {
     if (type == MDI_INT) {
       MPI_Recv(data_ptr, len, MPI_INT, (world_rank+1)%2, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
     }
     else if (type == MDI_DOUBLE) {
       MPI_Recv(data_ptr, len, MPI_DOUBLE, (world_rank+1)%2, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
     }
     else if (type == MDI_CHAR) {
       MPI_Recv(data_ptr, len, MPI_CHAR, (world_rank+1)%2, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
     }
     else {
       perror("MDI data type not recognized in MDI_Recv");
       exit(-1);
     }
   }
   else if ( comm->type == MDI_TCP ) {
     n = nr = read(comm->handle,data_ptr,len*datasize);

     while (nr>0 && n<len*datasize )
       {  nr=read(comm->handle,&data_ptr[n],len-n); n+=nr; }

     if (n == 0) { perror("Error reading from socket: server has quit or connection broke"); exit(-1); }
   }
   else {
     perror("MDI communication type not recognized in MDI_Recv");
     exit(-1);
   }

   return 0;
}


/* Send a string of length MDI_COMMAND_LENGTH through the socket */
int MDI_Send_Command(const char* data_ptr, int sockfd)
{
   int i;
   int n;
   int len=MDI_COMMAND_LENGTH;
   char buffer[MDI_COMMAND_LENGTH];
   int str_end;

   strcpy(buffer, data_ptr);

   //Fortran has trouble with null characters, so convert them to whitespace
   str_end = 0;
   for (i=0; i<MDI_COMMAND_LENGTH; i++) {
     if( buffer[i] == '\0' ) {
       str_end = 1;
     }
     if( str_end == 1 ) {
       buffer[i] = ' ';
     }
   }

   return MDI_Send( &buffer[0], len, MDI_CHAR, sockfd );

}


/* Receive a string of length MDI_COMMAND_LENGTH through the socket */
int MDI_Recv_Command(char* data_ptr, int sockfd)
{
   int len = MDI_COMMAND_LENGTH;
   int type = MDI_CHAR;
   return MDI_Recv( data_ptr, len, type, sockfd );
}
