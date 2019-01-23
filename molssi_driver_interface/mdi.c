/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Listen: Listen for an incoming connection of the specified type
   MDI_Request_Connection: Creates an outgoing connection request
   MDI_Accept_Connection: Accepts an incoming connection request
   MDI_MPI_Comm: Return the intra-code MPI communicator
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

// has MDI_Listen or MDI_Request_Connection been called?
static int any_initialization = 0;

// has MDI_Listen or MDI_Request_Connection been called with method="MPI"?
static int mpi_initialization = 0;

// internal MPI communicator
static MPI_Comm intra_MPI_comm;

// the TCP socket, initialized by MDI_Listen when method="TCP"
static int tcp_socket = -1;

// the MPI rank, initialized by MDI_Listen when method="MPI"
static int world_rank = -1;

// the MPI rank within the code
static int intra_rank = -1;

typedef struct communicator_struct {
  int type; // the type of communicator
  int handle; // for TCP, the socket descriptor
              // for MPI, the MPI communicator
  MPI_Comm MPI_handle;
  int MPI_rank;
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
static int returned_comms = 0;


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

static vector comms;


/*----------------------------*/
/* Signal handler definitions */
/*----------------------------*/

int driver_sockfd;
void sigint_handler(int dummy) {
  close(driver_sockfd);
}


int gather_names(const char* hostname_ptr){
   int i, j, icomm;
   int driver_rank;
   int nunique_names = 0;
   int my_name_count = 0;

   // get the number of processes
   int world_size;
   MPI_Comm_size(MPI_COMM_WORLD, &world_size);

   // get the rank of this process
   MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

   //create the name of this process
   char buffer[MDI_NAME_LENGTH];
   int str_end;
   strcpy(buffer, hostname_ptr);

   char* names = NULL;
   names = malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   char* unique_names = NULL;
   unique_names = malloc(sizeof(char) * world_size*MDI_NAME_LENGTH);

   //MPI_Gather(&buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
   //           MPI_CHAR, 0, MPI_COMM_WORLD);
   MPI_Allgather(&buffer, MDI_NAME_LENGTH, MPI_CHAR, names, MDI_NAME_LENGTH,
              MPI_CHAR, MPI_COMM_WORLD);

   if (world_rank == 0) {
     for (i=0; i<world_size; i++) {
       char* ptr1 = &names[i*MDI_NAME_LENGTH];
     }
   }

   // determine which rank corresponds to rank 0 of the driver
   driver_rank = -1;
   for (i=0; i<world_size; i++) {
     if ( driver_rank == -1 ) {
       char name[MDI_NAME_LENGTH];
       memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
       if ( strcmp(name, "") == 0 ) {
	 driver_rank = i;
       }
     }
   }
   if ( driver_rank == -1 ) {
     perror("Unable to identify driver when attempting to connect via MPI");
   }

   //if (world_rank == 0) {

     //create communicators
     for (i=0; i<world_size; i++) {
       char name[MDI_NAME_LENGTH];
       memcpy( name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

       int found = 0;
       /*
       for (icomm=0; icomm<comms.size; icomm++) {
	 communicator* comm = vector_get(&comms,icomm);
	 if ( strcmp(name, comm->name) == 0 ) {
	   found = 1;
	 }
       }
       */
       for (j=0; j<i; j++) {
	 char prev_name[MDI_NAME_LENGTH];
	 memcpy( prev_name, &names[j*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
	 if ( strcmp(name, prev_name) == 0 ) {
	   found = 1;
	 }
       }

       // check if this rank is the first instance of a new production code
       if ( found == 0 && strcmp(name,"") != 0 ) {
	 // add this code's name to the list of unique names
	 memcpy( &unique_names[nunique_names*MDI_NAME_LENGTH], name, MDI_NAME_LENGTH );
	 nunique_names++;
	 char my_name[MDI_NAME_LENGTH];
	 memcpy( my_name, &names[world_rank*MDI_NAME_LENGTH], MDI_NAME_LENGTH );
	 if ( strcmp(my_name, name) == 0 ) {
	   my_name_count = nunique_names;
	 }

         // create a communicator to handle communication with this production code
	 MPI_Comm new_mpi_comm;
	 int color = 0;
	 int key = 0;
	 if ( world_rank == driver_rank ) {
	   color = 1;
	 }
	 else if ( world_rank == i ) {
	   color = 1;
	   key = 1;
	 }
         MPI_Comm_split(MPI_COMM_WORLD, color, key, &new_mpi_comm);

	 if ( world_rank == driver_rank || world_rank == i ) {
	   communicator new_comm;
	   new_comm.type = MDI_MPI;
	   new_comm.MPI_handle = new_mpi_comm;
	   new_comm.MPI_rank = key;
	   memcpy( new_comm.name, &names[i*MDI_NAME_LENGTH], MDI_NAME_LENGTH );

	   vector_push_back( &comms, &new_comm );
	 }
       }
     }

     // create the intra-code communicators
     MPI_Comm_split(MPI_COMM_WORLD, my_name_count, world_rank, &intra_MPI_comm);
     MPI_Comm_rank(intra_MPI_comm, &intra_rank);

   MPI_Barrier(MPI_COMM_WORLD);

   return 0;
}



int MDI_Listen_TCP(int port)
{
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

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





/*--------------------------*/
/* MDI function definitions */
/*--------------------------*/


/* Initialize a socket and set it to listen */
int MDI_Listen(const char* method, void* options, void* world_comm)
{
  int ret;
  int sockfd;
  int port;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;
  char* strtol_ptr;

  if ( any_initialization == 0 ) {
    // create the vector for the communicators
    vector_init( &comms, sizeof(communicator) );
    any_initialization = 1;
  }

  if ( strcmp(method, "MPI") == 0 ) {
    gather_names("");
    mpi_initialization = 1;
  }
  else if ( strcmp(method, "TCP") == 0 ) {
    port = strtol( options, &strtol_ptr, 10 );
    MDI_Listen_TCP(port);
  }
  else {
    perror("Error in MDI_Listen: method not recognized");
    return -1;
  }

  return 0;
}


/* Open a socket and request a connection with a specified host */
//int MDI_Open(int inet, int port, const char* hostname_ptr)
int MDI_Request_Connection(const char* method, void* options, void* world_comm)
{
   int i;
   int port;
   int ret, sockfd;
   char* char_ptr;
   char* hostname_ptr;
   char* temp_char;

   if ( any_initialization == 0 ) {
     // create the vector for the communicators
     vector_init( &comms, sizeof(communicator) );
     any_initialization = 1;
   }

   //if (inet==MDI_TCP) { // create a TCP socket
   if ( strcmp(method, "MPI") == 0 ) {

     gather_names(options);
     mpi_initialization = 1;

   }
   else if ( strcmp(method, "TCP") == 0 ) {
     char sub_buff[strlen(options)];
     char hostname_buff[strlen(options)];
     char* strtol_ptr;

     // parse the hostname and port number from the options string
     temp_char = options;
     char_ptr = strrchr( options, ':' );
     memcpy( sub_buff, char_ptr+1, strlen(options) - (char_ptr - temp_char) );
     sub_buff[strlen(options) - (char_ptr - temp_char) - 1] = '\0';
     port = strtol( sub_buff, &strtol_ptr, 10 );

     //memcpy( hostname_buff, temp_char, char_ptr - temp_char + 1 );
     memcpy( hostname_buff, temp_char, (char_ptr-temp_char) );
     //////hostname_buff[ (char_ptr-temp_char) + 1 ] = '\0';
     hostname_buff[ (char_ptr-temp_char) ] = '\0';
     hostname_ptr = &hostname_buff[0];

     struct sockaddr_in driver_address;
     struct hostent* host_ptr;

     // get the address of the host
     printf("port: %d\n",port);
     printf("hostname: %s\n",hostname_ptr);
     host_ptr = gethostbyname((char*) hostname_ptr);
     //host_ptr = gethostbyname((char*) hostname_buff); 
     //host_ptr = gethostbyname("knl3.sirius.local.net");
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


/* Get the intra-code MPI communicator */
int MDI_MPI_Comm(MPI_Comm* input_comm)
{
  if ( any_initialization == 0 ) {
    perror("Must call MDI_Listen or MDI_Request_Connection before MDI_Get_MPI_Comm");
  }
  if ( mpi_initialization == 0 ) {
    *input_comm = MPI_COMM_WORLD;
  }
  else {
    *input_comm = intra_MPI_comm;
  }
  return 0;
}


/* Send data through the socket */
int MDI_Send(const char* data_ptr, int len, int type, int sockfd)
{
   if ( mpi_initialization == 1 && intra_rank != 0 ) {
     perror("Called MDI_Send with incorrect rank");
   }
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
       MPI_Send(data_ptr, len, MPI_INT, (comm->MPI_rank+1)%2, 0, comm->MPI_handle);
     }
     else if (type == MDI_DOUBLE) {
       MPI_Send(data_ptr, len, MPI_DOUBLE, (comm->MPI_rank+1)%2, 0, comm->MPI_handle);
     }
     else if (type == MDI_CHAR) {
       MPI_Send(data_ptr, len, MPI_CHAR, (comm->MPI_rank+1)%2, 0, comm->MPI_handle);
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
   if ( mpi_initialization == 1 && intra_rank != 0 ) {
     perror("Called MDI_Recv with incorrect rank");
   }
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
       MPI_Recv(data_ptr, len, MPI_INT, (comm->MPI_rank+1)%2, 0, comm->MPI_handle, MPI_STATUS_IGNORE);
     }
     else if (type == MDI_DOUBLE) {
       MPI_Recv(data_ptr, len, MPI_DOUBLE, (comm->MPI_rank+1)%2, 0, comm->MPI_handle, MPI_STATUS_IGNORE);
     }
     else if (type == MDI_CHAR) {
       MPI_Recv(data_ptr, len, MPI_CHAR, (comm->MPI_rank+1)%2, 0, comm->MPI_handle, MPI_STATUS_IGNORE);
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
   if ( mpi_initialization == 1 && intra_rank != 0 ) {
     perror("Called MDI_Send_Command with incorrect rank");
   }
   int len=MDI_COMMAND_LENGTH;
   char buffer[MDI_COMMAND_LENGTH];

   strcpy(buffer, data_ptr);
   return MDI_Send( &buffer[0], len, MDI_CHAR, sockfd );
}


/* Receive a string of length MDI_COMMAND_LENGTH through the socket */
int MDI_Recv_Command(char* data_ptr, int sockfd)
{
   if ( mpi_initialization == 1 && intra_rank != 0 ) {
     perror("Called MDI_Recv_Command with incorrect rank");
   }
   int len = MDI_COMMAND_LENGTH;
   int type = MDI_CHAR;

   return MDI_Recv( data_ptr, len, type, sockfd );
}
