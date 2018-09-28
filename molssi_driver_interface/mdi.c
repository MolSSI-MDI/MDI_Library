/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init: Initializes a socket and sets it to listen
   MDI_Open: Opens a socket and requests a connection with a specified host
   MDI_Accept_Connection: Accepts an incoming connection request
   MDI_Send: Sends data through the socket
   MDI_Recv: Receives data through the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH through the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH through the
      socket
*/

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
const int MDI_NAME_LENGTH = 12;

// MDI data types
const int MDI_INT    = 0;
const int MDI_DOUBLE = 1;
const int MDI_CHAR   = 2;

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


/*--------------------------*/
/* MDI function definitions */
/*--------------------------*/

/* Initialize a socket and set it to listen */
int MDI_Init(int port)
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

  return sockfd;
}


/* Open a socket and request a connection with a specified host */
int MDI_Open(int inet, int port, const char* hostname_ptr)
{
   int ret, sockfd;

   if (inet>0) { // create a TCP socket

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
     printf("Here is the socket: %i\n",sockfd);

     // connect to the driver
     // if the connection is refused, try again
     //   this allows the production code to start before the driver
     int try_connect = 1;
     while (try_connect == 1) {
       ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr_un));
       if (ret < 0 ) {
         if ( errno != ECONNREFUSED ) { // only error out for errors other than "connection refused"
           perror("Could not connect to the driver");
           return -1;
         }
       }
       else {
         try_connect = 0;
       }
     }

   }
   else { // create a unix socket

     struct sockaddr_un serv_addr;

     // fills up details of the socket addres
     memset(&serv_addr, 0, sizeof(serv_addr));
     serv_addr.sun_family = AF_UNIX;
     strcpy(serv_addr.sun_path, "/tmp/ipi_");
     strcpy(serv_addr.sun_path+9, hostname_ptr);

     // create the socket
     sockfd = socket(AF_UNIX, SOCK_STREAM, 0);

     // connect through the socket
     if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) { 
       perror("Error opening UNIX socket: path unavailable, or already existing"); exit(-1);
       return -1;
     }
   }

   return sockfd;
}


/* Accept an incoming connection request */
int MDI_Accept_Connection(int sockfd)
{
  int connection;

  //accept a connection
  connection = accept(sockfd, NULL, NULL);
  if (connection < 0) {
    perror("Could not accept connection");
  }

  return connection;
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

   n = write(sockfd,data_ptr,len*datasize);
   if (n < 0) { perror("Error writing to socket: server has quit or connection broke"); exit(-1); }

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

   n = nr = read(sockfd,data_ptr,len*datasize);

   while (nr>0 && n<len*datasize )
   {  nr=read(sockfd,&data_ptr[n],len-n); n+=nr; }

   if (n == 0) { perror("Error reading from socket: server has quit or connection broke"); exit(-1); }

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

   n = write(sockfd,buffer,len);
   if (n < 0) { perror("Error writing to socket: server has quit or connection broke"); exit(-1); }

   return 0;
}


/* Receive a string of length MDI_COMMAND_LENGTH through the socket */
int MDI_Recv_Command(char* data_ptr, int sockfd)
{
   int len = MDI_COMMAND_LENGTH;
   int type = MDI_CHAR;
   return MDI_Recv( data_ptr, len, type, sockfd);
}
