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
   MDI_Recv: Receives data from the socket
   MDI_Send_Command: Sends a string of length MDI_COMMAND_LENGTH over the
      socket
   MDI_Recv_Command: Receives a string of length MDI_COMMAND_LENGTH over the
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

// length of an MDI command in characters
const int MDI_COMMAND_LENGTH = 12;

// MDI data types
const int MDI_INT    = 0;
const int MDI_DOUBLE = 1;
const int MDI_CHAR   = 2;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
const int MDI_METER_TO_BOHR = 1.88972612546e10;
const int MDI_ANGSTROM_TO_BOHR = 1.88972612546;

// time
const int MDI_SECOND_TO_AUT = 4.1341374575751e16;
const int MDI_PICOSECOND_TO_AUT = 4.1341374575751e4;

// force
const int MDI_NEWTON_TO_AUF = 1.213780478e7;

// energy
const int MDI_JOULE_TO_HARTREE = 2.29371265835792e17;
const int MDI_KJ_TO_HARTREE = 2.29371265835792e20;
const int MDI_KJPERMOL_TO_HARTREE = 3.80879947807451e-4;
const int MDI_KCALPERMOL_TO_HARTREE = 1.5941730215480900E-3;
const int MDI_EV_TO_HARTREE = 3.67493266806491e-2;
const int MDI_RYDBERG_TO_HARTREE = 0.5;


/* Initialize MDI */
int MDI_Init(int* sockfd_ptr)
{
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int port;
  int reuse_value = 1;

  port = 8021;

  // write the hostname to a file
  system("hostname > ../hostname");

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    error("Could not create socket");
  }

  // create the socket address
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  // enable reuse of the socket
  ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &reuse_value, sizeof(int));
  if (ret < 0) {
    error("Could not reuse socket");
  }

  // bind the socket
  ret = bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
  if (ret < 0) {
    error("Could not bind socket");
  }

  // start listening (the second argument is the backlog size)
  ret = listen(sockfd, 20);
  if (ret < 0) {
    error("Could not listen");
  }

  *sockfd_ptr = sockfd;

  return ret;
}


int MDI_Open(int* sockfd_ptr, int* inet_ptr, int* port_ptr, const char* hostname_ptr)
{
   int sockfd, ai_err;
   int ret;

   if (*inet_ptr>0) { // create a TCP socket
     
     struct sockaddr_in driver_address;
     int i;
     int port;
     struct hostent* host_ptr;
     FILE* hostfile;
     char buff[255];

     port = 8021;

     hostfile = fopen("../hostname","r");
     fgets(buff, 255, (FILE*)hostfile);

     int hlen = 10;
     hlen = strlen(buff);

     char serv_host[hlen];
     for (i=0; i < hlen; i++) {
       serv_host[i] = buff[i];
     }
     serv_host[hlen-1] = '\0';

     printf("Driver hostname: %s\n",serv_host);

     // get the address of the host
     host_ptr = gethostbyname(serv_host);
     if (host_ptr == NULL) {
       error("Error in gethostbyname");
     }
     if (host_ptr->h_addrtype != AF_INET) {
       error("Unkown address type");
     }

     bzero((char *) &driver_address, sizeof(driver_address));
     driver_address.sin_family = AF_INET;
     driver_address.sin_addr.s_addr = 
       ((struct in_addr *)host_ptr->h_addr_list[0])->s_addr;
     driver_address.sin_port = htons(port);

     // create the socket
     sockfd = socket(AF_INET, SOCK_STREAM, 0);
     if (sockfd < 0) {
       error("Could not create socket");
     }
     printf("Here is the socket: %i\n",sockfd);

     // connect to the driver
     ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr_un));
     if (ret < 0) {
       error("Could not connect to the driver");
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
     }
   }

   *sockfd_ptr=sockfd;

   return ret;
}


int MDI_Accept_Connection(int* sockfd_ptr, int* connection_ptr)
{
  int sockfd=*sockfd_ptr;
  int connection;

  //accept a connection
  connection = accept(sockfd, NULL, NULL);
  if (connection < 0) {
    error("Could not accept connection");
  }

  *connection_ptr = connection;

  return 0;
}


int MDI_Send_Command(const char* data_ptr, int* sockfd_ptr)
{
   int i;
   int n;
   int sockfd=*sockfd_ptr;
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


int MDI_Send(const char* data_ptr, int* len_ptr, int* type_ptr, int* sockfd_ptr)
{
   int n;
   int len=*len_ptr;
   int type=*type_ptr;
   int sockfd=*sockfd_ptr;

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


int MDI_Recv(char* data_ptr, int* len_ptr, int* type_ptr, int* sockfd_ptr)
{
   int n, nr;
   int len=*len_ptr;
   int type=*type_ptr;
   int sockfd=*sockfd_ptr;

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


int MDI_Recv_Command(char* data_ptr, int* sockfd_ptr)
{
   int len = MDI_COMMAND_LENGTH;
   int* len_ptr = &len;
   int type = MDI_CHAR;
   int* type_ptr = &type;
   return MDI_Recv( data_ptr, len_ptr, type_ptr, sockfd_ptr );
}


int launch_server(const char* line)
{
  // create a fork
  int pid = fork();

  if (pid == 0) {
    // child process
    system(line);
    exit(0);
  }

  return 0;
}

