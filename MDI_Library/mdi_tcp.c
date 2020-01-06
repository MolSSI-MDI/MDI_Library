/*! \file
 *
 * \brief TCP communication implementation
 */
#ifdef _WIN32
  #include <winsock2.h>
  #include <windows.h>
#else
  #include <netinet/in.h>
  #include <sys/socket.h>
  #include <netdb.h>
  #include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "mdi.h"
#include "mdi_tcp.h"
#include "mdi_global.h"

static int sigint_sockfd;
/*! \brief SIGINT handler to ensure the socket is closed on termination
 *
 * \param [in]       dummy
 *                   Dummy argument.
 */
void sigint_handler(int dummy) {
#ifdef _WIN32
  closesocket(sigint_sockfd);
#else
  close(sigint_sockfd);
#endif
}

/*! \brief Socket over which a driver will listen for incoming connections */
int tcp_socket = -1;

/*! \brief Begin listening for incoming TCP connections
 *
 * \param [in]       port
 *                   Port to listen over
 */
int tcp_listen(int port) {
  int ret;
  int sockfd;
  struct sockaddr_in serv_addr;
  int reuse_value = 1;

#ifdef _WIN32
  // initialize Winsock
  WSADATA wsa_data;
  ret = WSAStartup(MAKEWORD(2,2), &wsa_data);
#endif

  // create the socket
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    mdi_error("Could not create socket");
    return 1;
  }

  // ensure that the socket is closed on sigint
  sigint_sockfd = sockfd;
  signal(SIGINT, sigint_handler);

  // create the socket address
  //bzero((char *) &serv_addr, sizeof(serv_addr));
  memset( &serv_addr, 0, sizeof(serv_addr) );
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  // enable reuse of the socket
  ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char*) &reuse_value, sizeof(int));
  if (ret < 0) {
    mdi_error("Could not reuse socket");
    return 1;
  }

  // bind the socket
  ret = bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr));
  if (ret < 0) {
    mdi_error("Could not bind socket");
    return 1;
  }

  // start listening (the second argument is the backlog size)
  ret = listen(sockfd, 20);
  if (ret < 0) {
    mdi_error("Could not listen");
    return 1;
  }

  //return sockfd;
  tcp_socket = sockfd;

  return 0;
}


/*! \brief Request a connection over TCP
 *
 * \param [in]       port
 *                   Port over which the driver is listening
 * \param [in]       hostname_ptr
 *                   Hostname of the driver
 */
int tcp_request_connection(int port, char* hostname_ptr) {
  int ret, sockfd;

#ifdef _WIN32
  // initialize Winsock
  WSADATA wsa_data;
  ret = WSAStartup(MAKEWORD(2,2), &wsa_data);
#endif

  code* this_code = get_code(current_code);

  struct sockaddr_in driver_address;
  struct hostent* host_ptr;

  // get the address of the host
  host_ptr = gethostbyname((char*) hostname_ptr);
  if (host_ptr == NULL) {
    mdi_error("Error in gethostbyname");
    return 1;
  }
  if (host_ptr->h_addrtype != AF_INET) {
    mdi_error("Unkown address type");
    return 1;
  }

  //bzero((char *) &driver_address, sizeof(driver_address));
  memset( &driver_address, 0, sizeof(driver_address) );
  driver_address.sin_family = AF_INET;
  driver_address.sin_addr.s_addr = 
    ((struct in_addr *)host_ptr->h_addr_list[0])->s_addr;
  driver_address.sin_port = htons(port);

  // connect to the driver
  // if the connection is refused, try again
  //   this allows the production code to start before the driver
  int try_connect = 1;
  while (try_connect == 1) {

    // create the socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
      mdi_error("Could not create socket");
      return 1;
    }

    ret = connect(sockfd, (const struct sockaddr *) &driver_address, sizeof(struct sockaddr));
    if (ret < 0 ) {
#ifdef _WIN32
      int sock_error = WSAGetLastError();
      if ( sock_error == WSAECONNREFUSED ) {
	// close the socket, so that a new one can be created
	ret = closesocket(sockfd);
#else
      if ( errno == ECONNREFUSED ) {
	// close the socket, so that a new one can be created
	ret = close(sockfd);
#endif
	if (ret != 0) {
	  mdi_error("Could not close socket");
	  return 1;
	}

      }
      else { // only error out for errors other than "connection refused"
	mdi_error("Could not connect to the driver");
	return 1;
      }

    }
    else {
      try_connect = 0;
    }
  }

  MDI_Comm comm_id = new_communicator(this_code->id, MDI_TCP);
  communicator* new_comm = get_communicator(this_code->id, comm_id);
  new_comm->sockfd = sockfd;


  // communicate the version number between codes
  // only do this if not in i-PI compatibility mode
  if ( ipi_compatibility != 1 ) {
    int version[3];
    version[0] = MDI_MAJOR_VERSION;
    version[1] = MDI_MINOR_VERSION;
    version[2] = MDI_PATCH_VERSION;
    tcp_send(&version[0], 3, MDI_INT, new_comm->id);
    tcp_recv(&new_comm->mdi_version[0], 3, MDI_INT, new_comm->id);
  }

  return 0;
}


/*! \brief Accept a TCP connection request
 */
int tcp_accept_connection() {
  int connection;
  code* this_code = get_code(current_code);

  connection = accept(tcp_socket, NULL, NULL);
  if (connection < 0) {
    mdi_error("Could not accept connection");
    return 1;
  }

  MDI_Comm comm_id = new_communicator(this_code->id, MDI_TCP);
  communicator* new_comm = get_communicator(this_code->id, comm_id);
  new_comm->sockfd = connection;

  // communicate the version number between codes
  // only do this if not in i-PI compatibility mode
  if ( ipi_compatibility != 1 ) {
    int version[3];
    version[0] = MDI_MAJOR_VERSION;
    version[1] = MDI_MINOR_VERSION;
    version[2] = MDI_PATCH_VERSION;
    tcp_send(&version[0], 3, MDI_INT, new_comm->id);
    tcp_recv(&new_comm->mdi_version[0], 3, MDI_INT, new_comm->id);
  }

  return 0;
}



/*! \brief Send data through an MDI connection, using TCP
 *
 * \param [in]       buf
 *                   Pointer to the data to be sent.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be sent.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be sent.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  // only send from rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  int n = 0;
  communicator* this = get_communicator(current_code, comm);
  size_t count_t = count;
  size_t total_sent = 0;

  // determine the byte size of the data type being sent
  size_t datasize;
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
    return 1;
  }

  while ( n >= 0 && total_sent < count_t*datasize ) {
#ifdef _WIN32
    n = send(this->sockfd, (char*)buf+total_sent, count_t*datasize-total_sent, 0);
#else
    n = write(this->sockfd, buf+total_sent, count_t*datasize-total_sent);
#endif
    total_sent += n;
  }
  if (n < 0) { 
    mdi_error("Error writing to socket: server has quit or connection broke");
    return 1;
  }

  return 0;
}


/*! \brief Receive data through an MDI connection, using TCP
 *
 * \param [in]       buf
 *                   Pointer to the buffer where the received data will be stored.
 * \param [in]       count
 *                   Number of values (integers, double precision floats, characters, etc.) to be received.
 * \param [in]       datatype
 *                   MDI handle (MDI_INT, MDI_DOUBLE, MDI_CHAR, etc.) corresponding to the type of data to be received.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  // only recv from rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  size_t n, nr;
  communicator* this = get_communicator(current_code, comm);
  size_t count_t = count;

  // determine the byte size of the data type being sent
  size_t datasize;
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
    mdi_error("MDI data type not recognized in tcp_recv");
    return 1;
  }

#ifdef _WIN32
  n = nr = recv(this->sockfd,(char*)buf,count_t*datasize,0);
#else
  n = nr = read(this->sockfd,buf,count_t*datasize);
#endif

  while (nr>0 && n<count_t*datasize ) {
#ifdef _WIN32
    nr=recv(this->sockfd,(char*)buf+n,count_t*datasize-n,0);
#else
    nr=read(this->sockfd,buf+n,count_t*datasize-n);
#endif
    n+=nr;
  }

  if (n == 0) { 
    mdi_error("Error reading from socket: server has quit or connection broke");
    return 1;
  }

  return 0;
}
