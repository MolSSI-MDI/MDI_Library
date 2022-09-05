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

static sock_t sigint_sockfd;



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



/*! \brief Enable support for the TCP method */
int enable_tcp_support(int code_id) {
  int ret;
  int method_id;
  ret = new_method(code_id, MDI_TCP, &method_id);
  if ( ret != 0 ) {
    mdi_error("Error in enable_tcp_support: new_method failed");
    return ret;
  }
  method* this_method;
  ret = get_method(code_id, MDI_TCP, &this_method);
  if ( ret != 0 ) {
    mdi_error("Error in enable_tcp_support: get_method failed");
    return ret;
  }
  this_method->on_selection = tcp_on_selection;
  this_method->on_accept_communicator = tcp_on_accept_communicator;
  this_method->on_send_command = tcp_on_send_command;
  this_method->after_send_command = tcp_after_send_command;
  this_method->on_recv_command = tcp_on_recv_command;
  return 0;
}



/*! \brief Callback when the end-user selects TCP as the method */
int tcp_on_selection() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_on_selection: get_current_code failed");
    return 1;
  }

  if ( this_code->tcp_initialized == 1 ) {
    mdi_error("MDI_Init called after TCP was already initialized");
    return 1;
  }
  this_code->tcp_initialized = 1;

  if ( strcmp(this_code->role, "DRIVER") == 0 ) {
    if ( this_code->port == -1 ) {
      mdi_error("Error in MDI_Init: -port option not provided");
      return 1;
    }
    if ( this_code->intra_rank == 0 ) {
      tcp_listen(this_code->port);
    }
    else {
      // If this isn't rank 0, just set tcp_socket to > 0 so that accept_communicator knows we are using TCP
      this_code->tcp_socket = 1;
    }
  }
  else if ( strcmp(this_code->role,"ENGINE") == 0 ) {
    if ( this_code->hostname == NULL ) {
      mdi_error("Error in MDI_Init: -hostname option not provided");
      return 1;
    }
    if ( this_code->port == -1 ) {
      mdi_error("Error in MDI_Init: -port option not provided");
      return 1;
    }
    if ( this_code->intra_rank == 0 ) {
      tcp_request_connection(this_code->port, this_code->hostname);
    }
    else {
      // If this isn't rank 0, just set tcp_socket to > 0 so that accept_communicator knows we are using TCP
      if ( this_code->intra_rank != 0 ) {
        this_code->tcp_socket = 1;
      }
    }
  }
  else {
    mdi_error("Error in MDI_Init: Role not recognized");
    return 1;
  }

  return 0;
}



/*! \brief Callback when the TCP method must accept a communicator */
int tcp_on_accept_communicator() {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_on_accept_communicator: get_current_code failed");
    return 1;
  }

  // If MDI hasn't returned some connections, do that now
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    communicator* comm_obj;
    ret = get_communicator(codes.current_key, this_code->returned_comms, &comm_obj);
    if ( ret != 0 ) {
      mdi_error("Error in tcp_on_accept_communicator: get_communicator failed");
      return ret;
    }
    comm_obj->is_accepted = 1;
    return this_code->returned_comms;
  }

  // Check for any production codes connecting via TCP
  if ( this_code->tcp_socket > 0 ) {
    // Accept a connection via TCP
    // NOTE: If this is not intra_rank==0, this will always create a dummy communicator
    int size_before = this_code->comms->size;
    tcp_accept_connection();
    int size_after = this_code->comms->size;

    // if MDI hasn't returned some connections, do that now
    if ( size_before < size_after ) {
      this_code->returned_comms++;
      communicator* comm_obj;
      ret = get_communicator(codes.current_key, this_code->returned_comms, &comm_obj);
      if ( ret != 0 ) {
        mdi_error("Error in tcp_on_accept_communicator: second get_communicator failed");
        return ret;
      }
      comm_obj->is_accepted = 1;
      return (MDI_Comm)this_code->returned_comms;
    }
  }

  // unable to accept any connections
  return MDI_COMM_NULL;
}



/*! \brief Callback when the TCP method must send a command */
int tcp_on_send_command(const char* command, MDI_Comm comm, int* skip_flag) {
  return 0;
}



/*! \brief Callback after the TCP method has received a command */
int tcp_after_send_command(const char* command, MDI_Comm comm) {
  // if the command was "EXIT", delete this communicator
  if ( strcmp( command, "EXIT" ) == 0 ) {
    delete_communicator(codes.current_key, comm);
  }

  return 0;
}



/*! \brief Callback when the TCP method must receive a command */
int tcp_on_recv_command(MDI_Comm comm) {
  return 0;
}



/*! \brief Begin listening for incoming TCP connections
 *
 * \param [in]       port
 *                   Port to listen over
 */
int tcp_listen(int port_in) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_listen: get_current_code failed");
    return 1;
  }

  struct sockaddr_in serv_addr;
  int reuse_value = 1;
  sock_t sockfd;

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
  serv_addr.sin_port = htons(port_in);

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
  this_code->tcp_socket = sockfd;

  return 0;
}


/*! \brief Request a connection over TCP
 *
 * \param [in]       port
 *                   Port over which the driver is listening
 * \param [in]       hostname_ptr
 *                   Hostname of the driver
 */
int tcp_request_connection(int port_in, char* hostname_ptr) {
  int ret;
  sock_t sockfd;

#ifdef _WIN32
  // initialize Winsock
  WSADATA wsa_data;
  ret = WSAStartup(MAKEWORD(2,2), &wsa_data);
#endif

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_request_connection: get_current_code failed");
    return 1;
  }

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
  driver_address.sin_port = htons(port_in);

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

        // wait a short period before attempting to connect again
        Sleep(1000);
#else
      if ( errno == ECONNREFUSED ) {
        // close the socket, so that a new one can be created
        ret = close(sockfd);

        // wait a short period before attempting to connect again
        sleep(1);
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

  MDI_Comm comm_id;
  ret = new_communicator(this_code->id, MDI_TCP, &comm_id);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_request_connection: new_communicator failed");
    return 1;
  }
  communicator* new_comm;
  ret = get_communicator(this_code->id, comm_id, &new_comm);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_request_connection: get_communicator failed");
    return ret;
  }
  new_comm->sockfd = sockfd;
  new_comm->send = tcp_send;
  new_comm->recv = tcp_recv;


  // communicate the version number between codes
  // only do this if not in i-PI compatibility mode
  if ( this_code->ipi_compatibility != 1 ) {
    int version[3];
    int length_send[2];
    int length_recv[2];
    version[0] = MDI_MAJOR_VERSION;
    version[1] = MDI_MINOR_VERSION;
    version[2] = MDI_PATCH_VERSION;
    tcp_send(&version[0], 3, MDI_INT, new_comm->id, 0);
    tcp_recv(&new_comm->mdi_version[0], 3, MDI_INT, new_comm->id, 0);
    if ( new_comm->mdi_version[0] > 1 ||
      ( new_comm->mdi_version[0] == 1 && new_comm->mdi_version[1] >= 4 ) ) {
          length_send[0] = MDI_NAME_LENGTH_;
          length_send[1] = MDI_COMMAND_LENGTH_;
          tcp_send(&length_send[0], 2, MDI_INT, new_comm->id, 0);
          tcp_recv(&length_recv[0], 2, MDI_INT, new_comm->id, 0);
          new_comm->name_length = length_recv[0];
          new_comm->command_length = length_recv[1];
    }
  }

  return 0;
}


/*! \brief Accept a TCP connection request
 */
int tcp_accept_connection() {
  sock_t connection;
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_accept_connection: get_current_code failed");
    return 1;
  }

  if ( this_code->intra_rank == 0 ) { // Running on rank 0

    connection = accept(this_code->tcp_socket, NULL, NULL);
    if (connection < 0) {
      mdi_error("Could not accept connection");
      return 1;
    }

    MDI_Comm comm_id;
    ret = new_communicator(this_code->id, MDI_TCP, &comm_id);
    if ( ret != 0 ) {
      mdi_error("Error in tcp_accept_connection: new_communicator failed");
      return 1;
    }
    communicator* new_comm;
    ret = get_communicator(this_code->id, comm_id, &new_comm);
    if ( ret != 0 ) {
      mdi_error("Error in tcp_accept_connection: get_communicator failed");
      return ret;
    }
    new_comm->sockfd = connection;
    new_comm->send = tcp_send;
    new_comm->recv = tcp_recv;

    // communicate the version number between codes
    // only do this if not in i-PI compatibility mode
    if ( this_code->ipi_compatibility != 1 ) {
      int version[3];
      int length_send[2];
      int length_recv[2];
      version[0] = MDI_MAJOR_VERSION;
      version[1] = MDI_MINOR_VERSION;
      version[2] = MDI_PATCH_VERSION;
      tcp_send(&version[0], 3, MDI_INT, new_comm->id, 0);
      tcp_recv(&new_comm->mdi_version[0], 3, MDI_INT, new_comm->id, 0);
      if ( new_comm->mdi_version[0] > 1 ||
        ( new_comm->mdi_version[0] == 1 && new_comm->mdi_version[1] >= 4 ) ) {
            length_send[0] = MDI_NAME_LENGTH_;
            length_send[1] = MDI_COMMAND_LENGTH_;
            tcp_send(&length_send[0], 2, MDI_INT, new_comm->id, 0);
            tcp_recv(&length_recv[0], 2, MDI_INT, new_comm->id, 0);
            new_comm->name_length = length_recv[0];
            new_comm->command_length = length_recv[1];
      }
    }

  }
  else { // Not running on rank 0

    // Simply create a dummy communicator
    MDI_Comm comm_id;
    ret = new_communicator(this_code->id, MDI_TCP, &comm_id);
    if ( ret != 0 ) {
      mdi_error("Error in dummy tcp_accept_connection: new_communicator failed");
      return 1;
    }
    communicator* new_comm;
    ret = get_communicator(this_code->id, comm_id, &new_comm);
    if ( ret != 0 ) {
      mdi_error("Error in tcp_accept_connection: second get_communicator failed");
      return ret;
    }
    new_comm->sockfd = connection;
    new_comm->send = tcp_send;
    new_comm->recv = tcp_recv;

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
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int tcp_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;

  // only send from rank 0
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_send: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_send: get_communicator failed");
    return ret;
  }
  size_t count_t = count;
#ifdef _WIN32
  int n = 0;
#else
  ssize_t n = 0;
#endif

  // determine the byte size of the data type being sent
  size_t datasize;
  MDI_Datatype basetype;
  ret = datatype_info(datatype, &datasize, &basetype);
  if ( ret != 0 ) { return ret; }

  n = 0;
  size_t total_sent = 0;
  while ( n >= 0 && total_sent < count_t*datasize ) {
#ifdef _WIN32
    n = send(this->sockfd, (char*)buf+total_sent, (int)(count_t*datasize-total_sent), 0);
#else
    n = write(this->sockfd, (char*)buf+total_sent, count_t*datasize-total_sent);
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
 * \param [in]       msg_flag
 *                   Type of role this data has within a message.
 *                   0: Not part of a message.
 *                   1: The header of a message.
 *                   2: The body (data) of a message.
 */
int tcp_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm, int msg_flag) {
  int ret;
  
  // only recv from rank 0
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_recv: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

#ifdef _WIN32
  int n, nr;
#else
  ssize_t n, nr;
#endif
  communicator* this;
  ret = get_communicator(codes.current_key, comm, &this);
  if ( ret != 0 ) {
    mdi_error("Error in tcp_recv: get_communicator failed");
    return ret;
  }
  size_t count_t = count;

  // determine the byte size of the data type being sent
  size_t datasize;
  MDI_Datatype basetype;
  ret = datatype_info(datatype, &datasize, &basetype);
  if ( ret != 0 ) { return ret; }

#ifdef _WIN32
  n = nr = recv(this->sockfd,(char*)buf,(int)(count_t*datasize),0);
#else
  n = nr = read(this->sockfd,(char*)buf,count_t*datasize);
#endif

  while (nr>0 && n<count_t*datasize ) {
#ifdef _WIN32
    nr=recv(this->sockfd,(char*)buf+n,(int)(count_t*datasize-n),0);
#else
    nr=read(this->sockfd,(char*)buf+n,count_t*datasize-n);
#endif
    n+=nr;
  }

  if (n == 0) { 
    mdi_error("Error reading from socket: server has quit or connection broke");
    return 1;
  }

  return 0;
}
