/*! \file
 *
 * \brief Generic MDI function calls
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdi.h"
#include "mdi_general.h"
#include "mdi_mpi.h"
#include "mdi_tcp.h"
#include "mdi_lib.h"
#include "mdi_test.h"

/*! \brief Initialize communication through the MDI library
 *
 * If using the "-method MPI" option, this function must be called by all ranks.
 * The function returns \p 0 on a success.
 *
 * \param [in]       options
 *                   Options describing the communication method used to connect to codes.
 */
int general_init(const char* options) {
  // If this is the first time MDI has initialized, initialize the code vector
  if ( ! is_initialized ) {
    vector_init(&codes, sizeof(code));
  }

  // MDI assumes that each call to general_init corresponds to a new code, so create a new code now
  // Note that unless using the LINK communication method, general_init should only be called once
  current_code = new_code();
  code* this_code = get_code(current_code);

  char* strtol_ptr;
  int i, ret;

  int mpi_initialized = 0;

  // values acquired from the input options
  char* role;
  char* method;
  char* hostname;
  int port;
  char* output_file;
  char* language_argument = ((char*)"");
  int has_role = 0;
  int has_method = 0;
  int has_name = 0;
  int has_hostname = 0;
  int has_port = 0;
  int has_plugin_path = 0;
  int has_output_file = 0;

  // calculate argc
  char* argv_line = mdi_strdup(options);
  char* token = strtok(argv_line, " ");
  int argc = 0;
  while (token != NULL) {
    argc++;
    token = strtok(NULL," ");
  }
  free( argv_line );

  // calculate argv
  char** argv = malloc( argc * sizeof(char*) );
  argv_line = mdi_strdup(options);
  token = strtok(argv_line, " ");
  for (i=0; i<argc; i++) {
    argv[i] = token;
    token = strtok(NULL," ");
  }

  // read options
  int iarg = 0;
  while (iarg < argc) {

    //-role
    if (strcmp(argv[iarg],"-role") == 0){
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -role option");
	return 1;
      }
      role = argv[iarg+1];
      if ( strlen(role) > NAME_LENGTH ) {
	mdi_error("Error in MDI_Init: Role option is larger than MDI_NAME_LENGTH");
	return 1;
      }
      snprintf(this_code->role, strlen(role)+1, "%s", role);
      has_role = 1;
      iarg += 2;
    }
    //-method
    else if (strcmp(argv[iarg],"-method") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -method option");
	return 1;
      }
      method = argv[iarg+1];
      has_method = 1;
      iarg += 2;
    }
    //-name
    else if (strcmp(argv[iarg],"-name") == 0){
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -name option");
	return 1;
      }
      if ( strlen(argv[iarg+1]) > NAME_LENGTH ) {
	mdi_error("Error in MDI_Init: Name argument length exceeds MDI_NAME_LENGTH");
	return 1;
      }
      snprintf(this_code->name, strlen(argv[iarg+1])+1, "%s", argv[iarg+1]);
      has_name = 1;
      iarg += 2;
    }
    //-hostname
    else if (strcmp(argv[iarg],"-hostname") == 0){
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -hostname option");
	return 1;
      }
      hostname = argv[iarg+1];
      has_hostname = 1;
      iarg += 2;
    }
    //-port
    else if (strcmp(argv[iarg],"-port") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -port option");
	return 1;
      }
      port = strtol( argv[iarg+1], &strtol_ptr, 10 );
      has_port = 1;
      iarg += 2;
    }
    //-ipi
    else if (strcmp(argv[iarg],"-ipi") == 0) {
      ipi_compatibility = 1;
      iarg += 1;
    }
    //-out
    else if (strcmp(argv[iarg],"-out") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -out option");
	return 1;
      }
      has_output_file = 1;
      output_file = argv[iarg+1];
      iarg += 2;
    }
    //-plugin_path
    else if (strcmp(argv[iarg],"-plugin_path") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -plugin_path option");
	return 1;
      }
      if ( strlen(argv[iarg+1]) > PLUGIN_PATH_LENGTH ) {
	mdi_error("Error in MDI_Init: Plugin path is larger than PLUGIN_PATH_LENGTH");
	return 1;
      }
      snprintf(this_code->plugin_path, strlen(argv[iarg+1])+1, "%s", argv[iarg+1]);
      has_plugin_path = 1;
      iarg += 2;
    }
    //_language
    else if (strcmp(argv[iarg],"_language") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -_language option");
	return 1;
      }
      language_argument = argv[iarg+1];
      if ( strcmp(language_argument, "Python") == 0 ) {
        this_code->language = MDI_LANGUAGE_PYTHON;
      }
      else if ( strcmp(language_argument, "Fortran") == 0 ) {
        this_code->language = MDI_LANGUAGE_FORTRAN;
      }
      else {
        mdi_error("Error in MDI_Init: Invalide -_language argument");
      }
      iarg += 2;
    }
    else {
      mdi_error("Error in MDI_Init: Unrecognized option");
      return 1;
    }
  }

  // ensure the -role option was provided
  if ( has_role == 0 ) {
    mdi_error("Error in MDI_Init: -role option not provided");
    return 1;
  }

  // ensure the -name option was provided
  if ( has_name == 0 ) {
    mdi_error("Error in MDI_Init: -name option not provided");
    return 1;
  }

  // ensure the -method option was provided
  if ( has_method == 0 ) {
    mdi_error("Error in MDI_Init: -method option not provided");
    return 1;
  }

  // if using the MPI method, check if MPI has been initialized
  int mpi_init_flag = 0;
  if ( strcmp(method, "MPI") == 0 && this_code->language != MDI_LANGUAGE_PYTHON ) {

    ret = MPI_Initialized(&mpi_init_flag);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init: MPI_Initialized failed");
      return ret;
    }

    if ( mpi_init_flag == 0 ) {

      // initialize MPI
      int mpi_argc = 0;
      char** mpi_argv;
      ret = MPI_Init( &mpi_argc, &mpi_argv );
      if ( ret != 0 ) {
	mdi_error("Error in MDI_Init: MPI_Init failed");
	return ret;
      }

      // confirm that MPI is now initialized
      // if it isn't, that indicates that the MPI stubs are being used
      ret = MPI_Initialized(&mpi_init_flag);
      if ( ret != 0 ) {
	mdi_error("Error in MDI_Init: MPI_Initialized failed");
	return ret;
      }
      if ( mpi_init_flag == 0 ) {
	mdi_error("Error in MDI_Init: Failed to initialize MPI. Check that the MDI Library is linked to an MPI library.");
	return 1;
      }

      // get the world_comm
      mdi_mpi_comm_world = MPI_COMM_WORLD;
      initialized_mpi = 1;

    }

  }

  // get the appropriate MPI communicator to use
  MPI_Comm mpi_communicator;
  ret = MPI_Initialized(&mpi_init_flag);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init: MPI_Initialized failed");
    return ret;
  }
  if ( mpi_init_flag == 0 ) {
    mpi_communicator = 0;
  }
  else {
    if ( this_code->language == MDI_LANGUAGE_PYTHON ) {
      mpi_communicator = 0;
    }
    else {
      mpi_communicator = MPI_COMM_WORLD;
      MPI_Comm_rank(mpi_communicator, &world_rank);
      MPI_Comm_size(mpi_communicator, &world_size);
      this_code->intra_rank = world_rank;
    }
  }

  // redirect the standard output
  if ( has_output_file == 1 ) {
    freopen(output_file, "w", stdout);
  }

  // if the method is not LIB, ensure that MDI has not been previously initialized
  if ( strcmp(method, "LINK") != 0 ) {
    if ( is_initialized == 1 ) {
      mdi_error("MDI_Init called after MDI was already initialized");
      return 1;
    }
  }

  // ensure that the name of this code is not the same as the name of any of the other codes
  for (i = 0; i < codes.size; i++) {
    if ( i != current_code ) {
      code* other_code = vector_get(&codes, i);
      if (strcmp(this_code->name, other_code->name) == 0) {
	mdi_error("MDI_Init found multiple codes with the same name");
	return 1;
      }
    }
  }

  // Check if this is an engine being used as a library
  if (strcmp(this_code->role, "ENGINE") == 0) {
    if ( strcmp(method, "LINK") == 0 ) {
      this_code->is_library = 1;
    }
  }

  // ensure that at most one driver has been initialized
  if (strcmp(this_code->role, "DRIVER") == 0) {
    for (i = 0; i < codes.size; i++) {
      if ( i != current_code ) {
	code* other_code = vector_get(&codes, i);
	if (strcmp(this_code->role, other_code->role) == 0) {
	  mdi_error("MDI_Init found multiple drivers");
	  return 1;
	}
      }
    }
  }

  // determine whether the intra-code MPI communicator should be split by mpi_init_mdi
  int use_mpi4py = 0;
  if ( this_code->language == MDI_LANGUAGE_PYTHON ) {
    use_mpi4py = 1;
  }

  if ( strcmp(role, "DRIVER") == 0 ) {
    // initialize this code as a driver

    if ( strcmp(method, "MPI") == 0 ) {
      mpi_identify_codes("", use_mpi4py, mpi_communicator);
      mpi_initialized = 1;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
	return 1;
      }
      if ( this_code->intra_rank == 0 ) {
	tcp_listen(port);
      }
      else {
	// If this isn't rank 0, just set tcp_socket to > 0 so that accept_communicator knows we are using TCP
	tcp_socket = 1;
      }
    }
    else if ( strcmp(method, "LINK") == 0 ) {
      //library_initialize();
    }
    else if ( strcmp(method, "TEST") == 0 ) {
      test_initialize();
    }
    else {
      mdi_error("Error in MDI_Init: Method not recognized");
      return 1;
    }

  }
  else if ( strcmp(role,"ENGINE") == 0 ) {
    // initialize this code as an engine

    if ( strcmp(method, "MPI") == 0 ) {
      code* this_code = get_code(current_code);
      mpi_identify_codes(this_code->name, use_mpi4py, mpi_communicator);
      mpi_initialized = 1;
    }
    else if ( strcmp(method, "TCP") == 0 ) {
      if ( has_hostname == 0 ) {
	mdi_error("Error in MDI_Init: -hostname option not provided");
	return 1;
      }
      if ( has_port == 0 ) {
	mdi_error("Error in MDI_Init: -port option not provided");
	return 1;
      }
      if ( this_code->intra_rank == 0 ) {
	tcp_request_connection(port, hostname);
      }
      else {
	// If this isn't rank 0, just set tcp_socket to > 0 so that accept_communicator knows we are using TCP
	if ( this_code->intra_rank != 0 ) {
	  tcp_socket = 1;
	}
      }
    }
    else if ( strcmp(method, "LINK") == 0 ) {
      library_initialize();
    }
    else if ( strcmp(method, "TEST") == 0 ) {
      test_initialize();
    }
    else {
      mdi_error("Error in MDI_Init: method not recognized");
      return 1;
    }

    
  }
  else {
    mdi_error("Error in MDI_Init: Role not recognized");
    return 1;
  }

  free( argv_line );
  free( argv );

  return 0;
}


/*! \brief Accept a new MDI communicator
 *
 * The function returns an MDI_Comm that describes a connection between two codes.
 * If no new communicators are available, the function returns \p MDI_COMM_NULL.
 *
 */
int general_accept_communicator() {
  // Give the library method an opportunity to update the current code
  library_accept_communicator();

  // If MDI hasn't returned some connections, do that now
  code* this_code = get_code(current_code);
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    return this_code->returned_comms;
  }

  // Check for any production codes connecting via TCP
  if ( tcp_socket > 0 ) {

    // Accept a connection via TCP
    // NOTE: If this is not intra_rank==0, this will always create a dummy communicator
    tcp_accept_connection();

    // if MDI hasn't returned some connections, do that now
    if ( this_code->returned_comms < this_code->comms->size ) {
      this_code->returned_comms++;
      return (MDI_Comm)this_code->returned_comms;
    }

  }

  // unable to accept any connections
  return MDI_COMM_NULL;
}


/*! \brief Send a message through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
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
int general_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  int ret = 0;

  communicator* this = get_communicator(current_code, comm);

  // send message header information
  // only do this if communicating with MDI version 1.1 or higher
  if ( ( this->mdi_version[0] > 1 ||
         ( this->mdi_version[0] == 1 && this->mdi_version[1] >= 1 ) )
       && ipi_compatibility != 1 ) {

    // prepare the header information
    size_t nheader = 4;
    int* header = (int*) malloc( nheader * sizeof(int) );
    header[0] = 0;        // error flag
    header[1] = 0;        // header type
    header[2] = datatype; // datatype
    header[3] = count;    // count

    // send the header
    ret = this->send((void*)header, (int)nheader, MDI_INT, comm, 1);
    if ( ret != 0 ) { return ret; }

    free( header );
  }

  // send the data
  ret = this->send(buf, count, datatype, comm, 2);
  if ( ret != 0 ) { return ret; }

  return 0;
}


/*! \brief Receive a message through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
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
int general_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm) {
  int ret = 0;

  // Actual datatype of data sent to this code
  // This will be read from the message header, and might be different from datatype
  int send_datatype = datatype;
  size_t send_datasize;

  communicator* this = get_communicator(current_code, comm);

  // receive message header information
  // only do this if communicating with MDI version 1.1 or higher
  if ( ( this->mdi_version[0] > 1 ||
         ( this->mdi_version[0] == 1 && this->mdi_version[1] >= 1 ) )
       && ipi_compatibility != 1 ) {

    // prepare buffer to hold header information
    size_t nheader = 4;
    int* header = (int*) malloc( nheader * sizeof(int) );

    // initialize the header with the expected data
    // this is important when ranks other than 0 call this function
    header[0] = 0;
    header[1] = 0;
    header[2] = datatype;
    header[3] = count;

    // receive the header
    ret = this->recv((void*)header, (int)nheader, MDI_INT, comm, 1);
    if ( ret != 0 ) { return ret; }

    // analyze the header information
    int error_flag = header[0];
    int header_type = header[1];
    send_datatype = header[2];
    int send_count = header[3];

    // verify that the error flag is zero
    if ( error_flag != 0 ) {
      mdi_error("Error in MDI_Recv: nonzero error flag received");
      return error_flag;
    }

    // verify that the header type is zero
    if ( header_type != 0 ) {
      mdi_error("Error in MDI_Recv: unsupported header type");
      return error_flag;
    }

    // verify agreement regarding the datatype
    MDI_Datatype send_basetype;
    ret = datatype_info(send_datatype, &send_datasize, &send_basetype);
    if ( ret != 0 ) { return ret; }
    size_t recv_datasize;
    MDI_Datatype recv_basetype;
    ret = datatype_info(datatype, &recv_datasize, &recv_basetype);
    if ( ret != 0 ) { return ret; }
    if ( send_basetype != recv_basetype ) {
      mdi_error("Error in MDI_Recv: inconsistent datatype");
      return 1;
    }

    // verify agreement regarding the count
    if ( send_count != count ) {
      mdi_error("Error in MDI_Recv: inconsistent count");
      return 1;
    }

    free( header );
  }

  // receive the data
  if ( send_datatype == datatype ) {
    ret = this->recv(buf, count, datatype, comm, 2);
    if ( ret != 0 ) { return ret; }
  }
  else {
    // the datatypes do not match, but are compatible
    // recieve the data into a temporary buffer
    void* tempbuf = malloc( count * send_datasize );
    ret = this->recv(tempbuf, count, send_datatype, comm, 2);
    if ( ret != 0 ) { return ret; }

    // convert the data to the correct datatype
    ret = convert_buf_datatype(buf, datatype, tempbuf, send_datatype, count);
    if ( ret != 0 ) { return ret; }

    // free the temporary buffer
    free( tempbuf );
  }

  return 0;
}


/*! \brief Send a command of length \p MDI_COMMAND_LENGTH through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the data to be sent.
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int general_send_command(const char* buf, MDI_Comm comm) {
  code* this_code = get_code(current_code);

  // ensure that the driver is the current code
  library_set_driver_current();

  communicator* this = get_communicator(current_code, comm);
  int method = this->method;

  int count = MDI_COMMAND_LENGTH;
  char* command = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
  int ichar;
  for ( ichar=0; ichar < MDI_COMMAND_LENGTH; ichar++) {
    command[ichar] = '\0';
  }
  //const char* command = buf;
  int ret = 0;

  // only need to copy the command if this is a plugin (for all ranks) or if this is rank 0
  if ( method == MDI_LINK || this_code->intra_rank == 0 ) {
    // copy the command string, inserting terminal zeros
    int actual_message_length = 0;
    for ( ichar=0; ichar < MDI_COMMAND_LENGTH; ichar++ ) {
      actual_message_length++;
      if ( buf[ichar] == '\0' ) {
	break;
      }
    }
    snprintf(command, actual_message_length, "%s", buf);
  }
  
  if ( method == MDI_LINK ) {
    // set the command for the engine to execute
    library_set_command(command, comm);

    if ( command[0] == '<' ) {
      // execute the command, so that the data from the engine can be received later by the driver
      ret = library_execute_command(comm);
      if ( ret != 0 ) {
	mdi_error("Error in MDI_Send_Command: Unable to execute receive command through library");
	return ret;
      }
    }
    else if ( command[0] == '>' ) {
      // flag the command to be executed after the next call to MDI_Send
      library_data* libd = (library_data*) this->method_data;
      libd->execute_on_send = 1;
    }
    else if ( plugin_mode && ( strcmp( command, "EXIT" ) == 0 || command[0] == '@' ) ) {
      // this command should be received by MDI_Recv_command, rather than through the execute_command callback
      ret = general_send( command, count, MDI_CHAR, comm );
      if ( ret != 0 ) {
	mdi_error("Error in MDI_Send_Command: Unable to send command");
	return ret;
      }
    }
    else {
      // this is a command that neither sends nor receives data, so execute it now
      ret = library_execute_command(comm);
      if ( ret != 0 ) {
	mdi_error("Error in MDI_Send_Command: Unable to execute command through library");
	return ret;
      }
    }
  }
  else {
    ret = general_send( command, count, MDI_CHAR, comm );
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Send_Command: Unable to send command");
      return ret;
    }
  }

  // if the command was "EXIT", delete this communicator
  // if running in plugin mode, the plugin system will delete the communicator instead
  //if ( ! plugin_mode && this_code->intra_rank == 0 && strcmp( command, "EXIT" ) == 0 ) {
  if ( ! plugin_mode && strcmp( command, "EXIT" ) == 0 ) {
    delete_communicator(current_code, comm);

    // if MDI called MPI_Init, and there are no more communicators, call MPI_Finalize now
    if ( initialized_mpi == 1 ) {
      if ( this_code->comms->size == 0 ) {
	MPI_Finalize();
      }
    }
  }

  free( command );
  return ret;
}


/*! \brief Respond to a general built-in command
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 1 if the command is a built-in command and \p 0 otherwise.
 *
 * \param [in]       buf
 *                   Pointer to the buffer for the command name.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int general_builtin_command(const char* buf, MDI_Comm comm) {
  int ret = 0;

  // check if this command corresponds to one of MDI's standard built-in commands
  if ( strcmp( buf, "<NAME" ) == 0 ) {
    code* this_code = get_code(current_code);
    MDI_Send(this_code->name, NAME_LENGTH, MDI_CHAR, comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<VERSION" ) == 0 ) {
    int version[3];
    version[0] = MDI_MAJOR_VERSION;
    version[1] = MDI_MINOR_VERSION;
    version[2] = MDI_PATCH_VERSION;
    MDI_Send(&version[0], 3, MDI_INT, comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<COMMANDS" ) == 0 ) {
    send_command_list(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<CALLBACKS" ) == 0 ) {
    send_callback_list(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<NODES" ) == 0 ) {
    send_node_list(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<NCOMMANDS" ) == 0 ) {
    send_ncommands(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<NCALLBACKS" ) == 0 ) {
    send_ncallbacks(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "<NNODES" ) == 0 ) {
    send_nnodes(comm);
    ret = 1;
  }
  else if ( strcmp( buf, "EXIT" ) == 0 ) {
    // if the MDI Library called MPI_Init, call MPI_Finalize now
    if ( initialized_mpi == 1 ) {
      MPI_Finalize();
    }
  }
  return ret;
}


/*! \brief Receive a command of length \p MDI_COMMAND_LENGTH through the MDI connection
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       buf
 *                   Pointer to the buffer where the received data will be stored.
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int general_recv_command(char* buf, MDI_Comm comm) {
  int ret;
  code* this_code = get_code(current_code);
  communicator* this = get_communicator(current_code, comm);

  // if this is a linked library, call the driver's node callback
  if ( this->method == MDI_LINK ) {
    int iengine = current_code;
    communicator* engine_comm = get_communicator(current_code, comm);

    // get the driver code to which this communicator connects
    library_data* libd = (library_data*) engine_comm->method_data;
    int idriver = libd->connected_code;
    code* driver_code = get_code(idriver);

    MDI_Comm driver_comm_handle = library_get_matching_handle(comm);
    communicator* driver_comm = get_communicator(idriver, driver_comm_handle);
    library_data* driver_lib = (library_data*) driver_comm->method_data;

    // set the current code to the driver
    current_code = idriver;

    void* class_obj = driver_lib->driver_callback_obj;
    ret = driver_lib->driver_node_callback(&driver_lib->mpi_comm, driver_comm_handle, class_obj);

    // set the current code to the engine
    current_code = iengine;

    //return 0;
  }

  // only receive on rank 0
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }
  int count = MDI_COMMAND_LENGTH;
  int datatype = MDI_CHAR;

  ret = general_recv( buf, count, datatype, comm );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Recv_Command: Unable to receive command");
    return ret;
  }

  // check if this command corresponds to one of MDI's standard built-in commands
  int builtin_flag = general_builtin_command(buf, comm);
  if ( builtin_flag == 1 ) {
    return general_recv_command(buf, comm);
  }
  else if ( builtin_flag != 0 ) {
    mdi_error("Error in MDI_Recv_Command: Unable to respond to builtin command");
    return builtin_flag;
  }

  return ret;
}


/*! \brief Register a node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_vec
 *                   Vector of nodes, into which the new node will be added.
 * \param [in]       node_name
 *                   Name of the node.
 */
int register_node(vector* node_vec, const char* node_name)
{
  // only register on rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) >= COMMAND_LENGTH ) {
    mdi_error("Cannot register node name with length greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // confirm that this node is not already registered
  int node_index = get_node_index(node_vec, node_name);
  if ( node_index != -1 ) {
    mdi_error("This node is already registered"); 
    return 1;
  }

  node new_node;
  vector* command_vec = malloc(sizeof(vector));
  vector* callback_vec = malloc(sizeof(vector));
  vector_init(command_vec, sizeof(char[COMMAND_LENGTH]));
  vector_init(callback_vec, sizeof(char[COMMAND_LENGTH]));
  new_node.commands = command_vec;
  new_node.callbacks = callback_vec;
  int ichar;
  for (ichar = 0; ichar < COMMAND_LENGTH; ichar++) {
    new_node.name[ichar] = '\0';
  }
  snprintf(new_node.name, strlen(node_name)+1, "%s", node_name);
  vector_push_back(node_vec, &new_node);
  return 0;
}


/*! \brief Register a command on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_vec
 *                   Vector of nodes, into which the new node will be added.
 * \param [in]       node_name
 *                   Name of the node on which the command will be registered.
 * \param [in]       command_name
 *                   Name of the command.
 */
int register_command(vector* node_vec, const char* node_name, const char* command_name)
{
  // only register on rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) >= COMMAND_LENGTH ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // confirm that the command_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(command_name) >= COMMAND_LENGTH ) {
    mdi_error("Cannot register command name with length greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // find the node
  int node_index = get_node_index(node_vec, node_name);
  if ( node_index == -1 ) {
    mdi_error("Attempting to register a command on an unregistered node");
    return 1;
  }
  node* target_node = vector_get(node_vec, node_index);

  // confirm that this command is not already registered
  int command_index = get_command_index(target_node, command_name);
  if ( command_index != -1 ) {
    mdi_error("This command is already registered for this node");
    return 1;
  }

  // register this command
  char new_command[COMMAND_LENGTH];
  int ichar;
  for ( ichar = 0; ichar < COMMAND_LENGTH; ichar++) {
    new_command[ichar] = '\0';
  }
  snprintf(new_command, strlen(node_name)+1, "%s", command_name);
  vector_push_back( target_node->commands, &new_command );

  return 0;
}


/*! \brief Register a callback on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_vec
 *                   Vector of nodes, into which the new node will be added.
 * \param [in]       node_name
 *                   Name of the node on which the callback will be registered.
 * \param [in]       callback_name
 *                   Name of the callback.
 */
int register_callback(vector* node_vec, const char* node_name, const char* callback_name)
{
  // only register on rank 0
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) >= COMMAND_LENGTH ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // confirm that the callback_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(callback_name) >= COMMAND_LENGTH ) {
    mdi_error("Cannot register callback name with length greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // find the node
  int node_index = get_node_index(node_vec, node_name);
  if ( node_index == -1 ) {
    mdi_error("Attempting to register a callback on an unregistered node");
    return 1;
  }
  node* target_node = vector_get(node_vec, node_index);

  // confirm that this callback is not already registered
  int callback_index = get_callback_index(target_node, callback_name);
  if ( callback_index != -1 ) {
    mdi_error("This callback is already registered for this node");
    return 1;
  }

  // register this callback
  char new_callback[COMMAND_LENGTH];
  int ichar;
  for ( ichar=0; ichar < COMMAND_LENGTH; ichar++ ) {
    new_callback[ichar] = '\0';
  }
  snprintf(new_callback, strlen(callback_name)+1, "%s", callback_name);
  vector_push_back( target_node->callbacks, &new_callback );

  return 0;
}


/*! \brief Send the list of supported commands
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_command_list(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send command information from the incorrect rank");
    return 1;
  }
  int ncommands = 0;
  int nnodes = (int)this_code->nodes->size;
  int inode, icommand;
  int stride = MDI_COMMAND_LENGTH + 1;

  // determine the number of commands
  for (inode = 0; inode < nnodes; inode++) {
    node* this_node = vector_get(this_code->nodes, inode);
    ncommands += (int)this_node->commands->size;
  }

  // allocate memory for the commands list
  int count = ( ncommands + nnodes ) * stride;
  char* commands = malloc( count * sizeof(char) );

  // form the list of commands
  int islot = 0;
  for (inode = 0; inode < nnodes; inode++) {
    // add the name of this node to the list
    node* this_node = vector_get(this_code->nodes, inode);
    int length = (int)strlen(this_node->name);
    snprintf(&commands[ islot * stride ], strlen(this_node->name)+1, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      commands[ islot * stride + ichar ] = ' ';
    }
    if ( this_node->commands->size > 0 ) {
      commands[ islot * stride + stride - 1 ] = ',';
    }
    else {
      commands[ islot * stride + stride - 1 ] = ';';
    }
    islot++;

    // add the commands for this node
    for (icommand = 0; icommand < this_node->commands->size; icommand++) {
      char* command = vector_get(this_node->commands, icommand);
      length = (int)strlen(command);
      snprintf(&commands[ islot * stride ], COMMAND_LENGTH, "%s", command);
      for (ichar = length; ichar < stride-1; ichar++) {
	commands[ islot * stride + ichar ] = ' ';
      }
      if ( icommand == this_node->commands->size - 1 ) {
	commands[ islot * stride + stride - 1 ] = ';';
      }
      else {
	commands[ islot * stride + stride - 1 ] = ',';
      }
      islot++;
    }
  }

  int ret = general_send( commands, count, MDI_CHAR, comm );
  free( commands );
  return ret;
}


/*! \brief Send the list of callbacks
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_callback_list(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send callback information from the incorrect rank");
    return 1;
  }
  int ncallbacks = 0;
  int nnodes = (int)this_code->nodes->size;
  int inode, icallback;
  int stride = MDI_COMMAND_LENGTH + 1;

  // determine the number of callbakcs
  for (inode = 0; inode < nnodes; inode++) {
    node* this_node = vector_get(this_code->nodes, inode);
    ncallbacks += (int)this_node->callbacks->size;
  }

  // allocate memory for the callbacks list
  int count = ( ncallbacks + nnodes ) * stride;
  char* callbacks = malloc( count * sizeof(char) );
  int ichar;
  for (ichar = 0; ichar < count; ichar++) {
    callbacks[ichar] = '\0';
  }

  // form the list of callbacks
  int islot = 0;
  for (inode = 0; inode < nnodes; inode++) {
    // add the name of this node to the list
    node* this_node = vector_get(this_code->nodes, inode);
    int length = (int)strlen(this_node->name);
    snprintf(&callbacks[ islot * stride ], strlen(this_node->name)+1, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      callbacks[ islot * stride + ichar ] = ' ';
    }
    if ( this_node->callbacks->size > 0 ) {
      callbacks[ islot * stride + stride - 1 ] = ',';
    }
    else {
      callbacks[ islot * stride + stride - 1 ] = ';';
    }
    islot++;

    // add the callbacks for this node
    for (icallback = 0; icallback < this_node->callbacks->size; icallback++) {
      char* callback = vector_get(this_node->callbacks, icallback);
      length = (int)strlen(callback);
      snprintf(&callbacks[ islot * stride ], COMMAND_LENGTH, "%s", callback);
      for (ichar = length; ichar < stride-1; ichar++) {
	callbacks[ islot * stride + ichar ] = ' ';
      }
      if ( icallback == this_node->callbacks->size - 1 ) {
	callbacks[ islot * stride + stride - 1 ] = ';';
      }
      else {
	callbacks[ islot * stride + stride - 1 ] = ',';
      }
      islot++;
    }
  }

  int ret = general_send( callbacks, count, MDI_CHAR, comm );
  free( callbacks );
  return ret;
}


/*! \brief Send the list of nodes
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_node_list(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send node information from the incorrect rank");
    return 1;
  }
  int nnodes = (int)this_code->nodes->size;
  int inode;
  int stride = MDI_COMMAND_LENGTH + 1;

  // allocate memory for the node list
  int count = nnodes * stride;
  char* node_list = malloc( count * sizeof(char) );

  // form the list of nodes
  for (inode = 0; inode < nnodes; inode++) {
    // add the name of this node to the list
    node* this_node = vector_get(this_code->nodes, inode);
    int length = (int)strlen(this_node->name);
    if ( strlen(this_node->name) >= COMMAND_LENGTH ) {
      mdi_error("Error in send_node_list: Node name is larger than MDI_COMMAND_LENGTH");
      return 1;
    }
    snprintf(&node_list[ inode * stride ], strlen(this_node->name)+1, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      node_list[ inode * stride + ichar ] = ' ';
    }
    node_list[ inode * stride + stride - 1 ] = ',';
  }

  int ret = general_send( node_list, count, MDI_CHAR, comm );
  free( node_list );
  return ret;
}


/*! \brief Send the number of supported commands
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_ncommands(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send command information from the incorrect rank");
    return 1;
  }
  int ncommands = 0;
  int nnodes = (int)this_code->nodes->size;
  int inode;
  int stride = MDI_COMMAND_LENGTH + 1;

  // determine the number of commands
  for (inode = 0; inode < nnodes; inode++) {
    node* this_node = vector_get(this_code->nodes, inode);
    ncommands += (int)this_node->commands->size;
  }

  int ret = general_send( &ncommands, 1, MDI_INT, comm );
  return ret;
}


/*! \brief Send the number of supported callbacks
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_ncallbacks(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send callback information from the incorrect rank");
    return 1;
  }
  int ncallbacks = 0;
  int nnodes = (int)this_code->nodes->size;
  int inode;
  int stride = MDI_COMMAND_LENGTH + 1;

  // determine the number of callbacks
  for (inode = 0; inode < nnodes; inode++) {
    node* this_node = vector_get(this_code->nodes, inode);
    ncallbacks += (int)this_node->callbacks->size;
  }

  int ret = general_send( &ncallbacks, 1, MDI_INT, comm );
  return ret;
}


/*! \brief Send the number of supported nodes
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the intended recipient code.
 */
int send_nnodes(MDI_Comm comm) {
  code* this_code = get_code(current_code);
  if ( this_code->intra_rank != 0 ) {
    mdi_error("Attempting to send callback information from the incorrect rank");
    return 1;
  }
  int nnodes = (int)this_code->nodes->size;
  int ret = general_send( &nnodes, 1, MDI_INT, comm );
  return ret;
}


/*! \brief Get information about the nodes of a particular code
 *
 * If running with MPI, this function must be called only by rank \p 0.
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator associated with the connection to the sending code.
 */
int get_node_info(MDI_Comm comm) {
  size_t stride = MDI_COMMAND_LENGTH + 1;
  communicator* this = get_communicator(current_code, comm);
  char* current_node = malloc( MDI_COMMAND_LENGTH * sizeof(char) );

  // get the number of nodes
  int nnodes_int;
  MDI_Send_Command("<NNODES",comm);
  MDI_Recv(&nnodes_int, 1, MDI_INT, comm);
  size_t nnodes = nnodes_int;

  // get the nodes
  size_t list_size = nnodes * stride * sizeof(char);
  char* node_list = malloc( list_size );
  MDI_Send_Command("<NODES",comm);
  MDI_Recv(node_list, (int)(nnodes * stride), MDI_CHAR, comm);

  // register the nodes
  int inode;
  int ichar;
  for (inode = 0; inode < nnodes; inode++) {
    // find the end of the node name
    char* name_start = &node_list[ inode * stride ];
    char* name_end = strchr( name_start, ' ' );
    int name_length;
    if (name_end == NULL) {
      name_length = MDI_COMMAND_LENGTH;
    }
    else {
      name_length = (int)(name_end - name_start);
    }
    if ( name_length >= MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse node name");
      return 1;
    }

    // construct the name of the node
    char* node_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    for (ichar = 0; ichar < name_length; ichar++) {
      node_name[ichar] = name_start[ichar];
    }
    for (ichar = name_length; ichar < MDI_COMMAND_LENGTH; ichar++) {
      node_name[ichar] = '\0';
    }

    // register this node
    register_node(this->nodes, node_name);

    // free the memory for the node name
    free( node_name );
  }

  // get the number of commands
  int ncommands;
  MDI_Send_Command("<NCOMMANDS",comm);
  MDI_Recv(&ncommands, 1, MDI_INT, comm);

  // get the list of commands
  int count = (int)( ( ncommands + nnodes ) * stride );
  char* commands = malloc( count * sizeof(char) );
  MDI_Send_Command("<COMMANDS",comm);
  MDI_Recv(commands, count, MDI_CHAR, comm);

  // register the commands
  int node_flag = 1;
  for (inode = 0; inode < nnodes + ncommands; inode++) {
    // find the end of the name
    char* name_start = &commands[ inode * stride ];
    char* name_end = strchr( name_start, ' ' );
    int name_length;
    if (name_end == NULL) {
      name_length = MDI_COMMAND_LENGTH;
    }
    else {
      name_length = (int)(name_end - name_start);
    }
    if ( name_length >= MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse command name");
      return 1;
    }

    // construct the name of the command
    char* command_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    for (ichar = 0; ichar < name_length; ichar++) {
      command_name[ichar] = name_start[ichar];
    }
    for (ichar = name_length; ichar < MDI_COMMAND_LENGTH; ichar++) {
      command_name[ichar] = '\0';
    }

    if ( node_flag == 1 ) { // node
      // store the name of the current node
      snprintf(current_node, strlen(command_name)+1, "%s", command_name);
    }
    else { // command
      // register this command
      register_command(this->nodes, current_node, command_name);
    }

    // determine whether the next name is for a node or a command
    if ( name_start[stride - 1] == ';' ) {
      node_flag = 1;
    }
    else if ( name_start[stride - 1] == ',' ) {
      node_flag = 0;
    }
    else {
      mdi_error("Error obtaining node information: could not parse delimiter");
      return 1;
    }

    // free the memory for the node name
    free( command_name );
  }


  // get the number of callbacks
  int ncallbacks;
  MDI_Send_Command("<NCALLBACKS",comm);
  MDI_Recv(&ncallbacks, 1, MDI_INT, comm);

  // get the list of callbacks
  count = (int)( ( ncallbacks + nnodes ) * stride );
  char* callbacks = malloc( count * sizeof(char) );
  MDI_Send_Command("<CALLBACKS",comm);
  MDI_Recv(callbacks, count, MDI_CHAR, comm);
  //printf("~~~CALLBACKS: %d %s\n",ncallbacks,callbacks);

  // register the callbacks
  node_flag = 1;
  for (inode = 0; inode < nnodes + ncallbacks; inode++) {
    // find the end of the name
    char* name_start = &callbacks[ inode * stride ];
    char* name_end = strchr( name_start, ' ' );
    int name_length;
    if (name_end == NULL) {
      name_length = MDI_COMMAND_LENGTH;
    }
    else {
      name_length = (int)(name_end - name_start);
    }
    if ( name_length >= MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse callback name");
      return 1;
    }

    // construct the name
    char* callback_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    for (ichar = 0; ichar < name_length; ichar++) {
      callback_name[ichar] = name_start[ichar];
    }
    for (ichar = name_length; ichar < MDI_COMMAND_LENGTH; ichar++) {
      callback_name[ichar] = '\0';
    }
    //printf("DRIVER CALLBACK: %d %d %s\n",inode,name_length,callback_name);

    if ( node_flag == 1 ) { // node
      // store the name of the current node
      snprintf(current_node, strlen(callback_name)+1, "%s", callback_name);
    }
    else { // callback
      // register this callback
      register_callback(this->nodes, current_node, callback_name);
    }

    // determine whether the next name is for a node or a callback
    if ( name_start[stride - 1] == ';' ) {
      node_flag = 1;
    }
    else if ( name_start[stride - 1] == ',' ) {
      node_flag = 0;
    }
    else {
      mdi_error("Error obtaining node information: could not parse delimiter");
      return 1;
    }

    // free the memory for the node name
    free( callback_name );
  }
  
  // free the memory
  free( node_list );
  free( commands );
  free( callbacks );
  free( current_node );

  return 0;
}


/*! \brief Get the node vector associated with a particular communicator
 *
 * The function returns the node vector for the communicator.
 *
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will return the node vector for the calling engine.
 */
vector* get_node_vector(MDI_Comm comm) {
  // get the vector of nodes associated with the communicator
  vector* node_vec;
  code* this_code = get_code(current_code);
  if ( comm == MDI_COMM_NULL ) {
    node_vec = this_code->nodes;
  }
  else {
    communicator* this = get_communicator(current_code, comm);
    if ( this->nodes->size == 0 ) {
      // acquire node information for this communicator
      get_node_info(comm);
    }
    node_vec = this->nodes;
  }
  return node_vec;
}
