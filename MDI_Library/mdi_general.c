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
 * \param [in, out]  world_comm
 *                   On input, the MPI communicator that spans all of the codes.
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 *                   Only used if the "-method MPI" option is provided.
 */
int general_init(const char* options, void* world_comm) {
  // If this is the first time MDI has initialized, initialize the code vector
  if ( ! is_initialized ) {
    vector_init(&codes, sizeof(code));
  }

  // MDI assumes that each call to general_init corresponds to a new code, so create a new code now
  // Note that unless using the LIBRARY communication method, general_init should only be called once
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
  char* driver_name;
  char* language = ((char*)"");
  int has_role = 0;
  int has_method = 0;
  int has_name = 0;
  int has_hostname = 0;
  int has_port = 0;
  int has_driver_name = 0;
  int has_output_file = 0;

  // calculate argc
  char* argv_line = mdi_strdup(options);
  char* token = strtok(argv_line, " ");
  int argc = 0;
  while (token != NULL) {
    argc++;
    token = strtok(NULL," ");
  }

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
      snprintf(this_code->role, NAME_LENGTH, "%s", role);
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
      snprintf(this_code->name, NAME_LENGTH, "%s", argv[iarg+1]);
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
    //-driver_name
    else if (strcmp(argv[iarg],"-driver_name") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -driver_name option");
	return 1;
      }
      driver_name = argv[iarg+1];
      has_driver_name = 1;
      iarg += 2;
    }
    //_language
    else if (strcmp(argv[iarg],"_language") == 0) {
      if (iarg+2 > argc) {
	mdi_error("Error in MDI_Init: Argument missing from -_language option");
	return 1;
      }
      language = argv[iarg+1];
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
  if ( strcmp(method, "MPI") == 0 && strcmp(language, "Python") != 0 ) {

    int mpi_init_flag = 0;
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
      world_comm = &mdi_mpi_comm_world;
      initialized_mpi = 1;

    }

  }

  // get the MPI rank
  MPI_Comm mpi_communicator;
  int mpi_rank = 0;
  if ( world_comm == NULL ) {
    mpi_communicator = 0;
    mpi_rank = 0;
  }
  else {
    if ( strcmp(language, "Python") == 0 ) {
      // Python case
      mpi_rank = world_rank;
    }
    else if ( strcmp(language, "Fortran") == 0 ) {
      // Fortran case
      mpi_communicator = MPI_Comm_f2c( *(MPI_Fint*) world_comm );
      MPI_Comm_rank(mpi_communicator, &mpi_rank);
    }
    else {
      // C and C++ case
      mpi_communicator = *(MPI_Comm*) world_comm;
      MPI_Comm_rank(mpi_communicator, &mpi_rank);
    }

    // for now, set the intra rank to the world rank
    // if using MPI for communication, it may change this
    this_code->intra_rank = mpi_rank;
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
  if ( strcmp(language, "Python") == 0 ) {
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
      if ( mpi_rank == 0 ) {
	tcp_listen(port);
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
      if ( mpi_rank == 0 ) {
	tcp_request_connection(port, hostname);
      }
    }
    else if ( strcmp(method, "LINK") == 0 ) {
      if ( has_driver_name == 0 ) {
	mdi_error("Error in MDI_Init: -driver_name option not provided");
	return 1;
      }
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

  // set the MPI communicator correctly
  if ( mpi_initialized == 1 ) {
    if ( use_mpi4py == 0 ) {
      if ( strcmp(language, "Fortran") == 0 ) {
	mpi_communicator = MPI_Comm_f2c( *(MPI_Fint*) world_comm );
	mpi_update_world_comm( (void*) &mpi_communicator);
        MPI_Fint f_comm = MPI_Comm_c2f( mpi_communicator );
	MPI_Fint* f_comm_ptr = (MPI_Fint*) world_comm;
	*f_comm_ptr = f_comm;
      }
      else {
	mpi_update_world_comm(world_comm);
      }
    }
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
  // give the library method an opportunity to update the current code
  library_accept_communicator();

  // if MDI hasn't returned some connections, do that now
  code* this_code = get_code(current_code);
  if ( this_code->returned_comms < this_code->next_comm - 1 ) {
    this_code->returned_comms++;
    return this_code->returned_comms;
  }

  // check for any production codes connecting via TCP
  if ( tcp_socket > 0 ) {

    //accept a connection via TCP
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
    int send_datatype = header[2];
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
    if ( send_datatype != datatype ) {
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
  ret = this->recv(buf, count, datatype, comm, 2);
  if ( ret != 0 ) { return ret; }

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
  // ensure that the driver is the current code
  library_set_driver_current();

  communicator* this = get_communicator(current_code, comm);
  int method = this->method;

  int count = MDI_COMMAND_LENGTH;
  char* command = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
  int ret;

  snprintf(command, COMMAND_LENGTH, "%s", buf);
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
      mdi_error("Error in MDI_Send_Command: Unable to execute command through library");
      return ret;
    }
  }

  // if the command was "EXIT", delete this communicator
  if ( strcmp( command, "EXIT" ) == 0 ) {
    delete_communicator(current_code, comm);

    // if MDI called MPI_Init, and there are no more communicators, call MPI_Finalize now
    if ( initialized_mpi == 1 ) {
      code* this_code = get_code(current_code);
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
  code* this_code = get_code(current_code);
  // only receive on rank 0
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }
  int count = MDI_COMMAND_LENGTH;
  int datatype = MDI_CHAR;

  int ret = general_recv( buf, count, datatype, comm );
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
  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > COMMAND_LENGTH ) {
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
  snprintf(new_node.name, COMMAND_LENGTH, "%s", node_name);
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
  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > COMMAND_LENGTH ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // confirm that the command_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(command_name) > COMMAND_LENGTH ) {
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
  snprintf(new_command, COMMAND_LENGTH, "%s", command_name);
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
  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > COMMAND_LENGTH ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 1;
  }

  // confirm that the callback_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(callback_name) > COMMAND_LENGTH ) {
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
  snprintf(new_callback, COMMAND_LENGTH, "%s", callback_name);
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
    snprintf(&commands[ islot * stride ], COMMAND_LENGTH, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      snprintf(&commands[ islot * stride + ichar ], sizeof(char*), "%c", ' ');
    }
    if ( this_node->commands->size > 0 ) {
      snprintf(&commands[ islot * stride + stride - 1 ], sizeof(char*), "%c", ',');
    }
    else {
      snprintf(&commands[ islot * stride + stride - 1 ], sizeof(char*), "%c", ';');
    }
    islot++;

    // add the commands for this node
    for (icommand = 0; icommand < this_node->commands->size; icommand++) {
      char* command = vector_get(this_node->commands, icommand);
      length = (int)strlen(command);
      snprintf(&commands[ islot * stride ], COMMAND_LENGTH, "%s", command);
      for (ichar = length; ichar < stride-1; ichar++) {
	snprintf(&commands[ islot * stride + ichar ], sizeof(char*), "%c", ' ');
      }
      if ( icommand == this_node->commands->size - 1 ) {
	snprintf(&commands[ islot * stride + stride - 1 ], sizeof(char*), "%c", ';');
      }
      else {
	snprintf(&commands[ islot * stride + stride - 1 ], sizeof(char*), "%c", ',');
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

  // form the list of callbacks
  int islot = 0;
  for (inode = 0; inode < nnodes; inode++) {
    // add the name of this node to the list
    node* this_node = vector_get(this_code->nodes, inode);
    int length = (int)strlen(this_node->name);
    snprintf(&callbacks[ islot * stride ], COMMAND_LENGTH, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      snprintf(&callbacks[ islot * stride + ichar ], sizeof(char*), "%c", ' ');
    }
    if ( this_node->callbacks->size > 0 ) {
      snprintf(&callbacks[ islot * stride + stride - 1 ], sizeof(char*), "%c", ',');
    }
    else {
      snprintf(&callbacks[ islot * stride + stride - 1 ], sizeof(char*), "%c", ';');
    }
    islot++;

    // add the callbacks for this node
    for (icallback = 0; icallback < this_node->callbacks->size; icallback++) {
      char* callback = vector_get(this_node->callbacks, icallback);
      length = (int)strlen(callback);
      snprintf(&callbacks[ islot * stride ], COMMAND_LENGTH, "%s", callback);
      for (ichar = length; ichar < stride-1; ichar++) {
	snprintf(&callbacks[ islot * stride + ichar ], sizeof(char*), "%c", ' ');
      }
      if ( icallback == this_node->callbacks->size - 1 ) {
	snprintf(&callbacks[ islot * stride + stride - 1 ], sizeof(char*), "%c", ';');
      }
      else {
	snprintf(&callbacks[ islot * stride + stride - 1 ], sizeof(char*), "%c", ',');
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
    snprintf(&node_list[ inode * stride ], COMMAND_LENGTH, "%s", this_node->name);
    int ichar;
    for (ichar = length; ichar < stride-1; ichar++) {
      snprintf(&node_list[ inode * stride + ichar ], COMMAND_LENGTH, "%c", ' ');
    }
    snprintf(&node_list[ inode * stride + stride - 1 ], COMMAND_LENGTH, "%c", ',');
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
    if ( name_length > MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse node name");
      return 1;
    }

    // construct the name of the node
    char* node_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    snprintf(node_name, COMMAND_LENGTH, "%s", name_start);
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
    if ( name_length > MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse command name");
      return 1;
    }

    // construct the name
    char* command_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    snprintf(command_name, COMMAND_LENGTH, "%s", name_start);
    for (ichar = name_length; ichar < MDI_COMMAND_LENGTH; ichar++) {
      command_name[ichar] = '\0';
    }
    //printf("DRIVER COMMAND: %d %d %s\n",inode,name_length,command_name);

    if ( node_flag == 1 ) { // node
      // store the name of the current node
      snprintf(current_node, COMMAND_LENGTH, "%s", command_name);
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
    if ( name_length > MDI_COMMAND_LENGTH ) {
      mdi_error("Error obtaining node information: could not parse callback name");
      return 1;
    }

    // construct the name
    char* callback_name = malloc( MDI_COMMAND_LENGTH * sizeof(char) );
    snprintf(callback_name, COMMAND_LENGTH, "%s", name_start);
    for (ichar = name_length; ichar < MDI_COMMAND_LENGTH; ichar++) {
      callback_name[ichar] = '\0';
    }
    //printf("DRIVER CALLBACK: %d %d %s\n",inode,name_length,callback_name);

    if ( node_flag == 1 ) { // node
      // store the name of the current node
      snprintf(current_node, COMMAND_LENGTH, "%s", callback_name);
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
