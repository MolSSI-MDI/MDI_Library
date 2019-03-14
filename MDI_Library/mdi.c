/*! \file
 *
 * \brief Functions callable by users of the MolSSI Driver Interface
 */



/* ----------------------------------------------------------------------
   MDI - MolSSI Driver Interface
   https://molssi.org/, Molecular Sciences Software Institute
   Taylor Barnes, tbarnes1@vt.edu
-------------------------------------------------------------------------

Contents:
   MDI_Init: Initialize MDI
   MDI_Accept_Communicator: Accepts a new MDI communicator
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
#include "communicator.h"
#include "mdi_manager.h"
#include "method.h"

// length of an MDI command in characters
const int MDI_COMMAND_LENGTH = 12;

// length of an MDI name in characters
const int MDI_NAME_LENGTH = 12;

// value of a null communicator
const MDI_Comm MDI_NULL_COMM = 0;

// MDI data types
const int MDI_INT          = 0;
const int MDI_DOUBLE       = 1;
const int MDI_CHAR         = 2;
const int MDI_INT_NUMPY    = 3;
const int MDI_DOUBLE_NUMPY = 4;

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


static int is_initialized = 0;


void mdi_error(const char* message) {
  perror(message);
  exit(1);
}


/*! \brief Initialize communication through the MDI library
 *
 * If using the "-method MPI" option, this function must be called by all ranks.
 * The function returns \p 0 on a success.
 *
 * \param [in]       options
 *                   Options describing the communication method used to connect to codes
 * \param [in, out]  world_comm
 *                   On input, the MPI communicator that spans all of the codes.
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 *                   Only used if the "-method MPI" option is provided.
 */
int MDI_Init(const char* options, void* world_comm)
{
  if ( is_initialized == 1 ) {
    mdi_error("MDI_Init called after MDI was already initialized");
  }
  manager_init(options, world_comm);
  is_initialized = 1;
  return 0;
}


/*! \brief Accept a new MDI communicator
 *
 * The function returns an MDI_Comm that describes a connection between two codes.
 * If no new communicators are available, the function returns \p MDI_NULL_COMM.
 *
 */
MDI_Comm MDI_Accept_Communicator()
{
  if ( is_initialized == 0 ) {
    mdi_error("MDI_Accept_Communicator called but MDI has not been initialized");
  }
  return manager_accept_communicator();
}


/*! \brief Send data through the MDI connection
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
int MDI_Send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm)
{
  if ( is_initialized == 0 ) {
    mdi_error("MDI_Send called but MDI has not been initialized");
  }
  return manager_send(buf, count, datatype, comm);
}


/*! \brief Receive data through the MDI connection
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
int MDI_Recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm)
{
  if ( is_initialized == 0 ) {
    mdi_error("MDI_Recv called but MDI has not been initialized");
  }
  return manager_recv(buf, count, datatype, comm);
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
int MDI_Send_Command(const char* buf, MDI_Comm comm)
{
  if ( is_initialized == 0 ) {
    mdi_error("MDI_Send_Command called but MDI has not been initialized");
  }
  return manager_send_command(buf, comm);
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
int MDI_Recv_Command(char* buf, MDI_Comm comm)
{
  if ( is_initialized == 0 ) {
    mdi_error("MDI_Recv_Command called but MDI has not been initialized");
  }
  return manager_recv_command(buf, comm);
}


/*! \brief Return a conversion factor between two units
 *
 * The function returns the conversion factor from \p in_unit to \p out_unit.
 *
 * \param [in]       in_unit
 *                   Name of the unit to convert from.
 * \param [in]       out_unit
 *                   Name of the unit to convert to.
 */
double MDI_Conversion_Factor(char* in_unit, char* out_unit)
{
  if ( strcmp(in_unit,"Angstrom") == 0 && strcmp(out_unit,"Bohr") == 0 ) {
    return MDI_ANGSTROM_TO_BOHR;
  }
  else {
    mdi_error("Unrecognized conversion requested in MDI_Conversion_Factor");
  }
  return 0.0;
}


int MDI_Get_MPI_Code_Rank()
{
  return mpi_code_rank;
}

void MDI_Set_MPI_Intra_Rank(int rank)
{
  intra_rank = rank;
}
