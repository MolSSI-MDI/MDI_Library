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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "mdi.h"
#include "mdi_global.h"
#include "mdi_general.h"
#include "mdi_mpi.h"

/*! \brief MDI version number */
const double MDI_VERSION = 0.5;

/*! \brief length of an MDI command in characters */
const int MDI_COMMAND_LENGTH = 12;

/*! \brief length of an MDI name in characters */
const int MDI_NAME_LENGTH = 12;

/*! \brief value of a null communicator */
const MDI_Comm MDI_NULL_COMM = 0;

// MDI data types
/*! \brief integer data type */
const int MDI_INT          = 0;
/*! \brief double precision float data type */
const int MDI_DOUBLE       = 1;
/*! \brief character data type */
const int MDI_CHAR         = 2;
/*! \brief NumPy integer data type */
const int MDI_INT_NUMPY    = 3;
/*! \brief NumPy double precision float data type */
const int MDI_DOUBLE_NUMPY = 4;

// MDI communication types
/*! \brief TCP/IP communication method */
const int MDI_TCP    = 1;
/*! \brief MPI communication method */
const int MDI_MPI    = 2;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
/*! \brief conversion factor between meters and bohr */
const double MDI_METER_TO_BOHR = 1.88972612546e10;
/*! \brief conversion factor between Angstroms and bohr */
const double MDI_ANGSTROM_TO_BOHR = 1.88972612546;

// time
/*! \brief conversion factor between seconds and atomic units of time */
const double MDI_SECOND_TO_AUT = 4.1341374575751e16;
/*! \brief conversion factor between picoseconds and atomic units of time */
const double MDI_PICOSECOND_TO_AUT = 4.1341374575751e4;

// force
/*! \brief conversion factor between newtons and atomic units of force */
const double MDI_NEWTON_TO_AUF = 1.213780478e7;

// energy
/*! \brief conversion factor between joules and hartrees */
const double MDI_JOULE_TO_HARTREE = 2.29371265835792e17;
/*! \brief conversion factor between kilojoules and hartrees */
const double MDI_KJ_TO_HARTREE = 2.29371265835792e20;
/*! \brief conversion factor between kilojoules/mol and hartrees */
const double MDI_KJPERMOL_TO_HARTREE = 3.80879947807451e-4;
/*! \brief conversion factor between kcal/mol and hartrees */
const double MDI_KCALPERMOL_TO_HARTREE = 1.5941730215480900e-3;
/*! \brief conversion factor between eV and hartrees */
const double MDI_EV_TO_HARTREE = 3.67493266806491e-2;
/*! \brief conversion factor between rydbergs and hartrees */
const double MDI_RYDBERG_TO_HARTREE = 0.5;
/*! \brief conversion factor between Kelvin and hartrees */
const double MDI_KELVIN_TO_HARTREE = 3.16681050847798e-6;


static int is_initialized = 0;


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
int MDI_Init(const char* options, void* world_comm)
{
  if ( is_initialized == 1 ) {
    mdi_error("MDI_Init called after MDI was already initialized");
  }
  general_init(options, world_comm);
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
  return general_accept_communicator();
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
  return general_send(buf, count, datatype, comm);
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
  return general_recv(buf, count, datatype, comm);
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
  return general_send_command(buf, comm);
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
  return general_recv_command(buf, comm);
}


/*! \brief Return a conversion factor between two units
 *
 * The function returns the conversion factor from \p in_unit to \p out_unit.
 * NOTE: This function is currently a placeholder.
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


/*! \brief Return order of this code within all codes represented in MPI_COMM_WORLD
 *
 * When using the MPI communication method, all processes across all codes are spawned 
 * as part of the same MPI_COMM_WORLD.
 * This funciton returns the order of the code associated with the calling process 
 * within MPI_COMM_WORLD.
 *
 */
int MDI_Get_MPI_Code_Rank()
{
  return mpi_code_rank;
}

/*! \brief Return the rank of the calling process within its associated code
 *
 * When using the MPI communication method, all processes across all codes are spawned 
 * as part of the same MPI_COMM_WORLD.
 * This funciton returns the rank of the calling process within the subset of processes
 * associated with the same code.
 *
 */
void MDI_Set_MPI_Intra_Rank(int rank)
{
  intra_rank = rank;
}
