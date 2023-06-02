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
#include "mdi_lib.h"
#include "physconst.h"
#include "elements.h"

/*! \brief MDI major version number */
const int MDI_MAJOR_VERSION = MDI_MAJOR_VERSION_;

/*! \brief MDI minor version number */
const int MDI_MINOR_VERSION = MDI_MINOR_VERSION_;

/*! \brief MDI patch version number */
const int MDI_PATCH_VERSION = MDI_PATCH_VERSION_;

/*! \brief length of an MDI command in characters */
const int MDI_COMMAND_LENGTH = MDI_COMMAND_LENGTH_;

/*! \brief length of an MDI name in characters */
const int MDI_NAME_LENGTH = MDI_NAME_LENGTH_;

/*! \brief length of an MDI label in characters */
const int MDI_LABEL_LENGTH = MDI_LABEL_LENGTH_;

/*! \brief value of a null communicator */
const MDI_Comm MDI_COMM_NULL = MDI_COMM_NULL_;

// MDI data types
/*! \brief integer data type */
const int MDI_INT          = MDI_INT_;
/*! \brief int8_t data type */
const int MDI_INT8_T       = MDI_INT8_T_;
/*! \brief int16_t data type */
const int MDI_INT16_T      = MDI_INT16_T_;
/*! \brief int32_t data type */
const int MDI_INT32_T      = MDI_INT32_T_;
/*! \brief int64_t data type */
const int MDI_INT64_T      = MDI_INT64_T_;
/*! \brief uint8_t data type */
const int MDI_UINT8_T      = MDI_UINT8_T_;
/*! \brief uint16_t data type */
const int MDI_UINT16_T     = MDI_UINT16_T_;
/*! \brief uint32_t data type */
const int MDI_UINT32_T     = MDI_UINT32_T_;
/*! \brief uint64_t data type */
const int MDI_UINT64_T     = MDI_UINT64_T_;
/*! \brief double precision float data type */
const int MDI_DOUBLE       = MDI_DOUBLE_;
/*! \brief character data type */
const int MDI_CHAR         = MDI_CHAR_;
/*! \brief single precision float data type */
const int MDI_FLOAT        = MDI_FLOAT_;
/*! \brief character data type */
const int MDI_BYTE         = MDI_BYTE_;

// MDI communication types
/*! \brief TCP/IP communication method */
const int MDI_TCP    = MDI_TCP_;
/*! \brief MPI communication method */
const int MDI_MPI    = MDI_MPI_;
/*! \brief Library communication method (deprecated) */
const int MDI_LINK   = MDI_LINK_;
/*! \brief Library communication method */
const int MDI_PLUGIN   = MDI_LINK_;
/*! \brief Test communication method */
const int MDI_TEST   = MDI_TEST_;

// MDI role types
/*! \brief Driver role type */
const int MDI_DRIVER    = MDI_DRIVER_;
/*! \brief Engine role type */
const int MDI_ENGINE    = MDI_ENGINE_;


/*! \brief Initialize communication through the MDI library
 *
 * If using the "-method MPI" option, this function must be called by all ranks.
 * The function returns \p 0 on a success.
 *
 * \param [in, out]  argc
 *                   Pointer to the number of arguments.
 * \param [in, out]  argv
 *                   Pointer to the argument vector.
 */
int MDI_Init(int* argc, char*** argv)
{
  int ret;

  // Check whether the --mdi option was provided
  int argc_in = *argc;
  char** argv_in = *argv;
  int iarg;
  int found_mdi = 0;
  for (iarg=0; iarg < argc_in; iarg++) {
    if (strcmp(argv_in[iarg],"-mdi") == 0) {
      found_mdi = 1;
    }
    else if (strcmp(argv_in[iarg],"--mdi") == 0) {
      found_mdi = 1;
    }
  }

  // The --mdi option was provided, so initialize MDI
  if ( found_mdi == 1 ) {

    ret = MDI_Init_code();
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init_code: MDI_Init_with_argv failed");
      return ret;
    }
    ret = MDI_Init_with_argv(argc, argv);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init: MDI_Init_with_argv failed");
      return ret;
    }

  }

  return 0;
}


/*! \brief Initialize communication through the MDI library
 *
 * If using the "-method MPI" option, this function must be called by all ranks.
 * The function returns \p 0 on a success.
 *
 * \param [in, out]  argc
 *                   Pointer to the number of arguments.
 * \param [in, out]  argv
 *                   Pointer to the argument vector.
 */
int MDI_Init_with_argv(int* argc, char*** argv)
{
  int argc_in = *argc;
  char** argv_in = *argv;

  // Extract the mdi options
  int iarg;
  int mdi_iarg = -1;
  for (iarg=0; iarg < argc_in; iarg++) {
    if (strcmp(argv_in[iarg],"-mdi") == 0) {
      mdi_iarg = iarg;
    }
    else if (strcmp(argv_in[iarg],"--mdi") == 0) {
      mdi_iarg = iarg;
    }
  }
  if ( mdi_iarg > argc_in - 2 ) {
    mdi_error("No argument to the -mdi option was provided");
    return 1;
  }

  if ( mdi_iarg >= 0 ) {
    // Initialize MDI
    int ret = general_init(argv_in[mdi_iarg + 1]);
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Init during call to general_init");
      return ret;
    }

    // pass out argc and argv, without the mdi-related options
    *argc = argc_in - 2;
    for (iarg=mdi_iarg+2; iarg < argc_in; iarg++) {
      argv_in[iarg - 2] = argv_in[iarg];
    }
  }
  else {
    // The -mdi argument was not provided, so don't initialize
    return 0;
  }
  return 0;
}


/*! \brief Initialize a code structure for the MDI library
 *
 * The function returns \p 0 on a success.
 *
 */
int MDI_Init_code()
{
  int ret = general_init_code();
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init_code");
    return ret;
  }

  ret = mdi_debug("[MDI:MDI_Init_code] New code ID: %lu\n", codes.current_key);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init_code: mdi_debug failed");
    return ret;
  }

  return 0;
}


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
int MDI_Init_with_options(const char* options)
{
  int ret;

  ret = mdi_debug("[MDI:MDI_Init_with_options] Options: %s\n", options);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init_with_options: mdi_debug failed");
    return ret;
  }

  ret = general_init(options);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Init_with_options");
    return ret;
  }
  return 0;
}


/*! \brief Indicates whether MDI_Init has been called
 *
 * \param [out]  flag
 *                   Flag is true if and only if MDI_Init has been called.
 */
int MDI_Initialized(int* flag)
{
  int ret;

  *flag = codes.initialized;

  return 0;
}

/*! \brief Accept a new MDI communicator
 *
 * The function returns an MDI_Comm that describes a connection between two codes.
 * If no new communicators are available, the function returns \p MDI_COMM_NULL.
 *
 */
int MDI_Accept_Communicator(MDI_Comm* comm)
{
  return MDI_Accept_communicator(comm);
}


/*! \brief Accept a new MDI communicator
 *
 * The function returns an MDI_Comm that describes a connection between two codes.
 * If no new communicators are available, the function returns \p MDI_COMM_NULL.
 *
 */
int MDI_Accept_communicator(MDI_Comm* comm)
{
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Accept_Communicator called but MDI has not been initialized");
    return 1;
  }
  *comm = general_accept_communicator();

  ret = mdi_debug("[MDI:MDI_Accept_communicator] Comm: %d\n", *comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Accept_Communicator: mdi_debug failed");
    return ret;
  }

  return 0;
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
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Send called but MDI has not been initialized");
    return 1;
  }

  ret = mdi_debug("[MDI:MDI_Send] count, datatype, comm: %d %d %d\n", count, datatype, comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Send: mdi_debug failed");
    return ret;
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
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Recv called but MDI has not been initialized");
    return 1;
  }

  ret = mdi_debug("[MDI:MDI_Recv] count, datatype, comm: %d %d %d\n", count, datatype, comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Recv: mdi_debug failed");
    return ret;
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
  return MDI_Send_command(buf, comm);
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
int MDI_Send_command(const char* buf, MDI_Comm comm)
{
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Send_Command called but MDI has not been initialized");
    return 1;
  }

  ret = mdi_debug("[MDI:MDI_Send_command] Command, comm: %s, %d\n", buf, comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Send_command: mdi_debug failed");
    return ret;
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
  return MDI_Recv_command(buf, comm);
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
int MDI_Recv_command(char* buf, MDI_Comm comm)
{
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Recv_Command called but MDI has not been initialized");
    return 1;
  }
  ret = general_recv_command(buf, comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Recv_command: general_recv_command failed");
    return ret;
  }

  ret = mdi_debug("[MDI:MDI_Recv_command] Command, comm: %s, %d\n", buf, comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Recv_command: mdi_debug failed");
    return ret;
  }

  return 0;
}


/*! \brief Determine the conversion factor between two units
 *
 * The function determines the conversion factor from \p in_unit to \p out_unit.
 * The function requires that \p in_unit and \p out_unit be members of the same category of unit (\em i.e. charge, energy, force, etc.).
 * For example, calling \p MDI_Conversion_Factor(\p "kilojoule_per_mol",\p "atomic_unit_of_energy") will return the conversion factor from kilojoule/mol to hartrees.
 * The function returns \p 0 on a success.
 *
 * All quantities communicated through MDI must be represented using atomic units.
 * When unit conversions are necessary, this function should be used to obtain the conversion factors, as this will ensure that all drivers and engines use conversion factors that are self-consistent across codes.
 * Use of conversion factors that are not self-consistent can result in numerical instabilities.
 *
 * The following is a list of the unit categories, along with the names of the units associated with each category:
 *
 * - charge
 *    - atomic_unit_of_charge
 *    - coulomb
 * - energy
 *    - atomic_unit_of_energy
 *    - calorie
 *    - electron_volt
 *    - hartree
 *    - inverse_meter_energy
 *    - joule
 *    - kelvin_energy
 *    - kilocalorie
 *    - kilocalorie_per_mol
 *    - kilojoule
 *    - kilojoule_per_mol
 *    - rydberg
 * - force
 *    - atomic_unit_of_force
 *    - newton
 * - length
 *    - angstrom
 *    - atomic_unit_of_length
 *    - bohr
 *    - meter
 *    - nanometer
 *    - picometer
 * - mass
 *    - atomic_mass_unit
 *    - atomic_unit_of_mass
 *    - gram
 *    - kilogram
 * - time
 *    - atomic_unit_of_time
 *    - picosecond
 *    - second
 *
 * All conversion factors were acquired from the <a href="https://physics.nist.gov/cuu/Constants/Table/allascii.txt">NIST CODATA</a>, 
 * except the conversion factors for calorie, kilocalorie, and kilocalorie_per_mol, which were obtained from the <a href="https://www.nist.gov/pml/nist-guide-si-appendix-b9-factors-units-listed-kind-quantity-or-field-science"> NIST Guide to the SI</a>.
 *
 * \param [in]       in_unit
 *                   Name of the unit to convert from.
 * \param [in]       out_unit
 *                   Name of the unit to convert to.
 * \param [out]      conv
 *                   Conversion factor from in_unit to out_unit
 */
int MDI_Conversion_Factor(const char* in_unit, const char* out_unit, double* conv)
{
  return MDI_Conversion_factor(in_unit, out_unit, conv);
}


/*! \brief Determine the conversion factor between two units
 *
 * The function determines the conversion factor from \p in_unit to \p out_unit.
 * The function requires that \p in_unit and \p out_unit be members of the same category of unit (\em i.e. charge, energy, force, etc.).
 * For example, calling \p MDI_Conversion_Factor(\p "kilojoule_per_mol",\p "atomic_unit_of_energy") will return the conversion factor from kilojoule/mol to hartrees.
 * The function returns \p 0 on a success.
 *
 * All quantities communicated through MDI must be represented using atomic units.
 * When unit conversions are necessary, this function should be used to obtain the conversion factors, as this will ensure that all drivers and engines use conversion factors that are self-consistent across codes.
 * Use of conversion factors that are not self-consistent can result in numerical instabilities.
 *
 * The following is a list of the unit categories, along with the names of the units associated with each category:
 *
 * - charge
 *    - atomic_unit_of_charge
 *    - coulomb
 * - energy
 *    - atomic_unit_of_energy
 *    - calorie
 *    - electron_volt
 *    - hartree
 *    - inverse_meter_energy
 *    - joule
 *    - kelvin_energy
 *    - kilocalorie
 *    - kilocalorie_per_mol
 *    - kilojoule
 *    - kilojoule_per_mol
 *    - rydberg
 * - force
 *    - atomic_unit_of_force
 *    - newton
 * - length
 *    - angstrom
 *    - atomic_unit_of_length
 *    - bohr
 *    - meter
 *    - nanometer
 *    - picometer
 * - mass
 *    - atomic_mass_unit
 *    - atomic_unit_of_mass
 *    - gram
 *    - kilogram
 * - time
 *    - atomic_unit_of_time
 *    - picosecond
 *    - second
 *
 * All conversion factors were acquired from the <a href="https://physics.nist.gov/cuu/Constants/Table/allascii.txt">NIST CODATA</a>, 
 * except the conversion factors for calorie, kilocalorie, and kilocalorie_per_mol, which were obtained from the <a href="https://www.nist.gov/pml/nist-guide-si-appendix-b9-factors-units-listed-kind-quantity-or-field-science"> NIST Guide to the SI</a>.
 *
 * \param [in]       in_unit
 *                   Name of the unit to convert from.
 * \param [in]       out_unit
 *                   Name of the unit to convert to.
 * \param [out]      conv
 *                   Conversion factor from in_unit to out_unit
 */
int MDI_Conversion_factor(const char* in_unit, const char* out_unit, double* conv)
{
  // Except where otherwise noted, all values are from:
  //   - https://physics.nist.gov/cuu/Constants/Table/allascii.txt

  // mass
  double atomic_unit_of_mass = 1.0;
  double kilogram = 1.0 / pc_atomic_unit_of_mass;
  double gram = kilogram / 1000.0;
  double atomic_mass_unit = kilogram * pc_atomic_mass_unit_kilogram_relationship;

  // charge
  double atomic_unit_of_charge = 1.0;
  double coulomb = 1.0 / pc_atomic_unit_of_charge;

  // energy
  double atomic_unit_of_energy = 1.0;
  double hartree = 1.0;
  double joule = 1.0 / pc_atomic_unit_of_energy;
  double kilojoule = joule * 1000.0;
  double kilojoule_per_mol = 1.0 / pc_hartree2kJmol;
  double calorie = joule * pc_calorie_joule_relationship;
  double kilocalorie = calorie * 1000.0;
  double kilocalorie_per_mol = 1.0 / pc_hartree2kcalmol;
  double electron_volt = pc_electron_volt_hartree_relationship;
  double rydberg = electron_volt * pc_Rydberg_constant_times_hc_in_eV;
  double kelvin_energy = pc_kelvin_hartree_relationship;
  double inverse_meter_energy = pc_inverse_meter_hartree_relationship;

  // force
  double atomic_unit_of_force = 1.0;
  double newton = 1.0 / pc_atomic_unit_of_force;

  // length
  double atomic_unit_of_length = 1.0;
  double bohr = 1.0;
  double meter = 1.0 / pc_atomic_unit_of_length;
  double nanometer = 1.0e-9 * meter;
  double picometer = 1.0e-12 * meter;
  double angstrom = 1.0 / pc_bohr2angstroms;

  // time
  double atomic_unit_of_time = 1.0;
  double second = 1.0 / pc_atomic_unit_of_time;
  double picosecond = 1.0e-12 * second;

  // categories of input and output units (i.e. length, force, time, etc.)
  //    0 - none
  //    1 - mass
  //    2 - charge
  //    3 - energy
  //    4 - force
  //    5 - length
  //    6 - time
  int in_category = 0;
  int out_category = 0;

  // conversion factors for input and output units
  double in_conv = 1.0;
  double out_conv = 1.0;

  // identify the input unit
  if ( strcmp( in_unit, "atomic_unit_of_mass" ) == 0 ) {
    in_category = 1;
    in_conv = atomic_unit_of_mass;
  }
  else if ( strcmp( in_unit, "kilogram" ) == 0 ) {
    in_category = 1;
    in_conv = kilogram;
  }
  else if ( strcmp( in_unit, "gram" ) == 0 ) {
    in_category = 1;
    in_conv = gram;
  }
  else if ( strcmp( in_unit, "atomic_mass_unit" ) == 0 ) {
    in_category = 1;
    in_conv = atomic_mass_unit;
  }
  else if ( strcmp( in_unit, "atomic_unit_of_charge" ) == 0 ) {
    in_category = 2;
    in_conv = atomic_unit_of_charge;
  }
  else if ( strcmp( in_unit, "coulomb" ) == 0 ) {
    in_category = 2;
    in_conv = coulomb;
  }
  else if ( strcmp( in_unit, "atomic_unit_of_energy" ) == 0 ) {
    in_category = 3;
    in_conv = atomic_unit_of_energy;
  }
  else if ( strcmp( in_unit, "hartree" ) == 0 ) {
    in_category = 3;
    in_conv = hartree;
  }
  else if ( strcmp( in_unit, "joule" ) == 0 ) {
    in_category = 3;
    in_conv = joule;
  }
  else if ( strcmp( in_unit, "kilojoule" ) == 0 ) {
    in_category = 3;
    in_conv = kilojoule;
  }
  else if ( strcmp( in_unit, "kilojoule_per_mol" ) == 0 ) {
    in_category = 3;
    in_conv = kilojoule_per_mol;
  }
  else if ( strcmp( in_unit, "calorie" ) == 0 ) {
    in_category = 3;
    in_conv = calorie;
  }
  else if ( strcmp( in_unit, "kilocalorie" ) == 0 ) {
    in_category = 3;
    in_conv = kilocalorie;
  }
  else if ( strcmp( in_unit, "kilocalorie_per_mol" ) == 0 ) {
    in_category = 3;
    in_conv = kilocalorie_per_mol;
  }
  else if ( strcmp( in_unit, "electron_volt" ) == 0 ) {
    in_category = 3;
    in_conv = electron_volt;
  }
  else if ( strcmp( in_unit, "rydberg" ) == 0 ) {
    in_category = 3;
    in_conv = rydberg;
  }
  else if ( strcmp( in_unit, "kelvin_energy" ) == 0 ) {
    in_category = 3;
    in_conv = kelvin_energy;
  }
  else if ( strcmp( in_unit, "inverse_meter_energy" ) == 0 ) {
    in_category = 3;
    in_conv = inverse_meter_energy;
  }
  else if ( strcmp( in_unit, "atomic_unit_of_force" ) == 0 ) {
    in_category = 4;
    in_conv = inverse_meter_energy;
  }
  else if ( strcmp( in_unit, "newton" ) == 0 ) {
    in_category = 4;
    in_conv = newton;
  }
  else if ( strcmp( in_unit, "atomic_unit_of_length" ) == 0 ) {
    in_category = 5;
    in_conv = atomic_unit_of_length;
  }
  else if ( strcmp( in_unit, "bohr" ) == 0 ) {
    in_category = 5;
    in_conv = bohr;
  }
  else if ( strcmp( in_unit, "meter" ) == 0 ) {
    in_category = 5;
    in_conv = meter;
  }
  else if ( strcmp( in_unit, "nanometer" ) == 0 ) {
    in_category = 5;
    in_conv = nanometer;
  }
  else if ( strcmp( in_unit, "picometer" ) == 0 ) {
    in_category = 5;
    in_conv = picometer;
  }
  else if ( strcmp( in_unit, "angstrom" ) == 0 ) {
    in_category = 5;
    in_conv = angstrom;
  }
  else if ( strcmp( in_unit, "atomic_unit_of_time" ) == 0 ) {
    in_category = 6;
    in_conv = atomic_unit_of_time;
  }
  else if ( strcmp( in_unit, "second" ) == 0 ) {
    in_category = 6;
    in_conv = second;
  }
  else if ( strcmp( in_unit, "picosecond" ) == 0 ) {
    in_category = 6;
    in_conv = picosecond;
  }
  else {
    mdi_error("Unit name not recognized");
    return 1;
  }

  // identify the output unit
  if ( strcmp( out_unit, "atomic_unit_of_mass" ) == 0 ) {
    out_category = 1;
    out_conv = atomic_unit_of_mass;
  }
  else if ( strcmp( out_unit, "kilogram" ) == 0 ) {
    out_category = 1;
    out_conv = kilogram;
  }
  else if ( strcmp( out_unit, "gram" ) == 0 ) {
    out_category = 1;
    out_conv = gram;
  }
  else if ( strcmp( out_unit, "atomic_mass_unit" ) == 0 ) {
    out_category = 1;
    out_conv = atomic_mass_unit;
  }
  else if ( strcmp( out_unit, "atomic_unit_of_charge" ) == 0 ) {
    out_category = 2;
    out_conv = atomic_unit_of_charge;
  }
  else if ( strcmp( out_unit, "coulomb" ) == 0 ) {
    out_category = 2;
    out_conv = coulomb;
  }
  else if ( strcmp( out_unit, "atomic_unit_of_energy" ) == 0 ) {
    out_category = 3;
    out_conv = atomic_unit_of_energy;
  }
  else if ( strcmp( out_unit, "hartree" ) == 0 ) {
    out_category = 3;
    out_conv = hartree;
  }
  else if ( strcmp( out_unit, "joule" ) == 0 ) {
    out_category = 3;
    out_conv = joule;
  }
  else if ( strcmp( out_unit, "kilojoule" ) == 0 ) {
    out_category = 3;
    out_conv = kilojoule;
  }
  else if ( strcmp( out_unit, "kilojoule_per_mol" ) == 0 ) {
    out_category = 3;
    out_conv = kilojoule_per_mol;
  }
  else if ( strcmp( out_unit, "calorie" ) == 0 ) {
    out_category = 3;
    out_conv = calorie;
  }
  else if ( strcmp( out_unit, "kilocalorie" ) == 0 ) {
    out_category = 3;
    out_conv = kilocalorie;
  }
  else if ( strcmp( out_unit, "kilocalorie_per_mol" ) == 0 ) {
    out_category = 3;
    out_conv = kilocalorie_per_mol;
  }
  else if ( strcmp( out_unit, "electron_volt" ) == 0 ) {
    out_category = 3;
    out_conv = electron_volt;
  }
  else if ( strcmp( out_unit, "rydberg" ) == 0 ) {
    out_category = 3;
    out_conv = rydberg;
  }
  else if ( strcmp( out_unit, "kelvin_energy" ) == 0 ) {
    out_category = 3;
    out_conv = kelvin_energy;
  }
  else if ( strcmp( out_unit, "inverse_meter_energy" ) == 0 ) {
    out_category = 3;
    out_conv = inverse_meter_energy;
  }
  else if ( strcmp( out_unit, "atomic_unit_of_force" ) == 0 ) {
    out_category = 4;
    out_conv = inverse_meter_energy;
  }
  else if ( strcmp( out_unit, "newton" ) == 0 ) {
    out_category = 4;
    out_conv = newton;
  }
  else if ( strcmp( out_unit, "atomic_unit_of_length" ) == 0 ) {
    out_category = 5;
    out_conv = atomic_unit_of_length;
  }
  else if ( strcmp( out_unit, "bohr" ) == 0 ) {
    out_category = 5;
    out_conv = bohr;
  }
  else if ( strcmp( out_unit, "meter" ) == 0 ) {
    out_category = 5;
    out_conv = meter;
  }
  else if ( strcmp( out_unit, "nanometer" ) == 0 ) {
    out_category = 5;
    out_conv = nanometer;
  }
  else if ( strcmp( out_unit, "picometer" ) == 0 ) {
    out_category = 5;
    out_conv = picometer;
  }
  else if ( strcmp( out_unit, "angstrom" ) == 0 ) {
    out_category = 5;
    out_conv = angstrom;
  }
  else if ( strcmp( out_unit, "atomic_unit_of_time" ) == 0 ) {
    out_category = 6;
    out_conv = atomic_unit_of_time;
  }
  else if ( strcmp( out_unit, "second" ) == 0 ) {
    out_category = 6;
    out_conv = second;
  }
  else if ( strcmp( out_unit, "picosecond" ) == 0 ) {
    out_category = 6;
    out_conv = picosecond;
  }
  else {
    mdi_error("Unit name not recognized");
    return 1;
  }

  // confirm that the two units belong to the same category (i.e. mass, energy, time, etc.)
  if ( in_category != out_category ) {
    mdi_error("The units are of two different types, and conversion is not possible.");
    return 2;
  }

  *conv = in_conv / out_conv;
  return 0;
}

/* \brief Convert an element name to its atomic number */
int MDI_String_to_atomic_number(const char* element_symbol, int* atomic_number)
{
  int i;
  int found_number = -1;

  for (i=0; i<118; i++){
    if ( strcmp(element_symbol, elements[i]) == 0 ) {

      // The atomic number is 1 + the index in the list.
      found_number = i + 1;
      break;
    };
  };

  if ( found_number == -1 ) {
    mdi_error("Unrecognized element name, unable to look up atomic number.");
    return 1;
  }
  else {
    *atomic_number = found_number;
    return 0;
  };

}

/*! \brief Get the role of the code
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]      role
 *                   Role of the code (either \p MDI_DRIVER or \p MDI_ENGINE)
 *
 */
int MDI_Get_Role(int* role)
{
  return MDI_Get_role(role);
}


/*! \brief Get the role of the code
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]      role
 *                   Role of the code (either \p MDI_DRIVER or \p MDI_ENGINE)
 *
 */
int MDI_Get_role(int* role)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_Role called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_role: get_current_code failed");
    return 1;
  }
  if (strcmp(this_code->role, "DRIVER") == 0) {
    *role = MDI_DRIVER;
  }
  else if (strcmp(this_code->role, "ENGINE") == 0) {
    *role = MDI_ENGINE;
  }
  else {
    mdi_error("Error in MDI_Get_Role: Unrecognized role");
    return 1;
  }
  return 0;
}


/*! \brief Get the communication method of a communicator
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]      method
 *                   Role of the code (either \p MDI_TCP, \p MDI_MPI, \p MDI_TEST, or \p MDI_PLUGIN)
 * \param [in]       comm
 *                   MDI communicator for which the library will return the communication method.
 *
 */
int MDI_Get_method(int* method, MDI_Comm comm)
{
  int ret;
  communicator* comm_obj;
  ret = get_communicator(codes.current_key, comm, &comm_obj);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_method: get_communicator failed");
    return 1;
  }
  *method = comm_obj->method_id;
  return 0;
}


/*! \brief Get the previously accepted MDI communicator at a specific index in the array of all communicators.
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]      comm
 *                   Value of the communicator.
 *                   If no communicator exists at the given index, returns \p MDI_COMM_NULL.
 * \param [in]       index
 *                   Request the i'th communicator in the list of accepted and valid communicators.
 *                   The list begins at \p 0.
 *
 */
int MDI_Get_communicator(MDI_Comm* comm, int index)
{
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_communicator: get_current_code failed");
    return 1;
  }
  if ( index >= this_code->comms->size || index < 0 ) {
    *comm = MDI_COMM_NULL;
  }
  else {
    communicator* comm_obj;
    ret = vector_get( this_code->comms, index, (void**)&comm_obj );
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Get_communicator: vector_get failed");
      return ret;
    }
    if ( comm_obj->is_accepted == 0 ) {
      // If the code hasn't accepted this communicator, return null
      *comm = MDI_COMM_NULL;
    }
    else {
      *comm = comm_obj->id;
    }
  }
  return 0;
}


/*! \brief Set the size of MPI_COMM_WORLD
 *
 * This function is only used if the linked program uses MPI4PY.
 *
 */
void MDI_Set_World_Size(int world_size_in)
{
  set_world_size(world_size_in);
}


/*! \brief Set the rank of this process within MPI_COMM_WORLD
 *
 * This function is only used if the linked program uses MPI4PY.
 *
 */
void MDI_Set_World_Rank(int world_rank_in)
{
  set_world_rank(world_rank_in);
}


/*! \brief Get the rank of this process within the MPI intra-communicator for the current code
 *
 * This function is only used by the Fortran wrapper
 *
 */
int MDI_Get_intra_rank(int intra_rank_out)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_intra_rank called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_intra_rank: get_current_code failed");
    return 1;
  }
  return this_code->intra_rank;
}


/*! \brief Register a node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 */
int MDI_Register_Node(const char* node_name)
{
  return MDI_Register_node(node_name);
}


/*! \brief Register a node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 */
int MDI_Register_node(const char* node_name)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Register_Node called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Register_node: get_current_code failed");
    return 1;
  }
  return register_node(this_code->nodes, node_name);
}


/*! \brief Check whether a node is supported on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the node is supported and 0 otherwise.
 */
int MDI_Check_Node_Exists(const char* node_name, MDI_Comm comm, int* flag)
{
  return MDI_Check_node_exists(node_name, comm, flag);
}


/*! \brief Check whether a node is supported on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the node is supported and 0 otherwise.
 */
int MDI_Check_node_exists(const char* node_name, MDI_Comm comm, int* flag)
{
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Check_Node_Exists called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_node_exists: get_current_code failed");
    return ret;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 2;
  }
  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_node_exists: get_node_vector failed"); 
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_node_exists: get_node_index failed"); 
    return ret;
  }
  if ( node_index == -1 ) {
    *flag = 0;
  }
  else {
    *flag = 1;
  }
  return 0;
}


/*! \brief Get the number of nodes on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      nnodes
 *                   On return, the number of nodes supported by the engine.
 */
int MDI_Get_NNodes(MDI_Comm comm, int* nnodes)
{
  return MDI_Get_nnodes(comm, nnodes);
}


/*! \brief Get the number of nodes on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      nnodes
 *                   On return, the number of nodes supported by the engine.
 */
int MDI_Get_nnodes(MDI_Comm comm, int* nnodes)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_NNodes called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_nnodes: get_current_code failed");
    return ret;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_nnodes: get_node_vector failed");
    return ret;
  }

  *nnodes = (int)node_vec->size;

  return 0;
}


/*! \brief Get the name of a node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       index
 *                   Index of the node on the specified engine.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the node
 */
int MDI_Get_Node(int index, MDI_Comm comm, char* name)
{
  return MDI_Get_node(index, comm, name);
}


/*! \brief Get the name of a node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       index
 *                   Index of the node on the specified engine.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the node
 */
int MDI_Get_node(int index, MDI_Comm comm, char* name)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_Node called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_node: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_nodes: get_node_vector failed");
    return ret;
  }

  node* ret_node;
  ret = vector_get( node_vec, index, (void**)&ret_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_node: vector_get failed");
    return ret;
  }
  if ( ret_node == NULL ) {
    mdi_error("MDI_Get_Node unable to find node");
    return 1;
  }
  snprintf(name, MDI_NAME_LENGTH, "%s", ret_node->name);
  return 0;
}


/*! \brief Register a command on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the command will be registered.
 * \param [in]       command_name
 *                   Name of the command.
 */
int MDI_Register_Command(const char* node_name, const char* command_name)
{
  return MDI_Register_command(node_name, command_name);
}


/*! \brief Register a command on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the command will be registered.
 * \param [in]       command_name
 *                   Name of the command.
 */
int MDI_Register_command(const char* node_name, const char* command_name)
{
  int ret;

  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Register_Command called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Register_command: get_current_code failed");
    return 1;
  }
  return register_command(this_code->nodes, node_name, command_name);
}


/*! \brief Check whether a command is supported on specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the command's node.
 * \param [in]       command_name
 *                   Name of the command.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the command is supported and 0 otherwise.
 */
int MDI_Check_Command_Exists(const char* node_name, const char* command_name, MDI_Comm comm, int* flag)
{
  return MDI_Check_command_exists(node_name, command_name, comm, flag);
}


/*! \brief Check whether a command is supported on specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the command's node.
 * \param [in]       command_name
 *                   Name of the command.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the command is supported and 0 otherwise.
 */
int MDI_Check_command_exists(const char* node_name, const char* command_name, MDI_Comm comm, int* flag)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Check_Command_Exists called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_command_exists: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 2;
  }

  // confirm that the command_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(command_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Cannot chcek command name with length greater than MDI_COMMAND_LENGTH");
    return 3;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_command_exists: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_command_exists: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("Could not find the node");
    return 1;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_command_exists: vector_get failed");
    return ret;
  }

  // find the command
  int command_index;
  ret = get_command_index(target_node, command_name, &command_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_command_exists: get_command_index failed");
    return ret;
  }
  if ( command_index == -1 ) {
    *flag = 0;
  }
  else {
    *flag = 1;
  }
  return 0;
}


/*! \brief Get the number of commands supported for a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      nnodes
 *                   On return, the number of commands supported on the specified engine
 *                   on the specified node.
 */
int MDI_Get_NCommands(const char* node_name, MDI_Comm comm, int* ncommands)
{
  return MDI_Get_ncommands(node_name, comm, ncommands);
}


/*! \brief Get the number of commands supported for a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      nnodes
 *                   On return, the number of commands supported on the specified engine
 *                   on the specified node.
 */
int MDI_Get_ncommands(const char* node_name, MDI_Comm comm, int* ncommands)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_NCommands called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncommands: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 2;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncommands: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncommands: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("Could not find the node");
    return 1;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncommands: vector_get failed");
    return ret;
  }

  *ncommands = (int)target_node->commands->size;
  return 0;
}


/*! \brief Get the name of a command on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the command is located.
 * \param [in]       index
 *                   Index of the command on the specified node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the command
 */
int MDI_Get_Command(const char* node_name, int index, MDI_Comm comm, char* name)
{
  return MDI_Get_command(node_name, index, comm, name);
}


/*! \brief Get the name of a command on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the command is located.
 * \param [in]       index
 *                   Index of the command on the specified node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the command
 */
int MDI_Get_command(const char* node_name, int index, MDI_Comm comm, char* name)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_Command called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_command: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_command: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_command: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("MDI_Get_Command could not find the requested node");
    return 1;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_command: vector_get failed for node");
    return ret;
  }

  if ( target_node->commands->size <= index ) {
    mdi_error("MDI_Get_Command failed because the command does not exist");
    return 1;
  }

  char* target_command;
  ret = vector_get( target_node->commands, index, (void**)&target_command );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_command: vector_get failed for command");
    return ret;
  }
  snprintf(name, MDI_NAME_LENGTH, "%s", target_command);
  return 0;
}


/*! \brief Register a callback on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the callback will be registered.
 * \param [in]       callback_name
 *                   Name of the callback.
 */
int MDI_Register_Callback(const char* node_name, const char* callback_name)
{
  return MDI_Register_callback(node_name, callback_name);
}


/*! \brief Register a callback on a specified node
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the callback will be registered.
 * \param [in]       callback_name
 *                   Name of the callback.
 */
int MDI_Register_callback(const char* node_name, const char* callback_name)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Register_Callback called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Register_callback: get_current_code failed");
    return 1;
  }
  return register_callback(this_code->nodes, node_name, callback_name);
}


/*! \brief Check whether a callback exists on specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the callbacks's node.
 * \param [in]       command_name
 *                   Name of the callback.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the callback is supported and 0 otherwise.
 */
int MDI_Check_Callback_Exists(const char* node_name, const char* callback_name, MDI_Comm comm, int* flag)
{
  return MDI_Check_callback_exists(node_name, callback_name, comm, flag);
}


/*! \brief Check whether a callback exists on specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the callbacks's node.
 * \param [in]       command_name
 *                   Name of the callback.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      flag
 *                   On return, 1 if the callback is supported and 0 otherwise.
 */
int MDI_Check_callback_exists(const char* node_name, const char* callback_name, MDI_Comm comm, int* flag)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Check_Callback_Exists called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_callback_exists: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 2;
  }

  // confirm that the callback_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(callback_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Cannot check callback name with length greater than MDI_COMMAND_LENGTH");
    return 3;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_callback_exists: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_callback_exists: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("Could not find the node");
    return 4;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_callback_exists: vector_get failed");
    return ret;
  }

  // find the callback
  int callback_index;
  ret = get_callback_index(target_node, callback_name, &callback_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_callback_exists: get_callback_index failed"); 
    return 1;
  }
  if ( callback_index == -1 ) {
    *flag = 0;
  }
  else {
    *flag = 1;
  }
  return 0;
}


/*! \brief Get the number of callbacks on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      ncallbacks
 *                   On return, the number of callbacks on the specified node
 *                   on the specified engine.
 */
int MDI_Get_NCallbacks(const char* node_name, MDI_Comm comm, int* ncallbacks)
{
  return MDI_Get_ncallbacks(node_name, comm, ncallbacks);
}


/*! \brief Get the number of callbacks on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      ncallbacks
 *                   On return, the number of callbacks on the specified node
 *                   on the specified engine.
 */
int MDI_Get_ncallbacks(const char* node_name, MDI_Comm comm, int* ncallbacks)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_NCallbacks called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncallbacks: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  // confirm that the node_name size is not greater than MDI_COMMAND_LENGTH
  if ( strlen(node_name) > MDI_COMMAND_LENGTH_ ) {
    mdi_error("Node name is greater than MDI_COMMAND_LENGTH");
    return 2;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncallbacks: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Check_node_exists: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("Could not find the node");
    return 3;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_ncallbacks: vector_get failed");
    return ret;
  }

  *ncallbacks = (int)target_node->callbacks->size;
  return 0;
}


/*! \brief Get the name of a callback on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the callback is located.
 * \param [in]       index
 *                   Index of the callback on the specified node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the callback
 */
int MDI_Get_Callback(const char* node_name, int index, MDI_Comm comm, char* name)
{
  return MDI_Get_callback(node_name, index, comm, name);
}


/*! \brief Get the name of a callback on a specified node on a specified engine
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       node_name
 *                   Name of the node on which the callback is located.
 * \param [in]       index
 *                   Index of the callback on the specified node.
 * \param [in]       comm
 *                   MDI communicator of the engine.  If comm is set to 
 *                   MDI_COMM_NULL, the function will check for the calling engine.
 * \param [out]      name
 *                   On return, the name of the callback
 */
int MDI_Get_callback(const char* node_name, int index, MDI_Comm comm, char* name)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_Get_Callback called but MDI has not been initialized");
    return 1;
  }

  // Only rank 0 should respond to this call
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_callback: get_current_code failed");
    return 1;
  }
  if ( this_code->intra_rank != 0 ) {
    return 0;
  }

  vector* node_vec;
  ret = get_node_vector(comm, &node_vec);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_callback: get_node_vector failed");
    return ret;
  }

  // find the node
  int node_index;
  ret = get_node_index(node_vec, node_name, &node_index);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_callback: get_node_index failed"); 
    return 1;
  }
  if ( node_index == -1 ) {
    mdi_error("MDI_Get_Command could not find the requested node");
    return 2;
  }
  node* target_node;
  ret = vector_get( node_vec, node_index, (void**)&target_node );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_callback: vector_get failed for node");
    return ret;
  }

  if ( target_node->callbacks->size <= index ) {
    mdi_error("MDI_Get_Command failed because the command does not exist");
    return 3;
  }

  char* target_callback;
  ret = vector_get( target_node->callbacks, index, (void**)&target_callback );
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_callback: vector_get failed for callback");
    return ret;
  }
  snprintf(name, MDI_NAME_LENGTH, "%s", target_callback);
  return 0;
}


/*! \brief Obtain the MPI communicator that spans the single code corresponding to the calling rank
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]  world_comm
 *                   On output, the MPI communicator that spans the single code corresponding to the calling rank.
 */
int MDI_MPI_get_world_comm(void* world_comm)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_MPI_get_world_comm called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_MPI_get_world_comm: get_current_code failed");
    return 1;
  }

  if ( this_code->language == MDI_LANGUAGE_PYTHON ) {
    mdi_error("MDI_MPI_get_world_comm was called by a Python code");
  }
  else if ( this_code->language == MDI_LANGUAGE_FORTRAN ) {
    MPI_Comm mpi_communicator = MPI_COMM_NULL;
    mpi_update_world_comm( (void*) &mpi_communicator );
    MPI_Fint f_comm = MPI_Comm_c2f( mpi_communicator );
    MPI_Fint* f_comm_ptr = (MPI_Fint*) world_comm;
    *f_comm_ptr = f_comm;
  }
  else if ( this_code->language == MDI_LANGUAGE_C ) {
    mpi_update_world_comm(world_comm);
  }
  else {
    mdi_error("MDI_MPI_get_world_comm was called by a code with an unrecognized language");
  }

  return 0;
}


/*! \brief Set the MPI communicator that spans the single code corresponding to the calling rank
 *
 * The function returns \p 0 on a success.
 *
 * \param [out]  world_comm
 *                   The MPI communicator that spans the single code corresponding to the calling rank.
 */
int MDI_MPI_set_world_comm(void* world_comm)
{
  int ret;
  if ( codes.initialized == 0 ) {
    mdi_error("MDI_MPI_set_world_comm called but MDI has not been initialized");
    return 1;
  }
  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_MPI_set_world_comm: get_current_code failed");
    return 1;
  }

  if ( this_code->language == MDI_LANGUAGE_PYTHON ) {
    mdi_error("MDI_MPI_set_world_comm was called by a Python code");
  }
  else if ( this_code->language == MDI_LANGUAGE_FORTRAN ) {
    MPI_Fint* f_comm_ptr = (MPI_Fint*) world_comm;
    MPI_Comm c_comm = MPI_Comm_f2c( *f_comm_ptr );
    this_code->intra_MPI_comm = c_comm;
  }
  else if ( this_code->language == MDI_LANGUAGE_C ) {
    this_code->intra_MPI_comm = *(MPI_Comm*) world_comm;
  }
  else {
    mdi_error("MDI_MPI_set_world_comm was called by a code with an unrecognized language");
  }

  return 0;
}


/*! \brief Launch an MDI plugin instance
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       plugin_name
 *                   Name of the plugin.
 * \param [in]       options
 *                   Command-line options for the plugin.
 * \param [in]       mpi_comm_ptr
 *                   Pointer to an MPI intra-communicator that spans all ranks that will run this plugin instance.
 * \param [in]       driver_node_callback
 *                   Function pointer to the driver code that will be executed on this engine.
 * \param [in]       driver_callback_object
 *                   Pointer to the object instance of which driver_node_callback is a class member.
 *                   Should be set void if driver_node_callback is not a member of a class.
 */
int MDI_Launch_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                      MDI_Driver_node_callback_t driver_node_callback,
                      void* driver_callback_object) {
#if _MDI_PLUGIN_SUPPORT == 1
  int ret;

  ret = mdi_debug("[MDI:MDI_Launch_plugin] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Launch_plugin: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Launch_plugin: get_current_code failed");
    return 1;
  }

  // If this is a Fortran code, convert the MPI communicator
  void* mpi_comm_ptr_use = mpi_comm_ptr;
  if ( this_code->language == MDI_LANGUAGE_FORTRAN ) {
    MPI_Fint* f_comm_ptr = (MPI_Fint*) mpi_comm_ptr;
    MPI_Comm c_comm = MPI_Comm_f2c( *f_comm_ptr );
    mpi_comm_ptr_use = (void*)(&c_comm);
  }

  ret = library_launch_plugin(plugin_name, options,
                                mpi_comm_ptr_use,
                                driver_node_callback,
                                driver_callback_object);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Launch_plugin: library_launch_plugin failed");
    return ret;
  }

  ret = mdi_debug("[MDI:MDI_Launch_plugin] Finished\n");
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Launch_plugin: third mdi_debug failed");
    return ret;
  }

  return 0;
#else
  mdi_error("MDI_Launch_plugin was called, but this build of the MDI Library was built without plugin support.");
  return 1;
#endif
}


/*! \brief Open an MDI plugin instance in the background
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       plugin_name
 *                   Name of the plugin.
 * \param [in]       options
 *                   Command-line options for the plugin.
 * \param [in]       mpi_comm_ptr
 *                   Pointer to an MPI intra-communicator that spans all ranks that will run this plugin instance.
 * \param [out]      mdi_comm_ptr
 *                   Pointer to an MDI communicator for communication with the launched plugin.
 */
int MDI_Open_plugin(const char* plugin_name, const char* options, void* mpi_comm_ptr,
                       MDI_Comm* mdi_comm_ptr) {
#if _MDI_PLUGIN_SUPPORT == 1
  int ret = library_open_plugin(plugin_name, options, mpi_comm_ptr,
                                   mdi_comm_ptr);
  return ret;
#else
  mdi_error("MDI_Open_plugin was called, but this build of the MDI Library was built without plugin support.");
  return 1;
#endif
}


/*! \brief Close an MDI plugin instance
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       plugin_name
 *                   Name of the plugin.
 * \param [in]       options
 *                   Command-line options for the plugin.
 * \param [in]       mpi_comm_ptr
 *                   Pointer to an MPI intra-communicator that spans all ranks that will run this plugin instance.
 * \param [out]      mdi_comm_ptr
 *                   Pointer to an MDI communicator for communication with the launched plugin.
 */
int MDI_Close_plugin(MDI_Comm mdi_comm) {
#if _MDI_PLUGIN_SUPPORT == 1
  int ret = library_close_plugin(mdi_comm);
  return ret;
#else
  mdi_error("MDI_Close_plugin was called, but this build of the MDI Library was built without plugin support.");
  return 1;
#endif
}


/*! \brief Set the callback MDI uses for MDI_Execute_Command
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       execute_command
 *                   Function pointer to the generic execute_command function
 */
int MDI_Set_Execute_Command_Func(int (*generic_command)(const char*, MDI_Comm, void*), void* class_object) {
  return MDI_Set_execute_command_func(generic_command, class_object);
}


/*! \brief Set the callback MDI uses for MDI_Execute_Command
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       execute_command
 *                   Function pointer to the generic execute_command function
 */
int MDI_Set_execute_command_func(int (*generic_command)(const char*, MDI_Comm, void*), void* class_object) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_execute_command_func: get_current_code failed");
    return 1;
  }
  this_code->execute_command_wrapper = generic_command;
  this_code->execute_command_obj = class_object;
  this_code->called_set_execute_command_func = 1;

  // loop over all library communicators, and set their value of execute_command
  // depending on the order the plugin calls MDI functions, this might instead happen
  //    when the library-side communicator is initialized
  int icomm;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm;
    ret = vector_get( this_code->comms, icomm, (void**)&comm );
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Set_execute_command_func: vector_get failed");
      return ret;
    }

    if ( comm->method_id == MDI_LINK ) {
      library_data* libd = (library_data*) comm->method_data;
      libd->shared_state->execute_command_wrapper = this_code->execute_command_wrapper;
      libd->shared_state->execute_command_obj = this_code->execute_command_obj;
    }
  }

  return 0;
}


int MDI_Set_plugin_state(void* state) {
  int ret;
  ret = mdi_debug("[MDI:MDI_Set_plugin_state] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_plugin_state: mdi_debug failed");
    return ret;
  }

  ret = MDI_Init_code();
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_plugin_state: MDI_Init_code failed");
    return ret;
  }

  return library_set_state(state);
}


/*! \brief Set the language-based callback for when a code is destroyed
 *
 * This is only intended to be used internally by the MDI Library
 *
 */
int MDI_Set_plugin_state_internal(void* state) {
  return library_set_state(state);
}


/*! \brief Set the language-based callback for when a code is destroyed
 *
 * This is currently only used by Fortran plugins
 *
 */
int MDI_Set_on_destroy_code(int (*func)(int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_on_destroy_code: get_current_code failed");
    return 1;
  }
  this_code->language_on_destroy = func;
  return 0;
}


/*! \brief Get the current code
 *
 */
int MDI_Get_Current_Code() {
  return codes.current_key;
}


/*! \brief Get plugin_argc
 *
 */
int MDI_Plugin_get_argc(int* argc_ptr) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Plugin_get_argc: get_current_code failed");
    return 1;
  }
  if ( *this_code->plugin_argc_ptr == -1 ) {
    mdi_error("MDI_Plugin_get_argc called, but plugin mode is not active.");
    return 1;
  }
  *argc_ptr = *this_code->plugin_argc_ptr;
  return 0;
}


/*! \brief Get plugin_argv
 *
 */
int MDI_Plugin_get_argv(char*** argv_ptr) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Plugin_get_argv: get_current_code failed");
    return 1;
  }
  if ( *this_code->plugin_argc_ptr == -1 ) {
    mdi_error("MDI_Plugin_get_argv called, but plugin mode is not active.");
    return 1;
  }
  *argv_ptr = *this_code->plugin_argv_ptr;
  return 0;
}


/*! \brief Get plugin_unedited_options
 *
 */
int MDI_Plugin_get_args(char** args_ptr) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Plugin_get_args: get_current_code failed");
    return 1;
  }
  if ( *this_code->plugin_argc_ptr == -1 ) {
    mdi_error("MDI_Plugin_get_args called, but plugin mode is not active.");
    return 1;
  }
  *args_ptr = *this_code->plugin_unedited_options_ptr;
  return 0;
}


/*! \brief Get a specific element from plugin_argv
 *
 */
int MDI_Plugin_get_arg(int index, char** arg_ptr) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Plugin_get_arg: get_current_code failed");
    return 1;
  }
  if ( *this_code->plugin_argc_ptr == -1 ) {
    mdi_error("MDI_Plugin_get_arg called, but plugin mode is not active.");
    return 1;
  }
  if ( index < 0 ) {
    mdi_error("MDI_Plugin_get_arg called with invalid value (<0) for index.");
    return 1;
  }
  if ( index > *this_code->plugin_argc_ptr ) {
    mdi_error("MDI_Plugin_get_arg called with invalid value (>argc) for index.");
    return 1;
  }
  *arg_ptr = (*this_code->plugin_argv_ptr)[index];
  return 0;
}


/*! \brief Get the Python plugin MPI communicator
 *
 */
int MDI_Get_python_plugin_mpi_world_ptr(void** python_plugin_mpi_world_ptr_ptr, void* state_in) {
  plugin_shared_state* this_state = (plugin_shared_state*) state_in;
  *python_plugin_mpi_world_ptr_ptr = this_state->mpi_comm_ptr;
  return 0;
}


/*! \brief Set the callback MDI uses for MPI_Recv when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_recv
 *                   Function pointer to the mpi4py_recv callback
 */
int MDI_Set_Mpi4py_Recv_Callback(int (*mpi4py_recv)(void*, int, int, int, MDI_Comm)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Recv_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_recv_callback = mpi4py_recv;
  return 0;
}


/*! \brief Set the callback MDI uses for MPI_Send when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_send
 *                   Function pointer to the mpi4py_send callback
 */
int MDI_Set_Mpi4py_Send_Callback(int (*mpi4py_send)(void*, int, int, int, MDI_Comm)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Send_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_send_callback = mpi4py_send;
  return 0;
}


/*! \brief Set the callback MDI uses for gathering MDI versions when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_allgather
 *                   Function pointer to the mpi4py_allgather callback
 */
int MDI_Set_Mpi4py_Allgather_Callback(int (*mpi4py_allgather)(void*, void*)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Allgather_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_allgather_callback = mpi4py_allgather;
  return 0;
}


/*! \brief Set the callback MDI uses for gathering code names when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_gather_names
 *                   Function pointer to the mpi4py_gather_names callback
 */
int MDI_Set_Mpi4py_Gather_Names_Callback(int (*mpi4py_gather_names)(void*, void*, int*, int*)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Gather_Names_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_gather_names_callback = mpi4py_gather_names;
  return 0;
}


/*! \brief Set the callback MDI uses for MPI_Split when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_split
 *                   Function pointer to the mpi4py_split callback
 */
int MDI_Set_Mpi4py_Split_Callback(int (*mpi4py_split)(int, int, MDI_Comm, int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Split_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_split_callback = mpi4py_split;
  return 0;
}


/*! \brief Set the callback MDI uses for MPI_Comm_rank when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_rank
 *                   Function pointer to the mpi4py_rank callback
 */
int MDI_Set_Mpi4py_Rank_Callback(int (*mpi4py_rank)(int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Rank_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_rank_callback = mpi4py_rank;
  return 0;
}

/*! \brief Set the callback MDI uses for MPI_Comm_size when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_size
 *                   Function pointer to the mpi4py_size callback
 */
int MDI_Set_Mpi4py_Size_Callback(int (*mpi4py_size)(int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Size_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_size_callback = mpi4py_size;
  return 0;
}


/*! \brief Set the callback MDI uses for MPI_Barrier when using mpi4py
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       mpi4py_barrier
 *                   Function pointer to the mpi4py_barrier callback
 */
int MDI_Set_Mpi4py_Barrier_Callback(int (*mpi4py_barrier)(int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Mpi4py_Barrier_Callback: get_current_code failed");
    return ret;
  }
  this_code->mpi4py_barrier_callback = mpi4py_barrier;
  return 0;
}


/*! \brief Set the callback MDI uses for MDI_Plugin_init when the driver is in Python
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       launch_plugin
 *                   Function pointer to the launch_plugin callback
 */
int MDI_Set_Launch_Plugin_Callback(int (*launch_plugin)(void*, void*, void*, int)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_Launch_Plugin_Callback: get_current_code failed");
    return ret;
  }
  this_code->py_launch_plugin_callback = launch_plugin;
  return 0;
}


/*! \brief Set the language of an MDI plugin
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       language
 *                   Language of the plugin
 */
int MDI_Set_plugin_language(int language, void* plugin_state) {
  plugin_shared_state* this_state = (plugin_shared_state*) plugin_state;
  this_state->engine_language = language;
  return 0;
}


/*! \brief Set the language execute_command function needed by a language wrapper
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       execute_command
 *                   Execute command callback
 */
int MDI_Set_language_execute_command(int (*execute_command)(void*, MDI_Comm, void*)) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_language_execute_command: get_current_code failed");
    return ret;
  }
  this_code->execute_command = execute_command;

  // loop over all library communicators, and set their value of execute_command
  // depending on the order the plugin calls MDI functions, this might instead happen
  //    when the library-side communicator is initialized
  int icomm;
  for (icomm = 0; icomm < this_code->comms->size; icomm++ ) {
    communicator* comm;
    ret = vector_get( this_code->comms, icomm, (void**)&comm );
    if ( ret != 0 ) {
      mdi_error("Error in MDI_Set_language_execute_command: vector_get failed");
      return ret;
    }

    if ( comm->method_id == MDI_LINK ) {
      library_data* libd = (library_data*) comm->method_data;
      libd->shared_state->execute_command = this_code->execute_command;
    }
  }

  return 0;
}



/*! \brief Get the language execute_command function needed by a language wrapper
 *
 * The function returns a function pointer to the execute_command function.
 *
 * \param [out]      execute_command
 *                   Execute command callback
 * \param [in]       comm
 *                   MDI communicator
 */
int MDI_Get_language_execute_command(MDI_Execute_command_callback_t* language_execute_command, MDI_Comm comm) {
  communicator* this_comm;
  int ret = get_communicator(codes.current_key, comm, &this_comm);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_language_execute_command: get_communicator failed");
    //return ret;
  }
  library_data* libd = (library_data*) this_comm->method_data;
  *language_execute_command = libd->shared_state->execute_command;
  return 0;
}


/*! \brief Get the language driver callback needed by a language wrapper
 *
 * The function returns a function pointer to the driver callback function.
 *
 * \param [out]      callback
 *                   Driver callback
 * \param [in]       comm
 *                   MDI communicator
 */
int MDI_Get_language_driver_callback(MDI_Driver_node_callback_f90_t* language_driver_callback) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Get_language_driver_callback: get_current_code failed");
    //return ret;
  }
  *language_driver_callback = this_code->driver_callback_f90;
  return 0;
}


/*! \brief Set the language-specific driver_callback function needed by a language wrapper
 *
 * The function returns \p 0 on a success.
 *
 * \param [in]       callback
 *                   Driver callback
 */
int MDI_Set_language_driver_callback(MDI_Driver_node_callback_f90_t callback) {
  int ret;

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in MDI_Set_language_driver_callback: get_current_code failed");
    return ret;
  }
  this_code->driver_callback_f90 = callback;
  return 0;
}
