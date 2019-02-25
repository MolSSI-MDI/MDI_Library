/*! \file
 *
 * \brief Global parameters used by the MolSSI Driver Interface
 */

#include "mdi_global.h"

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
