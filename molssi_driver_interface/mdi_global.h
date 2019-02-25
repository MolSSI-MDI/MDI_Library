#ifndef MDI_GLOBAL
#define MDI_GLOBAL

#ifdef __cplusplus
extern "C" {
#endif

// type of an MDI communicator handle
typedef int MDI_Comm;

// type of an MDI datatype handle
typedef int MDI_Datatype;

// length of an MDI command in characters
extern const int MDI_COMMAND_LENGTH;

// length of an MDI name in characters
extern const int MDI_NAME_LENGTH;

// value of a null communicator
extern const MDI_Comm MDI_NULL_COMM;

// MDI data types
extern const int MDI_INT;
extern const int MDI_DOUBLE;
extern const int MDI_CHAR;
extern const int MDI_INT_NUMPY;
extern const int MDI_DOUBLE_NUMPY;

// MDI communication types
extern const int MDI_TCP;
extern const int MDI_MPI;

/*----------------------*/
/* MDI unit conversions */
/*----------------------*/

// length
extern const double MDI_METER_TO_BOHR;
extern const double MDI_ANGSTROM_TO_BOHR;

// time
extern const double MDI_SECOND_TO_AUT;
extern const double MDI_PICOSECOND_TO_AUT;

// force
extern const double MDI_NEWTON_TO_AUF;

// energy
extern const double MDI_JOULE_TO_HARTREE;
extern const double MDI_KJ_TO_HARTREE;
extern const double MDI_KJPERMOL_TO_HARTREE;
extern const double MDI_KCALPERMOL_TO_HARTREE;
extern const double MDI_EV_TO_HARTREE;
extern const double MDI_RYDBERG_TO_HARTREE;
extern const double MDI_KELVIN_TO_HARTREE;

#ifdef __cplusplus
}
#endif

#endif
