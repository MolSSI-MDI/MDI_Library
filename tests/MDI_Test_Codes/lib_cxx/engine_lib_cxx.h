#ifndef ENGINE_LIB_CXX
#define ENGINE_LIB_CXX

// ensure that symbols are exported to Windows .dll files
#ifdef _WIN32
  #define DllExport   __declspec( dllexport )
#else
  #define DllExport
#endif

#include <mpi.h>
#include "mdi.h"

DllExport int engine_lib_cxx_create(MPI_Comm mpi_comm);
DllExport int execute_command(const char* command, MDI_Comm comm, void* class_obj);

#endif
