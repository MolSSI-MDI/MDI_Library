#ifndef ENGINE_CXX
#define ENGINE_CXX

#include <mpi.h>

// ensure that symbols are exported to Windows .dll files
#ifdef _WIN32
  #define DllExport   __declspec( dllexport )
#else
  #define DllExport
#endif

extern "C" DllExport int MDI_Plugin_init_engine_cxx();
int initialize_mdi(MDI_Comm* comm_ptr);
int respond_to_commands(MDI_Comm comm, MPI_Comm mpi_world_comm);
int execute_command(const char* command, MDI_Comm comm, void* class_obj);

#endif
