if ( use_CXX )
   add_subdirectory(driver_cxx)
   add_subdirectory(driver_serial_cxx)
   add_subdirectory(engine_cxx)
   add_subdirectory(lib_cxx)

   # Can't compile the i-PI test on Windows
if(NOT WIN32)
   add_subdirectory(engine_ipi_cxx)
endif()

endif()

if ( use_Fortran )
   # Fortran requires that the MPI stubs be compiled
   add_subdirectory(STUBS_MPI)

   add_subdirectory(driver_f90)
   add_subdirectory(engine_f90)
   add_subdirectory(lib_f90)
endif()

if ( use_Python )
   add_subdirectory(driver_py)
   add_subdirectory(driver_ipicomp_py)
   add_subdirectory(engine_py)
   add_subdirectory(lib_py)
   add_subdirectory(misc_py)
endif()
