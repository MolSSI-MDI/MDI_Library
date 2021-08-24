# Check for MPI

if ( NOT ( mpi STREQUAL "OFF") )
   find_package(MPI)
endif()
if( NOT MPI_FOUND )
   if( mpi STREQUAL "ON" )
      message( WARNING "Could not find MPI.  Compiling without MPI support." )
   endif()
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/mpi.h ${CMAKE_CURRENT_BINARY_DIR}/STUBS_MPI/mpi.h COPYONLY)
   set(mpi "OFF")
endif()



# Macro to link target to MPI

macro(link_against_mpi _TAR)
   if( mpi STREQUAL "ON" )

      #include MPI
      string_to_list(MPI_CXX_COMPILE_OPTIONS   "${MPI_CXX_COMPILE_FLAGS}")
      string_to_list(MPI_CXX_LINK_OPTIONS      "${MPI_CXX_LINK_FLAGS}")

      target_include_directories(${_TAR} PRIVATE ${MPI_CXX_INCLUDE_PATH})
      target_compile_options(${_TAR} PRIVATE ${MPI_CXX_COMPILE_OPTIONS})
      target_link_libraries(${_TAR} PRIVATE ${MPI_CXX_LIBRARIES} ${MPI_CXX_LINK_OPTIONS})

   elseif( mpi STREQUAL "OFF" )

      message( "Compiling without MPI." )
      target_include_directories(${_TAR} PRIVATE ${CMAKE_BINARY_DIR}/tests/MDI_Test_Codes/STUBS_MPI/)

   else()

      message( FATAL_ERROR "Value of mpi not recognized. Accepted values are: ON; OFF." )

   endif()
endmacro(link_against_mpi _TAR)



# Macro to link target to MPI (Fortran version)

macro(link_against_mpi_fortran _TAR)
   if( (mpi STREQUAL "ON") AND MPI_Fortran_FOUND )

      #include MPI
      string_to_list(MPI_Fortran_COMPILE_OPTIONS   "${MPI_Fortran_COMPILE_FLAGS}")
      string_to_list(MPI_Fortran_LINK_OPTIONS      "${MPI_Fortran_LINK_FLAGS}")

      target_include_directories(${_TAR} PRIVATE ${MPI_Fortran_INCLUDE_PATH})
      target_compile_options(${_TAR} PRIVATE ${MPI_Fortran_COMPILE_OPTIONS})
      target_link_libraries(${_TAR} ${MPI_Fortran_LIBRARIES} ${MPI_Fortran_LINK_OPTIONS})

   else()

      message( "Compiling without Fortran MPI." )
      target_include_directories(${_TAR} PRIVATE ${CMAKE_BINARY_DIR}/tests/MDI_Test_Codes/STUBS_MPI/)
      target_link_libraries(${_TAR} mpi)

   endif()
endmacro(link_against_mpi_fortran _TAR)



# Link everything to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



if ( use_CXX )
   add_subdirectory(driver_cxx)
   add_subdirectory(driver_serial_cxx)
   add_subdirectory(engine_cxx)
   add_subdirectory(driver_plug_cxx)
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
