# Locate MPI

find_package(MPI)
if(MPI_Fortran_FOUND)
   include_directories(${MPI_Fortran_INCLUDE_PATH})
endif()



# Link to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



# Compile the driver

add_executable(driver_f90
               driver_f90.f90)
set_target_properties(driver_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")



# Ensure that MPI is properly linked

if(NOT MPI_Fortran_FOUND)
   target_include_directories(driver_f90 PRIVATE ${CMAKE_BINARY_DIR}/tests/MDI_Test_Codes/STUBS_MPI/)
   target_link_libraries(driver_f90 mdi
                         mpi)
else()
   target_include_directories(driver_f90 PUBLIC
      ${mdi_path}
      ${MPI_Fortran_INCLUDE_PATH} )
   target_link_libraries(driver_f90 mdi
                         ${MPI_Fortran_LIBRARIES})
endif()
if(MPI_Fortran_COMPILE_FLAGS)
   set_target_properties(driver_f90 PROPERTIES
      COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
endif()
if(MPI_Fortran_LINK_FLAGS)
   set_target_properties(driver_f90 PROPERTIES
      LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
endif()
