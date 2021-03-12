# Locate MPI

find_package(MPI)
if(MPI_Fortran_FOUND)
   include_directories(${MPI_Fortran_INCLUDE_PATH})
endif()



# Link to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



# ----------------------------------------------------------------------------------------------------
# Executable


# Compile the engine

add_executable(engine_f90
               engine_f90.f90
               mdi_implementation.f90)
set_target_properties(engine_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")



# Ensure that MPI is properly linked

if(NOT MPI_Fortran_FOUND)
   target_include_directories(engine_f90 PRIVATE ${CMAKE_BINARY_DIR}/tests/MDI_Test_Codes/STUBS_MPI/)
   target_link_libraries(engine_f90 mdi
                         mpi)
else()
   target_include_directories(engine_f90 PUBLIC
      ${mdi_path}
      ${MPI_Fortran_INCLUDE_PATH} )
   target_link_libraries(engine_f90 mdi
                         ${MPI_Fortran_LIBRARIES})
endif()
if(MPI_Fortran_COMPILE_FLAGS)
   set_target_properties(engine_f90 PROPERTIES
      COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
endif()
if(MPI_Fortran_LINK_FLAGS)
   set_target_properties(engine_f90 PROPERTIES
      LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
endif()



# ----------------------------------------------------------------------------------------------------
# Library


# Compile the engine library
add_library(engine_f90_lib SHARED
            engine_f90.f90
            mdi_implementation.f90)
target_link_libraries(engine_f90_lib mdi)
set_target_properties(engine_f90_lib PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    OUTPUT_NAME engine_f90)

if(NOT MPI_Fortran_FOUND)
   target_include_directories(engine_f90_lib PRIVATE ${CMAKE_BINARY_DIR}/tests/MDI_Test_Codes/STUBS_MPI/)
   target_link_libraries(engine_f90_lib mpi)
else()
   target_include_directories(engine_f90_lib PUBLIC
      ${MPI_Fortran_INCLUDE_PATH} )
   target_link_libraries(engine_f90_lib
                         ${MPI_Fortran_LIBRARIES})
endif()
if(MPI_Fortran_COMPILE_FLAGS)
   set_target_properties(engine_f90_lib PROPERTIES
      COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
endif()
if(MPI_Fortran_LINK_FLAGS)
   set_target_properties(engine_f90_lib PROPERTIES
      LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
endif()
