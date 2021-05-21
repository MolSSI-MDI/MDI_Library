# Include the MDI library directory when linking

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )

link_directories( ${CMAKE_BINARY_DIR} )
include_directories( ${CMAKE_BINARY_DIR} )

# Include the engine library directory when linking

link_directories( ${CMAKE_BINARY_DIR}/MDI_Test_Codes/lib_f90 )
include_directories( ${CMAKE_BINARY_DIR}/MDI_Test_Codes/lib_f90 )

# gfortran has trouble identifying windows, so use CMake to set the appropriate defines
if(WIN32)
  add_definitions(-DMDI_WINDOWS=1)
else()
  add_definitions(-DMDI_WINDOWS=0)
endif()


# ----------------------------------------------------------------------------------------------------
# Compile the engine
add_library(engine_lib_f90 SHARED
            engine_lib_f90.F90)
target_link_libraries(engine_lib_f90 mdi)
set_target_properties(engine_lib_f90 PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

target_link_libraries(engine_lib_f90 mdi)
target_include_directories(engine_lib_f90 PUBLIC ${mdi_path})

link_against_mpi_fortran(engine_lib_f90)


# ----------------------------------------------------------------------------------------------------
# Compile the driver

add_executable(driver_lib_f90
               driver_lib_f90.F90)
target_link_libraries(driver_lib_f90 mdi
                      engine_lib_f90)
set_target_properties(driver_lib_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

target_link_libraries(driver_lib_f90 mdi)
target_include_directories(driver_lib_f90 PUBLIC ${mdi_path})

link_against_mpi_fortran(driver_lib_f90)
