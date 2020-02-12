# Locate MPI

find_package(MPI)
if(MPI_FOUND)
   include_directories(${MPI_INCLUDE_PATH})
else()
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/mpi.h ${CMAKE_CURRENT_BINARY_DIR}/../STUBS_MPI/mpi.h COPYONLY)
endif()



# Link to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



# Compile the driver

add_executable(driver_cxx_serial
               driver_cxx_serial.cpp)
target_link_libraries(driver_cxx_serial mdi
                      ${MPI_LIBRARIES})
set_target_properties(driver_cxx_serial PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
