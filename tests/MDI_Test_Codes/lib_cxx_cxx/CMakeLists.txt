# Link to MDI

#link_directories( ${mdi_lib_path} )
#include_directories( ${mdi_include_path} )

link_directories( ${CMAKE_BINARY_DIR} )
include_directories( ${CMAKE_BINARY_DIR} )



# ----------------------------------------------------------------------------------------------------
# Engine

# Compile the engine library
add_library(engine_lib_cxx_cxx SHARED
            engine_lib_cxx_cxx.h
            engine_lib_cxx_cxx.cpp)
set_target_properties(engine_lib_cxx_cxx PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
target_link_libraries(engine_lib_cxx_cxx PRIVATE mdi)

link_against_mpi(engine_lib_cxx_cxx)

# Copy the engine header file into the binary directory
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/engine_lib_cxx_cxx.h ${CMAKE_BINARY_DIR}/engine_lib_cxx_cxx.h COPYONLY)


# ----------------------------------------------------------------------------------------------------
# Driver


# Compile the driver

add_executable(driver_lib_cxx_cxx
               driver_lib_cxx_cxx.cpp)
target_link_libraries(driver_lib_cxx_cxx mdi
                      engine_lib_cxx_cxx)
set_target_properties(driver_lib_cxx_cxx PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

link_against_mpi(driver_lib_cxx_cxx)
