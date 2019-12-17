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

link_directories( ${CMAKE_BINARY_DIR} )
include_directories( ${CMAKE_BINARY_DIR} )



# ----------------------------------------------------------------------------------------------------
# Engine

# Compile the engine library
add_library(engine_lib_cxx SHARED
            engine_lib_cxx.h
            engine_lib_cxx.cpp)
set_target_properties(engine_lib_cxx PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
target_link_libraries(engine_lib_cxx mdi
                      ${MPI_LIBRARIES})

# Copy the engine header file into the binary directory
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/engine_lib_cxx.h ${CMAKE_BINARY_DIR}/engine_lib_cxx.h COPYONLY)

if(NOT MPI_FOUND)
   target_include_directories(engine_lib_cxx PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/)
endif()
if(MPI_COMPILE_FLAGS)
   set_target_properties(engine_lib_cxx PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()
if(MPI_LINK_FLAGS)
   set_target_properties(engine_lib_cxx PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()


# ----------------------------------------------------------------------------------------------------
# Driver


# Compile the driver

add_executable(driver_lib_cxx
               driver_lib_cxx.cpp)
target_link_libraries(driver_lib_cxx mdi
                      ${MPI_LIBRARIES}
                      engine_lib_cxx)
set_target_properties(driver_lib_cxx PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")


# Ensure that MPI is properly linked

if(NOT MPI_FOUND)
   target_include_directories(driver_lib_cxx PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/)
endif()
if(MPI_COMPILE_FLAGS)
   set_target_properties(driver_lib_cxx PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()
if(MPI_LINK_FLAGS)
   set_target_properties(driver_lib_cxx PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()
