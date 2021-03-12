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



# ----------------------------------------------------------------------------------------------------
# Executable



# Compile the engine

add_executable(engine_cxx
               engine_cxx.cpp)
target_link_libraries(engine_cxx mdi
                      ${MPI_LIBRARIES})
set_target_properties(engine_cxx PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")


# Ensure that MPI is properly linked

if(NOT MPI_FOUND)
   target_include_directories(engine_cxx PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/)
endif()
if(MPI_COMPILE_FLAGS)
   set_target_properties(engine_cxx PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()
if(MPI_LINK_FLAGS)
   set_target_properties(engine_cxx PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()


# ----------------------------------------------------------------------------------------------------
# Library



# Compile the engine

add_library(engine_cxx_lib SHARED
            engine_cxx.h
            engine_cxx.cpp)
set_target_properties(engine_cxx_lib PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    OUTPUT_NAME engine_cxx)
target_link_libraries(engine_cxx_lib mdi
                      ${MPI_LIBRARIES})


# Ensure that MPI is properly linked

if(NOT MPI_FOUND)
   target_include_directories(engine_cxx_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/)
endif()
if(MPI_COMPILE_FLAGS)
   set_target_properties(engine_cxx_lib PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()
if(MPI_LINK_FLAGS)
   set_target_properties(engine_cxx_lib PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()
