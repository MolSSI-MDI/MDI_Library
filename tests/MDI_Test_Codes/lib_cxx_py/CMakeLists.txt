# Activate C++11 Standard
set (CMAKE_CXX_STANDARD 11)

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
# Driver


# Compile the driver

add_executable(driver_lib_cxx_py
               driver_lib_cxx_py.cpp)
target_link_libraries(driver_lib_cxx_py PRIVATE mdi
                      ${MPI_LIBRARIES}
                      ${Python_LIBRARIES})
set_target_properties(driver_lib_cxx_py PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
target_include_directories(driver_lib_cxx_py PRIVATE ${Python_INCLUDE_DIRS})


# Ensure that MPI is properly linked

if(NOT MPI_FOUND)
   target_include_directories(driver_lib_cxx_py PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../STUBS_MPI/)
endif()
if(MPI_COMPILE_FLAGS)
   set_target_properties(driver_lib_cxx_py PROPERTIES
      COMPILE_FLAGS "${MPI_COMPILE_FLAGS}")
endif()
if(MPI_LINK_FLAGS)
   set_target_properties(driver_lib_cxx_py PROPERTIES
      LINK_FLAGS "${MPI_LINK_FLAGS}")
endif()


# ----------------------------------------------------------------------------------------------------
# Engine

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/engine_lib_cxx_py.py ${CMAKE_CURRENT_BINARY_DIR}/../../../engine_lib_cxx_py.py COPYONLY)

