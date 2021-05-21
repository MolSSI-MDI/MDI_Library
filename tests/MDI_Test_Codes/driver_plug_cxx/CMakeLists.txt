#link_directories( ${CMAKE_BINARY_DIR} )
#include_directories( ${CMAKE_BINARY_DIR} )



# Compile the driver

add_executable(driver_plug_cxx
               driver_plug_cxx.cpp)
target_link_libraries(driver_plug_cxx PRIVATE mdi)
set_target_properties(driver_plug_cxx PROPERTIES
    LINKER_LANGUAGE CXX
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

link_against_mpi(driver_plug_cxx)
