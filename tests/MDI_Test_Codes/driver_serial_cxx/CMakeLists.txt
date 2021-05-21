# Compile the driver

add_executable(driver_serial_cxx
               driver_serial_cxx.cpp)
target_link_libraries(driver_serial_cxx PRIVATE mdi)
set_target_properties(driver_serial_cxx PROPERTIES
    LINKER_LANGUAGE CXX
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

link_against_mpi(driver_serial_cxx)
