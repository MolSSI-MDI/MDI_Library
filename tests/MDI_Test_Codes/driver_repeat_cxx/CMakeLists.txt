# Compile the driver

add_executable(driver_repeat_cxx
               driver_repeat_cxx.cpp)
target_link_libraries(driver_repeat_cxx PRIVATE mdi)
set_target_properties(driver_repeat_cxx PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

link_against_mpi(driver_repeat_cxx)

