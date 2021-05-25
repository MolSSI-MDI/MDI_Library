# ----------------------------------------------------------------------------------------------------
# Compile the executable

add_executable(engine_cxx
               engine_cxx.cpp)
target_link_libraries(engine_cxx PRIVATE mdi)
set_target_properties(engine_cxx PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

link_against_mpi(engine_cxx)


# ----------------------------------------------------------------------------------------------------
# Compile the library

add_library(engine_cxx_lib SHARED
            engine_cxx.h
            engine_cxx.cpp)
set_target_properties(engine_cxx_lib PROPERTIES
    LINKER_LANGUAGE CXX
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    OUTPUT_NAME engine_cxx)
target_link_libraries(engine_cxx_lib PRIVATE mdi)

link_against_mpi(engine_cxx_lib)
