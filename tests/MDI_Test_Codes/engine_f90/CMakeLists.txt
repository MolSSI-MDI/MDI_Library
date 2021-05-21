# Link to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



# ----------------------------------------------------------------------------------------------------
# Compile the executable


# Compile the engine

add_executable(engine_f90
               engine_f90.f90
               mdi_implementation.f90)
set_target_properties(engine_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

target_link_libraries(engine_f90 mdi)
target_include_directories(engine_f90 PUBLIC ${mdi_path})

link_against_mpi_fortran(engine_f90)



# ----------------------------------------------------------------------------------------------------
# Compile the library

add_library(engine_f90_lib SHARED
            engine_f90.f90
            mdi_implementation.f90)
target_link_libraries(engine_f90_lib mdi)
set_target_properties(engine_f90_lib PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
    OUTPUT_NAME engine_f90)

target_link_libraries(engine_f90_lib mdi)
target_include_directories(engine_f90_lib PUBLIC ${mdi_path})

link_against_mpi_fortran(engine_f90_lib)
