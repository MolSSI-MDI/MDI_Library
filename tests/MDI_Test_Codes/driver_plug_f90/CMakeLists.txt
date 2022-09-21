# Link to MDI

link_directories( ${mdi_lib_path} )
include_directories( ${mdi_include_path} )



# Compile the driver

add_executable(driver_plug_f90
               driver_plug_f90.f90
               mdi_implementation.f90)
set_target_properties(driver_plug_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

target_link_libraries(driver_plug_f90 mdi)
target_include_directories(driver_plug_f90 PUBLIC ${mdi_path})

link_against_mpi_fortran(driver_plug_f90)
