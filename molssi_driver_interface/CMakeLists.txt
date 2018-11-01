#check for MPI
find_package(MPI)

if( (NOT language) OR (language STREQUAL "C") )

   add_library(mdi STATIC
                   mdi.c)

   #include MPI
   target_include_directories(mdi PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdi PRIVATE ${MPI_C_COMPILE_FLAGS})
   target_link_libraries(mdi ${MPI_C_LIBRARIES} ${MPI_C_LINK_FLAGS})

endif()

if( (NOT language) OR (language STREQUAL "Fortran") )
   add_library(mdif90 STATIC
                   mdi.c
                   mdi_f90.f90)

   #include MPI
   target_include_directories(mdif90 PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdif90 PRIVATE ${MPI_C_COMPILE_FLAGS})
   target_link_libraries(mdif90 ${MPI_C_LIBRARIES} ${MPI_C_LINK_FLAGS})
endif()

if( (NOT language) OR (language STREQUAL "Python") )

   add_library(mdipython SHARED
                   mdi.c)

   #include MPI
   target_include_directories(mdipython PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdipython PRIVATE ${MPI_C_COMPILE_FLAGS})
   target_link_libraries(mdipython ${MPI_C_LIBRARIES} ${MPI_C_LINK_FLAGS})

   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi_python.py ${CMAKE_CURRENT_BINARY_DIR}/mdi_python.py COPYONLY)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/__init__.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py COPYONLY)

   # Write the name of the mdi library, for use by mdi_python.py
   file(GENERATE
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mdi_name
      CONTENT $<TARGET_FILE_NAME:mdipython>
   )

endif()
