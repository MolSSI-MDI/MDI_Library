#check for MPI
find_package(MPI)
if( NOT MPI_FOUND )
   if( mpi STREQUAL "ON" )
      message( WARNING "Could not find MPI.  Compiling without MPI support." )
   endif()
   set(mpi "OFF")
endif()

#confirm that "language" is a valid value
if( language AND (NOT language STREQUAL "C") AND (NOT language STREQUAL "Fortran") AND (NOT language STREQUAL "Python") )
   message( FATAL_ERROR "Value of language not recognized. Accepted values are: C; Fortran; Python." )
endif()

#determine whether this is a SHARED or STATIC build
if( NOT libtype )
   set(libtype "SHARED")
elseif ( (NOT libtype STREQUAL "STATIC") AND (NOT libtype STREQUAL "SHARED") )
   message( FATAL_ERROR "Value of libtype not recognized. Accepted values are: SHARED; STATIC." )
endif()

#construct the list of source files
list(APPEND sources "mdi.c")
list(APPEND sources "communicator.h")
list(APPEND sources "communicator.c")
list(APPEND sources "mdi_manager.h")
list(APPEND sources "mdi_manager.c")
list(APPEND sources "method.h")
list(APPEND sources "method.c")
if( mpi STREQUAL "OFF" )
   list(APPEND sources "${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/mpi.h")
endif()
if( (NOT language) OR (language STREQUAL "Fortran") )
   list(APPEND sources "mdi_f90.f90")
endif()

add_library(mdi ${libtype}
                ${sources})

#include and link to MPI
if( mpi STREQUAL "ON" )

   #include MPI
   target_include_directories(mdi PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdi PRIVATE ${MPI_C_COMPILE_FLAGS})
   target_link_libraries(mdi ${MPI_C_LIBRARIES} ${MPI_C_LINK_FLAGS})

elseif( mpi STREQUAL "OFF" )

   message( "Compiling without MPI." )
   target_include_directories(mdi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/)

endif()

#do any Python-specific work
if( (NOT language) OR (language STREQUAL "Python") )

   if( libtype STREQUAL "STATIC" )
      if( NOT language )
         message( WARNING "STATIC builds are not compatible with Python." )
      else()
         message( FATAL_ERROR "STATIC builds are not compatible with Python." )
      endif()
   endif()

   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi.py ${CMAKE_CURRENT_BINARY_DIR}/mdi.py COPYONLY)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/__init__.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py COPYONLY)

   # Write the name of the mdi library, for use by mdi.py
   file(GENERATE
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mdi_name
      CONTENT $<TARGET_FILE_NAME:mdi>
   )

endif()

#copy the mdi.h header file into the binary directory
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi.h ${CMAKE_CURRENT_BINARY_DIR}/mdi.h COPYONLY)
