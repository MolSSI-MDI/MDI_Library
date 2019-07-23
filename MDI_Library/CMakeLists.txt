################################# MDI Project ##################################
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
set(PN ${PROJECT_NAME})


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
elseif ( (NOT libtype STREQUAL "STATIC") AND 
         (NOT libtype STREQUAL "SHARED") )
   message( FATAL_ERROR "Value of libtype not recognized. Accepted values are: SHARED; STATIC; PACKAGE." )
endif()

#construct the list of source files
list(APPEND sources "mdi.c")
list(APPEND sources "mdi_global.h")
list(APPEND sources "mdi_global.c")
list(APPEND sources "mdi_general.h")
list(APPEND sources "mdi_general.c")
list(APPEND sources "mdi_mpi.h")
list(APPEND sources "mdi_mpi.c")
list(APPEND sources "mdi_tcp.h")
list(APPEND sources "mdi_tcp.c")
list(APPEND sources "mdi_test.h")
list(APPEND sources "mdi_test.c")
if( mpi STREQUAL "OFF" )
   list(APPEND sources "${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/mpi.h")
endif()
if( (NOT language) OR (language STREQUAL "Fortran") )
   list(APPEND sources "mdi_f90.f90")
endif()

add_library(mdi ${libtype}
                ${sources})

#if this is a Windows build, link to ws2_32
if(WIN32)
  target_link_libraries(mdi wsock32 ws2_32)
endif()

#include and link to MPI
if( mpi STREQUAL "ON" )

   #include MPI
   target_include_directories(mdi PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdi PRIVATE ${MPI_C_COMPILE_FLAGS})
   target_link_libraries(mdi ${MPI_C_LIBRARIES} ${MPI_C_LINK_FLAGS})

elseif( mpi STREQUAL "OFF" )

   message( "Compiling without MPI." )
   target_include_directories(mdi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/)

else()

   message( FATAL_ERROR "Value of mpi not recognized. Accepted values are: ON; OFF." )

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
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi_mpi4py.py ${CMAKE_CURRENT_BINARY_DIR}/mdi_mpi4py.py COPYONLY)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/__init__.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py COPYONLY)

   # Write the name of the mdi library, for use by mdi.py
   file(GENERATE
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mdi_name
      CONTENT $<TARGET_FILE_NAME:mdi>
   )

endif()

#copy the mdi.h header file into the binary directory
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi.h ${CMAKE_CURRENT_BINARY_DIR}/mdi.h COPYONLY)




# ----------------------------------------------------------------------------------------------------
# Install

# Determine where the files should be installed
if( python_package STREQUAL "ON" )

    execute_process(
        COMMAND python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())"
        OUTPUT_VARIABLE _site_packages
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    set(PYTHON_SITE_PACKAGES "${_site_packages}" CACHE PATH INTERNAL)
    unset(_site_packages)

    set(MDI_INSTALL_INCLUDEDIR ${PYTHON_SITE_PACKAGES}/mdi)
    set(MDI_INSTALL_LIBDIR     ${PYTHON_SITE_PACKAGES}/mdi)

else()

    set(MDI_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR}/MDI_Library)
    set(MDI_INSTALL_LIBDIR     ${CMAKE_INSTALL_LIBDIR}/MDI_Library)

endif()

# Perform the installation
install(FILES ${CMAKE_BINARY_DIR}/MDI_Library/mdi.h
        DESTINATION ${MDI_INSTALL_INCLUDEDIR})
if( (NOT language) OR (language STREQUAL "Python") )
  install(FILES ${CMAKE_BINARY_DIR}/MDI_Library/mdi.py
                ${CMAKE_BINARY_DIR}/MDI_Library/mdi_name
                ${CMAKE_BINARY_DIR}/MDI_Library/__init__.py
                ${CMAKE_BINARY_DIR}/MDI_Library/mdi_mpi4py.py
          DESTINATION ${MDI_INSTALL_LIBDIR})
endif()
install(TARGETS mdi
        EXPORT "${PN}Targets"
        RUNTIME DESTINATION ${MDI_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${MDI_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${MDI_INSTALL_LIBDIR})
 
