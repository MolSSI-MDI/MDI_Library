cmake_minimum_required(VERSION 3.10)

# Options
option(MDI_CXX "Build with CXX suport - ON (DEFAULT) or OFF)" ON)
option(MDI_Fortran "Build with Fortran support - ON (DEFAULT) or OFF)" ON)
option(MDI_Python "Build with Python support - ON (DEFAULT) or OFF)" ON)
set(MDI_USE_MPI "" CACHE STRING "Flag to use MPI - ON (DEFAULT) or OFF")

option(python_package "on or off" OFF)
option(plugins "on or off" ON)
option(python_plugins "on or off" ${plugins})
option(python_version "python version number" OFF)
option(test_codes "on or off" OFF)
option(test_drivers "on or off" ${test_codes})
option(test_engines "on or off" ${test_codes})
option(debug "on or off" OFF)

# Deprecated Options
option(language "Deprecated; use MDI_CXX, MDI_Fortran, and/or MDI_Python instead.")
option(libtype "Deprecated; use BUILD_SHARED_LIBS instead.")
set(mpi ${MDI_USE_MPI} CACHE STRING "Deprecated; use MDI_USE_MPI instead.")

set(use_C "")
set(use_CXX "")
set(use_Fortran "")
set(use_Python "")

if( NOT language )
   # By default, compile for all languages
   set(use_C "C")
   if( MDI_CXX )
      set(use_CXX "CXX")
   endif()
   if( MDI_Fortran )
      set(use_Fortran "Fortran")
   endif()
   if( MDI_Python )
      set(use_Python "Python")
   endif()
elseif( language STREQUAL "C" )
   set(use_C "C")
elseif( language STREQUAL "CXX" )
   set(use_C "C")
   set(use_CXX "CXX")
elseif( language STREQUAL "Fortran" )
   set(use_C "C")
   set(use_Fortran "Fortran")
elseif( language STREQUAL "Python" )
   set(use_C "C")
   set(use_Python "Python")
else()
   message( FATAL_ERROR "Language not supported.  Supported languages: C, CXX, Fortran, Python" )
endif()

project(mdi
        VERSION 1.4.31
        LANGUAGES ${use_C} ${use_CXX} ${use_Fortran})

# Check for MPI
if ( mpi STREQUAL "ON" )
   find_package(MPI REQUIRED)
elseif( NOT ( mpi STREQUAL "OFF") )
   find_package(MPI)
   if ( MPI_FOUND )
      set( mpi "ON" )
   else()
      set( mpi "OFF" )
   endif()
endif()

# Set a definition to enable MDI debug mode
if( ${debug} )
  add_definitions(-D_MDI_DEBUG=1)
else()
  add_definitions(-D_MDI_DEBUG=0)
endif()

# Check for Python developement libraries, which are used for Python plugins
if( python_plugins )

   # Attempt to find a valid development version of Python
   if( python_version )
      # Find this specific Python version
      find_package(Python ${python_version} EXACT COMPONENTS Interpreter Development)
   else()
      # Find the latest Python version
      find_package(Python COMPONENTS Interpreter Development)
   endif()

   # Python plugins do not currently support PyPy
   if(Python_Development_FOUND AND "${Python_INTERPRETER_ID}" STREQUAL "PyPy")
      message( WARNING "Python Plugins are not currently supported for PyPy.  Disabling Python Plugins." )
      set(Python_Development_FOUND off)
   endif()

   # Python plugins do not currently support PyPy
   if(Python_Development_FOUND AND WIN32)
      message( WARNING "Python Plugins are not currently supported on Windows.  Disabling Python Plugins." )
      set(Python_Development_FOUND off)
   endif()

else()
   message( WARNING "Python Plugins have been disabled." )
endif()


add_subdirectory(MDI_Library)

# compile test codes
if ( test_codes )
   add_subdirectory(tests)
endif()
