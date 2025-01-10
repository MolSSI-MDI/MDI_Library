cmake_minimum_required(VERSION 3.10)

# Options
option(MDI_CXX "Build with CXX suport - ON (DEFAULT) or OFF)" ON)
option(MDI_Fortran "Build with Fortran support - ON (DEFAULT) or OFF)" ON)
option(MDI_Python "Build with Python support - ON (DEFAULT) or OFF)" ON)
option(MDI_Python_PACKAGE "Flag to install MDI as a Python package" OFF)
option(MDI_PLUGINS "Flag to compile with support for MDI plugins" ON)
option(MDI_Python_PLUGINS "Flag to compile with support for MDI Python plugins" ${MDI_PLUGINS})
option(MDI_TEST_CODES "Flag to compile MDI test codes" OFF)
option(MDI_TEST_DRIVERS "Flag to compile MDI test drivers" ${MDI_TEST_CODES})
option(MDI_TEST_ENGINES "Flag to compile MDI test engines" ${MDI_TEST_CODES})
option(MDI_DEBUG "Flag to compile MDI in debug mode" OFF)
set(MDI_USE_MPI "" CACHE STRING "Flag to use MPI - ON or OFF")
set(MDI_Python_VERSION "OFF" CACHE STRING "When linking against a build of Python, MDI will attempt to link against a build of Python corresponding to this version number.")

# Deprecated Options
option(language "Deprecated; use MDI_CXX, MDI_Fortran, and/or MDI_Python instead.")
option(libtype "Deprecated; use BUILD_SHARED_LIBS instead.")
set(mpi ${MDI_USE_MPI} CACHE STRING "Deprecated; use MDI_USE_MPI instead.")
set(python_package ${MDI_Python_PACKAGE} CACHE STRING "Deprecated; use MDI_Python_PACKAGE instead.")
set(plugins "" CACHE STRING "Deprecated; use MDI_PLUGINS instead.")
if ( plugins STREQUAL "" )
   set(plugins ${MDI_PLUGINS})
else()
   set(MDI_PLUGINS ${plugins})
   set(MDI_Python_PLUGINS ${plugins})
endif()
set(python_plugins ${MDI_Python_PLUGINS} CACHE STRING "Deprecated; use MDI_Python_PLUGINS instead.")
set(python_version ${MDI_Python_VERSION} CACHE STRING "Deprecated; use MDI_Python_VERSION instead.")
set(test_codes "" CACHE STRING "Deprecated; use MDI_TEST_CODES instead.")
if( test_codes STREQUAL "" )
   set(test_codes ${MDI_TEST_CODES})
else()
   set(MDI_TEST_CODES ${test_codes})
   set(MDI_TEST_DRIVERS ${test_codes})
   set(MDI_TEST_ENGINES ${test_codes})
endif()
set(test_drivers ${MDI_TEST_DRIVERS} CACHE STRING "Deprecated; use MDI_TEST_DRIVERS instead.")
set(test_engines ${MDI_TEST_ENGINES} CACHE STRING "Deprecated; use MDI_TEST_ENGINES instead.")
set(debug ${MDI_DEBUG} CACHE STRING "Deprecated; use MDI_DEBUG instead.")

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
        VERSION 1.4.34
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
