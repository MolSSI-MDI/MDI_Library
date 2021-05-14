cmake_minimum_required(VERSION 3.7)

option(language "language")
option(libtype "libtype")
option(mpi "on or off" ON)
option(python_package "on or off" OFF)
option(python_plugins "on or off" ON)
option(python_version "python version number" OFF)
option(test_codes "on or off" OFF)

set(use_C "")
set(use_CXX "")
set(use_Fortran "")
set(use_Python "")

if( NOT language )
   # By default, compile for all languages
   set(use_C "C")
   set(use_CXX "CXX")
   set(use_Fortran "Fortran")
   set(use_Python "Python")
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
        VERSION 1.2.10
        LANGUAGES ${use_C} ${use_CXX} ${use_Fortran})


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
