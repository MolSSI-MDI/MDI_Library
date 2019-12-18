cmake_minimum_required(VERSION 3.5)

option(language "language")
option(libtype "libtype")
option(mpi "on or off" ON)
option(python_package "on or off" OFF)
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
   message( FATAL_ERROR "Language not supported.  Supported languages: C, Fortran, Python" )
endif()

project(mdi
        VERSION 0.6.5
        LANGUAGES ${use_C} ${use_CXX} ${use_Fortran})

add_subdirectory(MDI_Library)

# compile test codes
if ( test_codes )
   add_subdirectory(tests)
endif()
