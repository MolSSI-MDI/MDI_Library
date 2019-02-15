cmake_minimum_required(VERSION 3.5)

project(mdi)

option(language "language")
option(libtype "libtype")
option(mpi "on or off" ON)

set(use_CXX "")
set(use_Fortran "")
set(use_Python "")

if( NOT language )
   # By default, compile for all languages
   set(use_CXX "CXX")
   set(use_Fortran "Fortran")
   set(use_Python "Python")
elseif( language STREQUAL "CXX" )
   set(use_CXX "CXX")
elseif( language STREQUAL "Fortran" )
   set(use_CXX "CXX")
   set(use_Fortran "Fortran")
elseif( language STREQUAL "Python" )
   set(use_CXX "CXX")
   set(use_Python "")
else()
   message( FATAL_ERROR "Language not supported.  Supported languages: CXX, Fortran, Python" )
endif()

project(MDI
        LANGUAGES ${use_CXX} ${use_Fortran})

add_subdirectory(molssi_driver_interface)
