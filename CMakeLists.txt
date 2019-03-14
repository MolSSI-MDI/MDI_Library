cmake_minimum_required(VERSION 3.5)

project(mdi)

option(language "language")
option(libtype "libtype")
option(mpi "on or off" ON)

set(use_C "")
set(use_Fortran "")
set(use_Python "")

if( NOT language )
   # By default, compile for all languages
   set(use_C "C")
   set(use_Fortran "Fortran")
   set(use_Python "Python")
elseif( language STREQUAL "C" )
   set(use_C "C")
elseif( language STREQUAL "Fortran" )
   set(use_C "C")
   set(use_Fortran "Fortran")
elseif( language STREQUAL "Python" )
   set(use_C "C")
   set(use_Python "")
else()
   message( FATAL_ERROR "Language not supported.  Supported languages: C, Fortran, Python" )
endif()

project(MDI
        LANGUAGES ${use_C} ${use_Fortran})

add_subdirectory(MDI_Library)
