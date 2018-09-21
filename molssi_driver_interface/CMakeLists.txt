if( (NOT language) OR (language STREQUAL "C") )
   add_library(mdi STATIC
                   mdi.c)
endif()

if( (NOT language) OR (language STREQUAL "Fortran") )
   add_library(mdif90 STATIC
                   mdi.c
                   mdi_f90.f90)
endif()
