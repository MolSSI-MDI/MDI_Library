if( (NOT language) OR (language STREQUAL "C") )
   add_library(mdi STATIC
                   mdi.c)
endif()

if( (NOT language) OR (language STREQUAL "Fortran") )
   add_library(mdif90 STATIC
                   mdi.c
                   mdi_f90.f90)
endif()

if( (NOT language) OR (language STREQUAL "Python") )

   add_library(mdipython SHARED
                   mdi.c)

   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/mdi_python.py ${CMAKE_CURRENT_BINARY_DIR}/mdi_python.py COPYONLY)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/__init__.py ${CMAKE_CURRENT_BINARY_DIR}/__init__.py COPYONLY)

   # Write the name of the mdi library, for use by mdi_python.py
   file(GENERATE
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mdi_name
      CONTENT $<TARGET_FILE_NAME:mdipython>
   )

endif()
