# Macro to convert strings to lists
macro(string_to_list _VAR _STR)
    STRING(REPLACE "  " " " ${_VAR} "${_STR}")
    STRING(REPLACE " " ";" ${_VAR} "${_STR}")
endmacro(string_to_list _VAR _STR)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Confirm that "language" is a valid value
if( language AND (NOT language STREQUAL "C") AND (NOT language STREQUAL "CXX") AND (NOT language STREQUAL "Fortran") AND (NOT language STREQUAL "Python") )
   message( FATAL_ERROR "Value of language not recognized. Accepted values are: C; CXX; Fortran; Python." )
endif()

# Set flag for whether to build plugins
if( ${plugins} )
  add_definitions(-D_MDI_PLUGIN_SUPPORT=1)
else()
  add_definitions(-D_MDI_PLUGIN_SUPPORT=0)
endif()

# Construct the list of source files
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
if( ${plugins} )
  list(APPEND sources "mdi_lib.h")
  list(APPEND sources "mdi_lib.c")

  # Either add Python Plugin support, or Python Plugin stubs
  if ( Python_Development_FOUND )
    list(APPEND sources "mdi_plug_py.h")
    list(APPEND sources "mdi_plug_py.c")
  else()
    message( WARNING "Could not find Python development libraries.  Compiling without Python plugin support." )
    list(APPEND sources "${CMAKE_CURRENT_SOURCE_DIR}/STUBS_Python/mdi_plug_py.h")
    list(APPEND sources "${CMAKE_CURRENT_SOURCE_DIR}/STUBS_Python/mdi_plug_py.c")
  endif()
endif()

# Add MPI stubs, if needed
if( mpi STREQUAL "OFF" )
   list(APPEND sources "${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/mpi.h")
endif()

# Add Fortran support, if requested
if( (NOT language) OR (language STREQUAL "Fortran") )
   list(APPEND sources "mdi_f90.F90")
endif()

# Set whether MDI should be built as a shared or static library
if ( libtype STREQUAL "SHARED" )
   set(BUILD_SHARED_LIBS ON)
endif()
if ( (NOT BUILD_SHARED_LIBS) AND MDI_Python )
   message( WARNING "Python support requires that the MDI Library be compiled as a shared library.  Turning on shared library build.  To disable this warning, set -DBUILD_SHARED_LIBS=ON or -DMDI_Python=OFF." )
   set(BUILD_SHARED_LIBS ON)
endif()
if (BUILD_SHARED_LIBS)
   set(mdi_STATIC_BUILD OFF)
else()
   set(mdi_STATIC_BUILD ON)
endif()

# Add the MDI target
add_library(mdi)

# Add the source files
target_sources(mdi
        PRIVATE ${sources})

# If this is a static library, should compile with position independent code
if ( mdi_STATIC_BUILD )
  set_target_properties(mdi PROPERTIES POSITION_INDEPENDENT_CODE ON)
endif()

# Set API version of MDI Library
set_target_properties(mdi PROPERTIES SOVERSION 1)  # bump whenever interface has changes or removals

# If this is a Windows build, link to ws2_32
if(WIN32)
  target_link_libraries(mdi PRIVATE wsock32 ws2_32)
endif()

# Link to libdl, which is used for the plugin system
if(NOT WIN32)
  target_link_libraries(mdi PRIVATE dl)
endif()

# gfortran has trouble identifying windows, so use CMake to set the appropriate defines
if(WIN32)
  add_definitions(-DMDI_WINDOWS=1)
else()
  add_definitions(-DMDI_WINDOWS=0)
endif()

# Include and link to MPI
if( mpi STREQUAL "ON" )

   # Include MPI
   string_to_list(MPI_C_COMPILE_OPTIONS   "${MPI_C_COMPILE_FLAGS}")
   string_to_list(MPI_C_LINK_OPTIONS      "${MPI_C_LINK_FLAGS}")

   target_include_directories(mdi PRIVATE ${MPI_C_INCLUDE_PATH})
   target_compile_options(mdi PRIVATE ${MPI_C_COMPILE_OPTIONS})
   target_link_libraries(mdi PRIVATE ${MPI_C_LIBRARIES} ${MPI_C_LINK_OPTIONS})

elseif( mpi STREQUAL "OFF" )

   message( "Compiling without MPI." )
   target_include_directories(mdi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/STUBS_MPI/)

else()

   message( FATAL_ERROR "Value of mpi (${mpi}) not recognized. Accepted values are: ON; OFF." )

endif()

# Include and link to Python
if( Python_Development_FOUND )

   target_include_directories(mdi PRIVATE ${Python_INCLUDE_DIRS})
   target_link_libraries(mdi PRIVATE ${Python_LIBRARIES})

else()

   target_include_directories(mdi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/STUBS_Python/)

endif()

# Do any Python-specific work
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




# ----------------------------------------------------------------------------------------------------
# Install

set(PN ${PROJECT_NAME})

# Determine where the files should be installed
if( python_package STREQUAL "ON" )

    execute_process(
        COMMAND python -c "import sysconfig; print(sysconfig.get_path('platlib'))"
        OUTPUT_VARIABLE _site_packages
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    set(PYTHON_SITE_PACKAGES "${_site_packages}" CACHE PATH INTERNAL)
    unset(_site_packages)

    set(MDI_INSTALL_INCLUDEDIR ${PYTHON_SITE_PACKAGES}/mdi)
    set(MDI_INSTALL_LIBDIR     ${PYTHON_SITE_PACKAGES}/mdi)

else()

    set(MDI_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR})
    set(MDI_INSTALL_LIBDIR     ${CMAKE_INSTALL_LIBDIR})

endif()

# Perform the installation
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/mdi.h
        DESTINATION ${MDI_INSTALL_INCLUDEDIR})
if( (NOT language) OR (language STREQUAL "Fortran") )
  if( python_package STREQUAL "OFF" )
install(CODE "
    # Check for both lowercase and uppercase versions of the file
    if(EXISTS \"${CMAKE_CURRENT_BINARY_DIR}/mdi.mod\")
        file(INSTALL \"${CMAKE_CURRENT_BINARY_DIR}/mdi.mod\" DESTINATION \"${CMAKE_INSTALL_PREFIX}/${MDI_INSTALL_INCLUDEDIR}\")
    elseif(EXISTS \"${CMAKE_CURRENT_BINARY_DIR}/MDI.mod\")
        file(INSTALL \"${CMAKE_CURRENT_BINARY_DIR}/MDI.mod\" DESTINATION \"${CMAKE_INSTALL_PREFIX}/${MDI_INSTALL_INCLUDEDIR}\")
    else()
        message(FATAL_ERROR \"Fortran module file not found\")
    endif()
")
  endif()
endif()
if( (NOT language) OR (language STREQUAL "Python") )
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/mdi.py
                ${CMAKE_CURRENT_BINARY_DIR}/mdi_name
                ${CMAKE_CURRENT_BINARY_DIR}/__init__.py
          DESTINATION ${MDI_INSTALL_LIBDIR})
endif()

install(TARGETS mdi
        EXPORT "${PN}Targets"
        RUNTIME DESTINATION ${MDI_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${MDI_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${MDI_INSTALL_LIBDIR})

# Provide support for packages
set(CMAKECONFIG_INSTALL_DIR "share/cmake/${PN}")
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/../cmake/${PN}Config.cmake.in"
                              "${CMAKE_CURRENT_BINARY_DIR}/${PN}Config.cmake"
                              INSTALL_DESTINATION ${CMAKECONFIG_INSTALL_DIR})
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/${PN}ConfigVersion.cmake
                                 VERSION ${${PN}_VERSION}
                                 COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PN}Config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${PN}ConfigVersion.cmake
        DESTINATION ${CMAKECONFIG_INSTALL_DIR})
install(EXPORT "${PN}Targets"
        NAMESPACE "${PN}::"
        DESTINATION ${CMAKECONFIG_INSTALL_DIR})
