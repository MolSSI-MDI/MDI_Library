set( mdi_include_path ${CMAKE_CURRENT_BINARY_DIR}/../MDI_Library )
if(WIN32)
  set( mdi_lib_path ${CMAKE_BINARY_DIR}/bin/mdi/MDI_Library/ )
else()
  set( mdi_lib_path ${CMAKE_BINARY_DIR}/lib/mdi/MDI_Library/ )
endif()
set( build_mdi OFF )

add_subdirectory(MDI_Test_Codes)
