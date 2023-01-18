/*! \file
 *
 * \brief Implementation of library-based communication
 */

#ifndef MDI_PLUG_PY
#define MDI_PLUG_PY

int print_traceback();
int python_plugin_init( const char* engine_name, const char* engine_path, void* engine_comm_ptr, void* shared_state, int mode );

#endif
