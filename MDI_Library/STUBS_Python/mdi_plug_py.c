#include <stdio.h>

int python_plugin_init( const char* engine_name, const char* engine_path, const char* options, void* engine_comm_ptr ) {
  fprintf( stderr, "%s\n", "MDI is not compiled to support Python plugins" );
  return -1;
}
