#include <mpi.h>
#include <string.h>
#include "mdi.h"
#include "mdi_global.h"
#include <Python.h>

int python_plugin_init( const char* engine_name, const char* engine_path, const char* options, void* engine_comm_ptr ) {
  // Initialize the Python interpreter
  Py_Initialize();

  // Open the Python script for the Engine
  FILE* engine_script = fopen(engine_path, "r");

  if( engine_script ) {
    printf("Opened engine script\n");

    // Set the python_plugin_mpi_world_ptr
    python_plugin_mpi_world_ptr = engine_comm_ptr;

    // Get the global dictionary
    PyObject* main_module = PyImport_AddModule("__main__");
    PyObject* main_dict = PyModule_GetDict(main_module);

    // Ensure that the current working directory is in sys.path
    PyRun_SimpleString("import sys; sys.path.insert(0, '')");

    // Set the name to anything other than __main__
    PyRun_SimpleString("__name__ = '__mdi__'");

    // Run the engine file, which will make the MDI_Plugin_init function available
    PyRun_File(engine_script, engine_path,
	       Py_file_input,
	       main_dict, main_dict);

    // Call the MDI_Plugin_init function for this plugin
    char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
    snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_init_%s()", engine_name);
    PyRun_SimpleString(plugin_init_name);
    free( plugin_init_name );

  }
  
  // Close the script
  fclose( engine_script );

  // Finalize the Python interpreter
  Py_Finalize();

  return 0;
}
