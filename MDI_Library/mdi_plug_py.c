#include <mpi.h>
#include <string.h>
#include "mdi.h"
#include "mdi_global.h"
#include "mdi_plug_py.h"
#include <Python.h>

/*! \brief Flag whether the Python interpreter has been initialized */
int python_interpreter_initialized = 0;

/*! \brief Pointer to the original Python interpreter's dictionary.
 * Only used for Python plugins */
void* python_interpreter_dict;

int python_plugin_init( const char* engine_name, const char* engine_path, const char* options, void* engine_comm_ptr ) {
  // Initialize the Python interpreter
  // Because Python has problems with reinitialization, only initialize Python once
  if ( ! python_interpreter_initialized ) {
    Py_Initialize();
    PyObject* main_module = PyImport_AddModule("__main__");
    PyObject* original_dict = PyModule_GetDict(main_module);
    python_interpreter_dict = PyDict_Copy(original_dict);
    python_interpreter_initialized = 1;

    // free original_dict
    Py_DECREF(original_dict);
  }
  PyObject* main_dict = PyDict_Copy(python_interpreter_dict);

  // Open the Python script for the Engine
  FILE* engine_script = fopen(engine_path, "r");

  if( engine_script ) {
    printf("Opened engine script\n");

    // Set the python_plugin_mpi_world_ptr
    python_plugin_mpi_world_ptr = engine_comm_ptr;

    // Set the name to anything other than __main__
    // Also, ensure that sys.argv exists
    PyRun_String("import sys\n"
		 "sys.path.insert(0, '')\n"
		 "if not hasattr(sys, 'argv'):\n"
		 "    sys.argv = ['']\n"
		 "__name__ = '__mdi__'",
		 Py_file_input,
		 main_dict, main_dict);

    // Run the engine file, which will make the MDI_Plugin_init function available
    PyRun_File(engine_script, engine_path,
	       Py_file_input,
	       main_dict, main_dict);

    // Call the MDI_Plugin_init function for this plugin
    char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
    snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_init_%s()", engine_name);
    PyRun_String(plugin_init_name,
		 Py_file_input,
		 main_dict, main_dict);
    free( plugin_init_name );

  }
  
  // Close the script
  fclose( engine_script );

  // free main_dict
  Py_DECREF(main_dict);

  // Finalize the Python interpreter
  // Because Python has problems with reinitialization, only initialize Python once
  //Py_Finalize();

  return 0;
}
