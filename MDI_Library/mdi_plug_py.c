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

int print_traceback()
{
  // Function to print the Python traceback
  
  PyObject *ptype, *pvalue, *ptraceback;
  PyObject *traceback_module;
  char *c_traceback;

  PyErr_Fetch(&ptype, &pvalue, &ptraceback);

  traceback_module = PyImport_ImportModule("traceback");
  if (traceback_module != NULL) {
    PyObject *traceback_list, *empty_str, *traceback_pystr;

    traceback_list = PyObject_CallMethod(traceback_module,
					 "format_exception",
					 "OOO",
					 ptype,
					 pvalue == NULL ? Py_None : pvalue,
					 ptraceback == NULL ? Py_None : ptraceback);

#if PY_MAJOR_VERSION >= 3
    empty_str = PyUnicode_FromString("");
#else
    empty_str = PyString_FromString("");
#endif

    traceback_pystr = PyObject_CallMethod(empty_str, "join", "O", traceback_list);

#if PY_MAJOR_VERSION >= 3
    c_traceback = strdup(PyUnicode_AsUTF8(traceback_pystr));
#else
    c_traceback = strdup(PyString_AsString(traceback_pystr));
#endif

    Py_DECREF(traceback_list);
    Py_DECREF(empty_str);
    Py_DECREF(traceback_pystr);
    Py_DECREF(traceback_module);
  }
  else {
    mdi_error("Unable to print Python plugin traceback");
    return 1;
  }

  Py_DECREF(ptype);
  Py_XDECREF(pvalue);
  Py_XDECREF(ptraceback);

  // Print the traceback
  printf("%s\n", c_traceback);
  free(c_traceback);

  return 0;
}

int python_plugin_init( const char* engine_name, const char* engine_path, const char* options, void* engine_comm_ptr ) {
  // Initialize the Python interpreter
  // Because Python has problems with reinitialization, only initialize Python once
  if ( ! python_interpreter_initialized ) {
    Py_Initialize();
    if ( ! Py_IsInitialized() ) {
      mdi_error("Unable to initialize Python interpreter");
      return 1;
    }
    PyObject* main_module = PyImport_AddModule("__main__");
    PyObject* original_dict = PyModule_GetDict(main_module);
    python_interpreter_dict = PyDict_Copy(original_dict);
    python_interpreter_initialized = 1;
  }
  PyObject* main_dict = PyDict_Copy(python_interpreter_dict);

  // Open the Python script for the Engine
  FILE* engine_script = fopen(engine_path, "r");

  if( engine_script ) {

    // Set the python_plugin_mpi_world_ptr
    python_plugin_mpi_world_ptr = engine_comm_ptr;

    // Set the name to anything other than __main__
    // Also, ensure that sys.argv exists
    PyObject* sysrun = PyRun_String("import sys\n"
				    "sys.path.insert(0, '')\n"
				    "if not hasattr(sys, 'argv'):\n"
				    "    sys.argv = ['']\n"
				    "__name__ = '__mdi__'",
				    Py_file_input,
				    main_dict, main_dict);
    if ( PyErr_Occurred() ) {
      mdi_error("Unable to set system properties for Python plugin");
      print_traceback();
      return 1;
    }
    Py_XDECREF(sysrun);

    // Run the engine file, which will make the MDI_Plugin_init function available
    PyObject* filerun = PyRun_File(engine_script, engine_path,
				   Py_file_input,
				   main_dict, main_dict);
    if ( PyErr_Occurred() ) {
      mdi_error("Error when loading Python plugin file");
      print_traceback();
      return 1;
    }
    Py_XDECREF(filerun);

    // Call the MDI_Plugin_init function for this plugin
    char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
    snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "MDI_Plugin_init_%s()", engine_name);
    PyObject* initrun = PyRun_String(plugin_init_name,
				     Py_file_input,
				     main_dict, main_dict);
    free( plugin_init_name );
    if ( PyErr_Occurred() ) {
      mdi_error("Error when running MDI_Plugin_init function");
      print_traceback();
      return 1;
    }
    Py_XDECREF(initrun);

  }
  
  // Close the script
  fclose( engine_script );

  // free main_dict
  Py_DECREF(main_dict);

  // Finalize the Python interpreter
  // Because Python has problems with reinitialization, only initialize Python once
  //Py_DECREF(python_interpreter_dict);
  //Py_Finalize();

  return 0;
}
