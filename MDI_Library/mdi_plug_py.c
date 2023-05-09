#include <mpi.h>
#include <string.h>
#include "mdi.h"
#include "mdi_global.h"
#include "mdi_lib.h"
#include "mdi_plug_py.h"
#include <Python.h>
#include <stdint.h>

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

int python_plugin_init( const char* engine_name, const char* engine_path, void* engine_comm_ptr, void* shared_state, int mode ) {
  int ret;
  ret = mdi_debug("[MDI:python_plugin_init] Start\n");
  if ( ret != 0 ) {
    mdi_error("Error in python_plugin_init: mdi_debug failed");
    return ret;
  }

  code* this_code;
  ret = get_current_code(&this_code);
  if ( ret != 0 ) {
    mdi_error("Error in python_plugin_init: get_current_code failed");
    return ret;
  }

  // If the driver is a Python code, just call the callback and return
  if ( this_code->language == MDI_LANGUAGE_PYTHON ) {
    char engine_name_copy[MDI_NAME_LENGTH_];
    char engine_path_copy[PLUGIN_PATH_LENGTH];
    snprintf(engine_name_copy, strlen(engine_name)+1, "%s", engine_name);
    snprintf(engine_path_copy, strlen(engine_path)+1, "%s", engine_path);
    this_code->py_launch_plugin_callback(engine_name_copy, engine_path_copy, shared_state, mode);
    return 0;
  }

  plugin_shared_state* my_state = (plugin_shared_state*) shared_state;

  // Initialize the Python interpreter
  // Because Python has problems with reinitialization, only initialize Python once
  if ( ! my_state->python_interpreter_initialized ) {

    ret = mdi_debug("[MDI:python_plugin_init] Initializing the Python interpreter\n");
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init: mdi_debug failed");
      return ret;
    }

    Py_Initialize();
    if ( ! Py_IsInitialized() ) {
      mdi_error("Unable to initialize Python interpreter");
      return 1;
    }

    ret = mdi_debug("[MDI:python_plugin_init] Creating an initial Python dictionary\n");
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init: mdi_debug failed");
      return ret;
    }

    PyObject* main_module = PyImport_AddModule("__main__");
    PyObject* original_dict = PyModule_GetDict(main_module);
    my_state->python_interpreter_dict = PyDict_Copy(original_dict);
    my_state->python_interpreter_initialized = 1;
  }

  ret = mdi_debug("[MDI:python_plugin_init] Initializing a Python dictionary for this plugin\n");
  if ( ret != 0 ) {
    mdi_error("Error in python_plugin_init: mdi_debug failed");
    return ret;
  }

  PyObject* main_dict = PyDict_Copy(my_state->python_interpreter_dict);

  ret = mdi_debug("[MDI:python_plugin_init] Finished initializing the Python interpreter\n");
  if ( ret != 0 ) {
    mdi_error("Error in python_plugin_init: mdi_debug failed");
    return ret;
  }

  // Open the Python script for the Engine
  FILE* engine_script = fopen(engine_path, "r");

  if( engine_script ) {

    ret = mdi_debug("[MDI:python_plugin_init] Setting up the Python environment\n");
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init: mdi_debug failed");
      return ret;
    }

    // Get the pointer to the plugin state
    uintptr_t plugin_state_int = (uintptr_t)shared_state;

    // Set the name to anything other than __main__
    // Also, ensure that sys.argv exists
    PyObject* sysrun = PyRun_String("import sys\n"
                    "sys.path.insert(0, '')\n"
                    "if not hasattr(sys, 'argv'):\n"
                    "    sys.argv = ['']\n"
                    "__name__ = '__mdi__'\n"
                    "import ctypes\n"
                    "plugin_state = ctypes.c_void_p(1)\n",
                    Py_file_input,
                    main_dict, main_dict);
    if ( PyErr_Occurred() ) {
      mdi_error("Unable to set system properties for Python plugin");
      print_traceback();
      return 1;
    }
    Py_XDECREF(sysrun);

    ret = mdi_debug("[MDI:python_plugin_init] Running the engine script\n");
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init: mdi_debug failed");
      return ret;
    }

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

    ret = mdi_debug("[MDI:python_plugin_init] Calling MDI_Plugin_init\n");
    if ( ret != 0 ) {
      mdi_error("Error in python_plugin_init: mdi_debug failed");
      return ret;
    }

    // Call the MDI_Plugin_init function for this plugin
    char* plugin_init_name = malloc( PLUGIN_PATH_LENGTH * sizeof(char) );
    if ( mode == 0 ) {
      snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "plugin_state = ctypes.c_void_p(%ld)\nMDI_Plugin_init_%s(plugin_state)", plugin_state_int, engine_name);
    }
    else {
      snprintf(plugin_init_name, PLUGIN_PATH_LENGTH, "plugin_state = ctypes.c_void_p(%ld)\nMDI_Plugin_open_%s(plugin_state)", plugin_state_int, engine_name);
    }
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

  ret = mdi_debug("[MDI:python_plugin_init] Finished\n");
  if ( ret != 0 ) {
    mdi_error("Error in python_plugin_init: mdi_debug failed");
    return ret;
  }

  return 0;
}
