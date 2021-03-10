#ifndef ENGINE_CXX
#define ENGINE_CXX

// ensure that symbols are exported to Windows .dll files
#ifdef _WIN32
  #define DllExport   __declspec( dllexport )
#else
  #define DllExport
#endif

extern "C" DllExport int MDI_Plugin_init();

#endif
