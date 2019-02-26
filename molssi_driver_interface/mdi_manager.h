/*! \file
 *
 * \brief Class declaration for top-level manager of MDI operations
 */

#ifndef MDI_CLASS
#define MDI_CLASS

#include <vector>
#include "mdi.h"
#include "method.h"

class MDIManager
{
public:
  MDIManager(const char* options, void* world_comm);
  MethodTCP* method_tcp;
  MethodMPI* method_mpi;

//private:
//  vector <Method*> methods;
};

#endif
