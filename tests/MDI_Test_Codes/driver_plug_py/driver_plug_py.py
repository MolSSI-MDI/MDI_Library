import sys
import time

try: # Check for local build
    import MDI_Library as mdi
except: # Check for installed package
    import mdi

try:
    import numpy as np
    use_numpy = True
except ImportError:
    use_numpy = False

# Check for a -nompi argument
# This argument prevents the code from importing MPI
nompi_flag = False
for arg in sys.argv:
    if arg == "-nompi":
        nompi_flag = True

use_mpi4py = False
if not nompi_flag:
    try:
        from mpi4py import MPI
        use_mpi4py = True
    except ImportError:
        pass

def code_for_plugin_instance(mpi_comm, mdi_comm, class_object):
    mpi_rank = 0
    if use_mpi4py:
        mpi_rank = mpi_comm.Get_rank()

    # Determine the name of the engine
    mdi.MDI_Send_Command("<NAME", mdi_comm)
    name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, mdi_comm)

    if mpi_rank == 0:
        print(" Engine name: " + str(name))

    # Determine the number of atoms
    mdi.MDI_Send_Command("<NATOMS", mdi_comm)
    natoms = mdi.MDI_Recv(1, mdi.MDI_INT, mdi_comm)

    if mpi_rank == 0:
        print(" natoms: " + str(natoms))

    # Send the "EXIT" command to the engine
    mdi.MDI_Send_Command("EXIT", mdi_comm)

    return 0


if __name__ == "__main__":

    # Parse command-line arguments
    mdi_options = None
    driver_nranks = -1
    plugin_nranks = -1
    plugin_name = None
    for iarg in range( len(sys.argv) ):
        if sys.argv[iarg] == "-mdi":
            mdi_options = sys.argv[iarg+1]
            iarg += 1
        elif sys.argv[iarg] == "-driver_nranks":
            driver_nranks = int(sys.argv[iarg+1])
            iarg += 1
        elif sys.argv[iarg] == "-plugin_nranks":
            plugin_nranks = int(sys.argv[iarg+1])
            iarg += 1
        elif sys.argv[iarg] == "-plugin_name":
            plugin_name = sys.argv[iarg+1]
            iarg += 1
    if mdi_options is None:
        raise Exception("-mdi command-line option was not provided")
    if driver_nranks < 0:
        raise Exception("-driver_nranks command-line option is not valid")
    if plugin_nranks < 0:
        raise Exception("-plugin_nranks command-line option is not valid")
    if plugin_name is None:
        raise Exception("-plugin_name command-line option was not provided")

    # Initialize the MDI Library
    mdi.MDI_Init( mdi_options )

    mpi_world = None
    world_rank = 0
    if use_mpi4py:
        mpi_world = mdi.MDI_MPI_get_world_comm()
        world_rank = mpi_world.Get_rank()

    # Confirm that this code is being used as a driver
    role = mdi.MDI_Get_Role()
    if not role == mdi.MDI_DRIVER:
        raise Exception("Must run driver_py.py as a DRIVER")

    if True:
        if world_rank == 0:
            print("I am engine instance: 1")

        # Launch an instance of the engine library
        mdi.MDI_Launch_plugin(plugin_name,
                              "-mdi \"-name MM -role ENGINE -method LINK\"",
                              mpi_world,
                              code_for_plugin_instance,
                              None)
