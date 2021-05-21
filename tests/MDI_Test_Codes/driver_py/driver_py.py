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

# get the MPI communicator
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

# Initialize the MDI Library
mdi.MDI_Init(sys.argv[2])
if use_mpi4py:
    mpi_world = mdi.MDI_MPI_get_world_comm()
    world_rank = mpi_world.Get_rank()
else:
    world_rank = 0

# Confirm that this code is being used as a driver
role = mdi.MDI_Get_Role()
if not role == mdi.MDI_DRIVER:
    raise Exception("Must run driver_py.py as a DRIVER")

# Connect to the engine
comm = mdi.MDI_Accept_Communicator()

# Determine the name of the engine
mdi.MDI_Send_Command("<NAME", comm)
name = mdi.MDI_Recv(mdi.MDI_NAME_LENGTH, mdi.MDI_CHAR, comm)

if world_rank == 0:
    print(" Engine name: " + str(name))

if world_rank == 0:
    # Check if the engine has the @DEFAULT node
    if ( not mdi.MDI_Check_Node_Exists("@DEFAULT",comm) ):
        raise Exception("Engine does not have the @DEFAULT node")

    # Check if the engine supports the EXIT command
    if ( not mdi.MDI_Check_Command_Exists("@DEFAULT","EXIT",comm) ):
        raise Exception("Engine does not support the EXIT command")

    # Test the node, command, and callback inquiry functions
    nnodes = mdi.MDI_Get_NNodes(comm)
    print("NNODES: " + str(nnodes))
    second_node = mdi.MDI_Get_Node(1, comm)
    print("NODE: " + str(second_node))
    ncommands = mdi.MDI_Get_NCommands(second_node, comm)
    print("NCOMMANDS: " + str(ncommands))
    third_command = mdi.MDI_Get_Command(second_node, 2, comm)
    print("COMMAND: " + str(third_command))
    ncallbacks = mdi.MDI_Get_NCallbacks(second_node, comm)
    print("NCALLBACKS: " + str(ncallbacks))
    first_callback = mdi.MDI_Get_Callback(second_node, 0, comm)
    print("CALLBACK: " + str(first_callback))

    # Check if the engine supports >FORCES callback
    if ( not mdi.MDI_Check_Callback_Exists("@FORCES",">FORCES",comm) ):
        raise Exception("Engine does not support the >FORCES command")

# Send the "<NATOMS" command to the engine
mdi.MDI_Send_Command("<NATOMS", comm)
natoms = mdi.MDI_Recv(1, mdi.MDI_INT, comm)
if world_rank == 0:
    print("NATOMS: " + str(natoms))

# Send the "<COORDS" command to the engine
mdi.MDI_Send_Command("<COORDS", comm)
if use_numpy:
    coords_temp = np.zeros((natoms,3), dtype='float64')
    mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE, comm, buf = coords_temp)
    coords = [ str( round(coords_temp[icoord // 3][icoord % 3], 10) ) for icoord in range( 3 * natoms ) ]
else:
    coords_temp = mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE, comm)
    coords = [ str( round(coords_temp[icoord], 10) ) for icoord in range( 3 * natoms ) ]
if world_rank == 0:
    print("COORDS: " + '[%s]' % ', '.join(map(str, coords)) )

# Send the "<FORCES" command to the engine
mdi.MDI_Send_Command("<FORCES", comm)
forces = mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE, comm)
for iforce in range( 3 * natoms ):
    forces[iforce] = str( round(forces[iforce], 10) )
if world_rank == 0:
    print("FORCES: " + '[%s]' % ', '.join(map(str, forces)) )

if use_numpy:
    mdi.MDI_Send_Command("<FORCES_B", comm)
    double_size = np.dtype(np.float64).itemsize
    forces_double = np.zeros((natoms,3), dtype='float64')
    type_name = forces_double.dtype.name
    forces_bytes = forces_double.tobytes()
    mdi.MDI_Recv(3 * natoms * double_size, mdi.MDI_BYTE, comm, buf = forces_bytes)
    forces_b = np.frombuffer(forces_bytes, dtype = type_name)
    forces_print = [ str( round(forces_b[iforce], 10) ) for iforce in range( 3 * natoms ) ]
else:
    forces_print = forces
if world_rank == 0:
    print("FORCES_B: " + '[%s]' % ', '.join(map(str, forces_print)) )

# Send the "EXIT" command to the engine
mdi.MDI_Send_Command("EXIT", comm)
