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

try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

# get the MPI communicator
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

# Initialize the MDI Library
mdi.MDI_Init(sys.argv[2],mpi_world)
if use_mpi4py:
    mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
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

print(" Engine name: " + str(name))

# Check if the engine has the @GLOBAL node
if ( not mdi.MDI_Check_Node_Exists("@GLOBAL",comm) ):
    raise Exception("Engine does not have the @GLOBAL node")

# Check if the engine supports the EXIT command
if ( not mdi.MDI_Check_Command_Exists("@GLOBAL","EXIT",comm) ):
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
print("NATOMS: " + str(natoms))

# Send the "<COORDS" command to the engine
mdi.MDI_Send_Command("<COORDS", comm)
if use_numpy:
    coords_temp = mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE_NUMPY, comm)
else:
    coords_temp = mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE, comm)
coords = [ round(coords_temp[icoord], 10) for icoord in range( 3 * natoms ) ]
print("COORDS: " + str(coords))

# Send the "<FORCES" command to the engine
mdi.MDI_Send_Command("<FORCES", comm)
forces = mdi.MDI_Recv(3 * natoms, mdi.MDI_DOUBLE, comm)
for iforce in range( 3 * natoms ):
    forces[iforce] = round(forces[iforce], 10)
print("FORCES: " + str(forces))

# Send the "EXIT" command to the engine
mdi.MDI_Send_Command("EXIT", comm)
