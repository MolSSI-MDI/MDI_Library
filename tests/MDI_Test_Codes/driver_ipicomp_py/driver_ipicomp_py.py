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

ipi_msglen = 12

# Test INIT
mdi.MDI_Send_Command("INIT", comm)

# Test STATUS
mdi.MDI_Send_Command("STATUS", comm)
status = mdi.MDI_Recv(ipi_msglen, mdi.MDI_CHAR, comm)
print("Engine status: " + str(status))

# Test POSDATA
mdi.MDI_Send_Command("POSDATA", comm)
cell = [ 0.1 * icell for icell in range(9) ]
celli = [ 1.1 * icell for icell in range(9) ]
natoms = 3
coords = [ ( (0.5 * icoord) - 5.0 ) for icoord in range(3*natoms) ]
mdi.MDI_Send(cell, 9, mdi.MDI_DOUBLE, comm)
mdi.MDI_Send(celli, 9, mdi.MDI_DOUBLE, comm)
mdi.MDI_Send(natoms, 1, mdi.MDI_INT, comm)
mdi.MDI_Send(coords, 3*natoms, mdi.MDI_DOUBLE, comm)

# Test GETFORCE
mdi.MDI_Send_Command("GETFORCE", comm)
force_status = mdi.MDI_Recv(ipi_msglen, mdi.MDI_CHAR, comm)
pot = mdi.MDI_Recv(1, mdi.MDI_DOUBLE, comm)
nat = mdi.MDI_Recv(1, mdi.MDI_INT, comm)
forces = mdi.MDI_Recv(3*nat, mdi.MDI_DOUBLE, comm)
stress = mdi.MDI_Recv(9, mdi.MDI_DOUBLE, comm)
nextra = mdi.MDI_Recv(1, mdi.MDI_INT, comm)
print("force_status: " + str(force_status))
print("pot: " + str(pot))
print("nat: " + str(nat))
print("forces: " + str(forces))
print("stress: " + str(stress))

# Command the engine to exit
mdi.MDI_Send_Command("EXIT", comm)
