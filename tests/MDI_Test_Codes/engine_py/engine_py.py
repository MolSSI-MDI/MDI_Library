import sys
import time

try: # Check for local build
    import MDI_Library as mdi
except: # Check for installed package
    import mdi

try:
    import numpy
    use_numpy = True
except ImportError:
    use_numpy = False

try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

exit_flag = False

def execute_command(command, comm, class_obj):
    global exit_flag

    if command == "EXIT":
        exit_flag = True
    elif command == "<NATOMS":
        print("SUCCESS")
    else:
        raise Exception("Error in engine_py.py: MDI command not recognized")

    return 0

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

# Register the supported commands
mdi.MDI_Register_Node("@GLOBAL")
mdi.MDI_Register_Command("@GLOBAL","EXIT")
mdi.MDI_Register_Command("@GLOBAL","<NATOMS")

# Set the generic execute_command function
mdi.MDI_Set_Execute_Command_Func(execute_command, None)

# Connect to the driver
comm = mdi.MDI_Accept_Communicator()

while not exit_flag:
    if world_rank == 0:
        command = mdi.MDI_Recv_Command(comm)
    else:
        command = None
    if use_mpi4py:
        command = mpi_world.bcast(command, root=0)

    execute_command( command, comm, None )
