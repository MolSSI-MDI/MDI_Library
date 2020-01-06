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

# get the MPI communicator
if use_mpi4py:
    mpi_world = MPI.COMM_WORLD
else:
    mpi_world = None

######################################################################
# Engine code
######################################################################

def execute_command_general(command, comm, class_obj):
    class_obj.execute_command(command, comm)
    return 0

class MDIEngine:
    def __init__(self, mdi_options, mpi_comm):
        # Initialize the MDI Library
        mdi.MDI_Init(mdi_options,mpi_comm)
        self.mpi_world = mpi_comm
        self.world_rank = 0
        self.world_size = 1
        if use_mpi4py:
            self.mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
            self.world_rank = self.mpi_world.Get_rank()
            self.world_size = self.mpi_world.Get_size()

        # Set the generic execute_command function
        mdi.MDI_Set_Execute_Command_Func(execute_command_general, self)

        self.exit_flag = False

        # Set some dummy molecular properties
        self.natoms = 10 * self.world_size
        self.atoms = [ iatom * 1.1 for iatom in range( self.natoms ) ]

    def execute_command(self, command, comm):
        global exit_flag

        if command == "EXIT":
            exit_flag = True
        elif command == "<NATOMS":
            mdi.MDI_Send(self.natoms, 1, mdi.MDI_INT, comm)
        else:
            raise Exception("Error in engine_py.py: MDI command not recognized")

        return 0


######################################################################
# Driver code
######################################################################
if use_mpi4py:
    mpi_world.Barrier()

# Initialize the MDI Library
mdi.MDI_Init(sys.argv[2],mpi_world)
if use_mpi4py:
    mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
    world_rank = mpi_world.Get_rank()
    world_size = mpi_world.Get_size()
else:
    world_rank = 0
    world_size = 0

if world_rank == 0:
    print("Start of driver")

# Split the communicator into individual tasks
color = 0
key = world_rank
if world_rank < world_size//2:
    color = 1
if use_mpi4py:
    mpi_task_comm = mpi_world.Split(color, key)
    task_rank = mpi_task_comm.Get_rank()
else:
    mpi_task_comm = None
    task_rank = 0

# Check if this connection uses the LIBRARY method
method = mdi.MDI_LIB

niterations = 10
for iiteration in range(niterations):

    # Create and connect to a library instance that spans the MPI task communicator
    MDIEngine("-role ENGINE -name MM -method LIB -driver_name driver", mpi_task_comm)
    comm = mdi.MDI_Accept_Communicator()

    # Create and connect to a library instance that spans MPI_COMM_WORLD
    MDIEngine("-role ENGINE -name unsplit -method LIB -driver_name driver", mpi_world)
    comm_unsplit = mdi.MDI_Accept_Communicator()

    # Communicate with the library instance that spans the MPI task communicator
    mdi.MDI_Send_Command("<NATOMS", comm)
    natoms = mdi.MDI_Recv(1, mdi.MDI_INT, comm)
    if world_rank == 0:
        print("NATOMS: " + str(natoms))
    mdi.MDI_Send_Command("EXIT", comm)

    # Communicate with the library instance that spans MPI_COMM_WORLD
    mdi.MDI_Send_Command("<NATOMS", comm_unsplit)
    natoms = mdi.MDI_Recv(1, mdi.MDI_INT, comm_unsplit)
    if world_rank == 0:
        print("NATOMS: " + str(natoms))
    mdi.MDI_Send_Command("EXIT", comm_unsplit)

if use_mpi4py:
    mpi_world.Barrier()
