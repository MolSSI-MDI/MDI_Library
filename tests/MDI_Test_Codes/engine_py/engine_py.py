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

def execute_command(command, comm, self):

    if command == "EXIT":
        self.exit_flag = True
    elif command == "<NATOMS":
        mdi.MDI_Send(self.natoms, 1, mdi.MDI_INT, comm)
    elif command == "<COORDS":
        mdi.MDI_Send(self.coords, 3 * self.natoms, mdi.MDI_DOUBLE, comm)
    elif command == "<FORCES_B":
        # Create NumPy byte array
        double_size = np.dtype(np.float64).itemsize
        forces_bytes = self.forces.tobytes()

        mdi.MDI_Send(forces_bytes, 3 * self.natoms * double_size, mdi.MDI_BYTE, comm)
    elif command == "<FORCES":
        mdi.MDI_Send(self.forces, 3 * self.natoms, mdi.MDI_DOUBLE, comm)
    else:
        raise Exception("Error in engine_py.py: MDI command not recognized")

    return 0

class MDIEngine:

    def __init__(self):
        self.exit_flag = False

        # MPI variables
        self.mpi_world = None
        self.world_rank = 0

        # set dummy molecular information
        self.natoms = 10
        self.coords = [ 0.1 * i for i in range( 3 * self.natoms ) ]
        forces = [ 0.01 * i for i in range( 3 * self.natoms ) ]
        if use_numpy:
            self.forces = np.array(forces)
        else:
            self.forces = forces

    def run(self, mdi_options):
        # get the MPI communicator
        if use_mpi4py:
            self.mpi_world = MPI.COMM_WORLD

        # Initialize the MDI Library
        mdi.MDI_Init(mdi_options)
        if use_mpi4py:
            self.mpi_world = mdi.MDI_MPI_get_world_comm()
            self.world_rank = self.mpi_world.Get_rank()

        # Confirm that this code is being used as an engine
        role = mdi.MDI_Get_Role()
        if not role == mdi.MDI_ENGINE:
            raise Exception("Must run engine_py.py as an ENGINE")

        # Register the supported commands
        mdi.MDI_Register_Node("@DEFAULT")
        mdi.MDI_Register_Command("@DEFAULT","EXIT")
        mdi.MDI_Register_Command("@DEFAULT","<NATOMS")
        mdi.MDI_Register_Command("@DEFAULT","<COORDS")
        mdi.MDI_Register_Command("@DEFAULT","<FORCES")
        mdi.MDI_Register_Command("@DEFAULT","<FORCES_B")
        mdi.MDI_Register_Node("@FORCES")
        mdi.MDI_Register_Command("@FORCES","EXIT")
        mdi.MDI_Register_Command("@FORCES","<FORCES")
        mdi.MDI_Register_Command("@FORCES",">FORCES")
        mdi.MDI_Register_Callback("@FORCES",">FORCES")

        # Set the generic execute_command function
        mdi.MDI_Set_Execute_Command_Func(execute_command, self)

        # Connect to the driver
        comm = mdi.MDI_Accept_Communicator()

        while not self.exit_flag:
            command = mdi.MDI_Recv_Command(comm)
            if use_mpi4py:
                command = self.mpi_world.bcast(command, root=0)

            execute_command( command, comm, self )

def MDI_Plugin_init_engine_py():
    # Get command-line information from the driver
    argc = mdi.MDI_Plugin_get_argc()
    found_mdi_options = False
    mdi_options = ""
    for iarg in range(argc):
        arg = mdi.MDI_Plugin_get_arg(iarg)
        if arg == "-mdi" or arg == "--mdi":
            if argc > iarg + 1:
                mdi_options = mdi.MDI_Plugin_get_arg(iarg+1)
            else:
                raise Exception("Error in engine_py.py: -mdi argument not provided")
            found_mdi_options = True
    if not found_mdi_options:
        raise Exception("Error in engine_py.py: -mdi option not provided")

    engine = MDIEngine()
    engine.run(mdi_options)

if __name__== "__main__":
    engine = MDIEngine()
    engine.run(sys.argv[2])
