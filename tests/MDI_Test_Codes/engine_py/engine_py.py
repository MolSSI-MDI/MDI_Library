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

def execute_command(command, comm, self):

    if command == "EXIT":
        self.exit_flag = True
    elif command == "<NATOMS":
        mdi.MDI_Send(self.natoms, 1, mdi.MDI_INT, comm)
    elif command == "<COORDS":
        mdi.MDI_Send(self.coords, 3 * self.natoms, mdi.MDI_DOUBLE, comm)
    elif command == "<FORCES":
        if use_numpy:
            datatype = mdi.MDI_DOUBLE_NUMPY
        else:
            datatype = mdi.MDI_DOUBLE
        mdi.MDI_Send(self.forces, 3 * self.natoms, datatype, comm)
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

    def run(self):
        # get the MPI communicator
        if use_mpi4py:
            self.mpi_world = MPI.COMM_WORLD

        # Initialize the MDI Library
        mdi.MDI_Init(sys.argv[2],self.mpi_world)
        if use_mpi4py:
            self.mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
            self.world_rank = self.mpi_world.Get_rank()

        # Confirm that this code is being used as an engine
        role = mdi.MDI_Get_Role()
        if not role == mdi.MDI_ENGINE:
            raise Exception("Must run engine_py.py as an ENGINE")

        # Register the supported commands
        mdi.MDI_Register_Node("@GLOBAL")
        mdi.MDI_Register_Command("@GLOBAL","EXIT")
        mdi.MDI_Register_Command("@GLOBAL","<NATOMS")
        mdi.MDI_Register_Command("@GLOBAL","<COORDS")
        mdi.MDI_Register_Command("@GLOBAL","<FORCES")
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
            if self.world_rank == 0:
                command = mdi.MDI_Recv_Command(comm)
            else:
                command = None
            if use_mpi4py:
                command = self.mpi_world.bcast(command, root=0)

            execute_command( command, comm, self )

if __name__== "__main__":
    engine = MDIEngine()
    engine.run()
