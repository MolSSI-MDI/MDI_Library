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

def execute_command(command, comm, self):

    if command == "EXIT":
        self.exit_flag = True
    elif command == "<NATOMS":
        mdi.MDI_Send(self.natoms, 1, mdi.MDI_INT, comm)
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

    def run(self):
        # get the MPI communicator
        if use_mpi4py:
            self.mpi_world = MPI.COMM_WORLD

        # Initialize the MDI Library
        mdi.MDI_Init(sys.argv[2],self.mpi_world)
        if use_mpi4py:
            self.mpi_world = mdi.MDI_Get_Intra_Code_MPI_Comm()
            self.world_rank = self.mpi_world.Get_rank()

        # Register the supported commands
        mdi.MDI_Register_Node("@GLOBAL")
        mdi.MDI_Register_Command("@GLOBAL","EXIT")
        mdi.MDI_Register_Command("@GLOBAL","<NATOMS")

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
