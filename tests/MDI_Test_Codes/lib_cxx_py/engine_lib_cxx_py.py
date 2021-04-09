import sys
import ctypes

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



def execute_command_general(command, comm, class_obj):
    class_obj.execute_command(command, comm)
    return 0

class MDIEngine:
    def __init__(self, mdi_options, mpi_comm):
        # Initialize the MDI Library
        mdi.MDI_Init(mdi_options)
        self.mpi_world = mpi_comm
        self.world_rank = 0
        self.world_size = 1
        if use_mpi4py:
            mdi.MDI_MPI_set_world_comm(self.mpi_world)
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
            raise Exception("Error in engine_lib_cxx_py.py: MDI command not recognized")

        return 0



# Get command-line inputs
mdi_options = str(sys.argv[1])
mpi_comm_type = str(sys.argv[2])
mpi_comm_input = str(sys.argv[3])

if mpi_comm_type == "pointer":

    # Convert the command-line address to a c_void_p
    ptr_type = ctypes.c_void_p
    mpi_hex_address = int(mpi_comm_input, 16)
    mpi_ptr = ctypes.cast(mpi_hex_address, ptr_type)

    # From the pointer to MPI communicator, get the mpi4py communicator
    handle_t = ctypes.c_void_p
    newobj = type(MPI.COMM_WORLD)()
    handle_old = mpi_ptr
    handle_new = handle_t.from_address(MPI._addressof(newobj))
    handle_new.value = handle_old.value
    world_comm = newobj

elif mpi_comm_type == "int":

    handle_old_value = int(mpi_comm_input)

    handle_t = ctypes.c_void_p
    newobj = type(MPI.COMM_WORLD)()
    handle_new = handle_t.from_address(MPI._addressof(newobj))
    handle_new.value = handle_old_value
    world_comm = newobj

else:
    raise Exception("Error in engine_lib_cxx_py.py: MPI_Comm type not recognized")

# Launch the engine
MDIEngine(mdi_options, world_comm)
