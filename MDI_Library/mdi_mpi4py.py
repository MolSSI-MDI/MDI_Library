""" Class for handling MPI4PY-based communication. """

import sys
import os
dir_path = os.path.dirname(os.path.realpath(__file__))

import ctypes

# attempt to import numpy
try:
    import numpy as np
    use_numpy = True
except ImportError:
    use_numpy = False

# attempt to import mpi4py
try:
    from mpi4py import MPI
    use_mpi4py = True
except ImportError:
    use_mpi4py = False

# get the path to the MDI Library
try: # Unix
    mdi_name_file = open(dir_path + "/mdi_name","r")
    mdi_name = mdi_name_file.read()
    mdi_path = dir_path + "/" + mdi_name
except IOError: # Windows
    mdi_name_file = open(dir_path + "\\mdi_name","r")
    mdi_name = mdi_name_file.read()
    mdi_path = dir_path + "\\" + mdi_name

# load the MDI Library
try:
    mdi = ctypes.CDLL(mdi_path)
    MDI_COMMAND_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_COMMAND_LENGTH").value
except (ValueError, AttributeError):
    mdi = ctypes.WinDLL(mdi_path)
    MDI_COMMAND_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_COMMAND_LENGTH").value

# MDI Variables
MDI_COMMAND_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_COMMAND_LENGTH").value
MDI_NAME_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_NAME_LENGTH").value
MDI_NULL_COMM = ctypes.c_int.in_dll(mdi, "MDI_NULL_COMM").value
MDI_INT = ctypes.c_int.in_dll(mdi, "MDI_INT").value
MDI_DOUBLE = ctypes.c_int.in_dll(mdi, "MDI_DOUBLE").value
MDI_CHAR = ctypes.c_int.in_dll(mdi, "MDI_CHAR").value
MDI_INT_NUMPY = ctypes.c_int.in_dll(mdi, "MDI_INT_NUMPY").value
MDI_DOUBLE_NUMPY = ctypes.c_int.in_dll(mdi, "MDI_DOUBLE_NUMPY").value
MDI_TCP = ctypes.c_int.in_dll(mdi, "MDI_TCP").value
MDI_MPI = ctypes.c_int.in_dll(mdi, "MDI_MPI").value
MDI_VERSION = ctypes.c_double.in_dll(mdi, "MDI_VERSION").value

class MPI4PYCommunicator():
    def __init__(self, method, mpi_comm, mpi_rank):
        self.method = method
        self.mpi_comm = mpi_comm
        self.mpi_rank = mpi_rank
        self.mdi_version = None

    def send(self, buf, length, send_type):
        if send_type == MDI_INT_NUMPY or send_type == MDI_INT:
            mpi_type = MPI.INT
            numpy_type = np.int32

            if send_type == MDI_INT:
                if not isinstance(buf, list):
                    if length == 1:
                        buf = np.array([buf], numpy_type)
                    else:
                        raise Exception("MDI Error: MDI_Send requires a list if length != 1 and datatype = MDI_INT")
                else:
                    buf = np.array(buf, numpy_type)

        elif send_type == MDI_DOUBLE_NUMPY or send_type == MDI_DOUBLE:
            mpi_type = MPI.DOUBLE
            numpy_type = np.float64

            if send_type == MDI_DOUBLE:
                if not isinstance(buf, list):
                    if length == 1:
                        buf = np.array([buf], numpy_type)
                    else:
                        raise Exception("MDI Error: MDI_Send requires a list if length != 1 and datatype = MDI_DOUBLE")
                else:
                    buf = np.array(buf, numpy_type)

        elif send_type == MDI_CHAR:
            mpi_type = MPI.CHAR
            numpy_type = np.uint8

            # construct a NumPy buffer
            strbuf = [ 0 for i in range(length) ]
            for i in range(min(length,len(buf))):
                strbuf[i] = ord(buf[i])
            buf = np.array(strbuf, dtype=numpy_type)

        else:
            raise Exception("MDI Error: MDI type not recognized")

        self.mpi_comm.Send([buf, mpi_type], dest=(self.mpi_rank+1)%2)
        return 0

    def recv(self, length, recv_type):
        if recv_type == MDI_INT_NUMPY or recv_type == MDI_INT:
            mpi_type = MPI.INT
            numpy_type = np.int32
        elif recv_type == MDI_DOUBLE_NUMPY or recv_type == MDI_DOUBLE:
            mpi_type = MPI.DOUBLE
            numpy_type = np.float64
        elif recv_type == MDI_CHAR:
            mpi_type = MPI.CHAR
            numpy_type = np.uint8
        else:
            raise Exception("MDI Error: MDI type not recognized")

        buf = np.empty(length, dtype=numpy_type)
        self.mpi_comm.Recv([buf, mpi_type], source=(self.mpi_rank+1)%2)

        if recv_type == MDI_INT or recv_type == MDI_DOUBLE:
            if length == 1:
                return buf[0]
            else:
                return [ buf[i] for i in range(length) ]

        elif recv_type == MDI_INT_NUMPY or recv_type == MDI_DOUBLE_NUMPY:
            return buf

        elif recv_type == MDI_CHAR:
            # convert from np.uint8 to a Python string
            strbuf = ''
            found_zero = False
            for ichar in range(length):
                if buf[ichar] == 0:
                    found_zero = True
                if not found_zero:
                    strbuf += chr( buf[ichar] )
            return strbuf

        else:
            raise Exception("MDI Error: MDI data type not recognized")


    def exchange_version(self):
        sendbuf = np.array([MDI_VERSION], dtype=np.float64)
        self.send(sendbuf, 1, MDI_DOUBLE)

        self.mdi_version = self.recv(1, MDI_DOUBLE)
        return 0

class MPI4PYManager():

    def __init__(self, mdi_options, mpi_comm):
        # communicator for intra-code communication
        self.intra_code_comm = None

        # list of all MPI4PYCommunicator objects
        self.communicators = []

        # number of MDI communicators this manager has returned
        self.returned_comms = 0

        # order of this rank's associated code
        self.mpi_code_rank = 0

        # name of this code
        self.name = None

        if mpi_comm is None:
            raise Exception("MDI Error: Attempting to use MPI4Py without an MPI communciator")
        elif not use_mpi4py:
            raise Exception("MDI Error: Attempting to use MPI4Py but MPI4PY was not found")
        elif not use_numpy:
            raise Exception("MDI Error: Attempting to use MPI4Py but NumPy was not found")

        # confirm that the communication method is MPI
        options = mdi_options.split()
        method = None
        for i in range(len(options)):
            if options[i] == "-method" and i < len(options) - 1:
                method = options[i+1]
        if method != "MPI":
            raise Exception("MDI Error: Attempting to use MPI4Py but MDI method is not MPI")

        # determine the name of the code associated with this MPI rank
        self.name = None
        for i in range(len(options)):
            if options[i] == "-name" and i < len(options) - 1:
                self.name = options[i+1]
        if not self.name:
            raise Exception("MDI Error: Unable to find -name option")

        # determine the role of this code
        my_role = None
        for i in range(len(options)):
            if options[i] == "-role" and i < len(options) - 1:
                my_role = options[i+1]
        if my_role == "ENGINE":
            pass
        elif my_role == "DRIVER":
            # if the role is driver, set the name to a null string
            self.name = ""
        else:
            raise Exception("MDI Error: Unable to find -name option")

        # determine if running in i-PI compatibility mode
        ipi_compatibility = False
        for i in range(len(options)):
            if options[i] == "-ipi":
                ipi_compatibility = True

        # check if this calculation should redirect the standard output
        output_file = None
        for i in range(len(options)):
            if options[i] == "-out" and i < len(options) - 1:
                output_file = options[i+1]
        if output_file is not None:
            sys.stdout = open(output_file, 'w')

        # obtain basic information about the MPI configuration
        mpi_rank = mpi_comm.Get_rank()
        mpi_world_size = mpi_comm.Get_size()

        # construct a NumPy buffer of the name of this rank's code
        namebuf = [ 0 for i in range(MDI_NAME_LENGTH) ]
        for i in range(len(self.name)):
            namebuf[i] = ord(self.name[i])
        sendbuf = np.array(namebuf, dtype=np.uint8)

        # gather the names of the codes associated with all of the ranks
        names_buf = np.empty([mpi_world_size,MDI_NAME_LENGTH], dtype=np.uint8)
        mpi_comm.Allgather([sendbuf, MPI.CHAR], [names_buf, MPI.CHAR])
        names = [ '' for irank in range(mpi_world_size) ]
        name_conv = ''
        for i in range(mpi_world_size):
            found_zero = False
            for j in range(MDI_NAME_LENGTH):
                if names_buf[i][j] == 0:
                    found_zero = True
                if found_zero:
                    name_conv += chr( 0 )
                    names[i] += chr( 0 )
                else:
                    name_conv += chr( names_buf[i][j] )
                    names[i] += chr( names_buf[i][j] )

        # determine which rank corresponds to rank 0 of the driver
        driver_rank = -1
        for irank in range(mpi_world_size):
            if driver_rank == -1:
                if names_buf[irank][0] == 0:
                    driver_rank = irank
        if driver_rank == -1:
            raise Exception("MDI Error: Could not identify driver in mdi_mpi4py.py")

        nunique_names = 0
        for irank in range(mpi_world_size):

            # determine if this is the first instance of a new code
            new_code = True
            for jrank in range(irank):
                if names[irank] == names[jrank]:
                    new_code = False

            # is this is the first instance of a new production code?
            if new_code and names_buf[irank][0] != 0:
                nunique_names += 1
                if names[mpi_rank] == names[irank]:
                    self.mpi_code_rank = nunique_names

                # create an MPI communicator for communication between the driver and engine
                mpi_color = 0
                mpi_key = 0
                if mpi_rank == driver_rank:
                    mpi_color = 1
                elif mpi_rank == irank:
                    mpi_color = 1
                    mpi_key = 1
                new_mpi_comm = mpi_comm.Split(mpi_color, mpi_key)

                # create an MDI communicator for communication between the driver and engine
                if mpi_rank == driver_rank or mpi_rank == irank:
                    self.communicators.append( MPI4PYCommunicator(MDI_MPI, new_mpi_comm, mpi_key) )

                    # communicate the version number between codes
                    if not ipi_compatibility:
                        self.communicators[-1].exchange_version()

        # split the intra-code communicator
        mpi_color = self.mpi_code_rank
        self.intra_code_comm = mpi_comm.Split(mpi_color, mpi_rank)
        mdi.MDI_Set_MPI_Intra_Rank( self.intra_code_comm.Get_rank() )
        mpi_comm.Barrier()

    def Accept_Communicator(self):
        if self.returned_comms < len(self.communicators):
            self.returned_comms += 1
            return self.returned_comms

    def Send(self, buf, length, send_type, comm):
        communicator = self.communicators[comm-1]
        return communicator.send(buf, length, send_type)

    def Recv(self, length, send_type, comm):
        communicator = self.communicators[comm-1]
        return communicator.recv(length, send_type)

    def Recv_Command(self, comm):
        communicator = self.communicators[comm-1]

        get_new_command = True

        while get_new_command:
            command = communicator.recv(MDI_COMMAND_LENGTH, MDI_CHAR)
            get_new_command = False

            # check if this command corresponds to one of MDI's standard built-in commands
            if command == "<NAME":
                self.Send(self.name, MDI_NAME_LENGTH, MDI_CHAR, comm)
                get_new_command = True
            elif command == "<VERSION":
                self.Send(MDI_VERSION, 1, MDI_DOUBLE, comm)
                get_new_command = True

        return command

    def Send_Command(self, buf, comm):
        communicator = self.communicators[comm-1]
        return_value = communicator.send(buf, MDI_COMMAND_LENGTH, MDI_CHAR)
        return return_value
