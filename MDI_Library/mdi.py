""" Python wrapper for MDI. """

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
MDI_LIB = ctypes.c_int.in_dll(mdi, "MDI_LIB").value
MDI_TEST = ctypes.c_int.in_dll(mdi, "MDI_TEST").value
MDI_DRIVER = ctypes.c_int.in_dll(mdi, "MDI_DRIVER").value
MDI_ENGINE = ctypes.c_int.in_dll(mdi, "MDI_ENGINE").value
MDI_MAJOR_VERSION = ctypes.c_int.in_dll(mdi, "MDI_MAJOR_VERSION").value
MDI_MINOR_VERSION = ctypes.c_int.in_dll(mdi, "MDI_MINOR_VERSION").value
MDI_PATCH_VERSION = ctypes.c_int.in_dll(mdi, "MDI_PATCH_VERSION").value

world_comm = None

intra_code_comm = None

# dictionary of mpi4py communicators
mpi4py_comms = {}

# dictionary of function callbacks
execute_command_dict = {}

# set_world_size
mdi.MDI_Set_World_Size.argtypes = [ctypes.c_int]
mdi.MDI_Set_World_Size.restype = None

# set_world_rank
mdi.MDI_Set_World_Rank.argtypes = [ctypes.c_int]
mdi.MDI_Set_World_Rank.restype = None

# MDI_Get_Current_Code
mdi.MDI_Get_Current_Code.argtypes = []
mdi.MDI_Get_Current_Code.restype = ctypes.c_int
def MDI_Get_Current_Code():
    return mdi.MDI_Get_Current_Code()

# delete all Python state associated with the current code
def delete_code_state(mdi_comm):
    current_code = MDI_Get_Current_Code()
    if current_code in execute_command_dict.keys():
        del execute_command_dict[current_code]

    # if there is an mpi4py communicator associated with this mdi_comm, delete it
    if mdi_comm in mpi4py_comms:
        del mpi4py_comms[mdi_comm]

def get_mpi_comm_from_flag(comm_flag):
    global world_comm
    global intra_code_comm

    # get the correct communicator, based on the comm_flag
    comm = None
    if comm_flag == 0: # use world_comm
        comm = world_comm
    elif comm_flag == 1: # use the code intra_comm
        comm = intra_code_comm
    else:
        raise Exception("MDI Error: Unknown comm flag in mpi4py callback")
    return comm

def c_ptr_to_py_str(in_ptr, length): 
    result = ctypes.cast(in_ptr, ctypes.POINTER(ctypes.c_char*length)).contents
    presult = ctypes.cast(result, ctypes.c_char_p).value
    presult = presult.decode('utf-8')
    return presult

##################################################
# MPI4Py Recv Callback                           #
##################################################

# define the type of the callback function
mpi4py_recv_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.POINTER(ctypes.c_byte), # buf (ctypes.c_void_p?)
                                         ctypes.c_int, # count
                                         ctypes.c_int, # datatype
                                         ctypes.c_int, # source
                                         ctypes.c_int) # mdi_comm

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Recv_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Recv_Callback.argtypes = [mpi4py_recv_func_type]

# define the python callback function
def mpi4py_recv_callback(buf, count, datatype, source, mdi_comm):

    # determine the data type
    if datatype == MDI_INT_NUMPY or datatype == MDI_INT:
        mpi_type = MPI.INT
        datasize = ctypes.sizeof( ctypes.c_int )
    elif datatype == MDI_DOUBLE_NUMPY or datatype == MDI_DOUBLE:
        mpi_type = MPI.DOUBLE
        datasize = ctypes.sizeof( ctypes.c_double )
    elif datatype == MDI_CHAR:
        mpi_type = MPI.CHAR
        datasize = ctypes.sizeof( ctypes.c_char )
    else:
        raise Exception("MDI Error: MDI type not recognized")

    # get a numpy representation of the data
    nparray = np.ctypeslib.as_array(buf, shape=[ count * datasize ])

    comm = mpi4py_comms[mdi_comm]
    comm.Recv([nparray, mpi_type], source=source)
    return 0

# define the python function that will set the callback function in c
mpi4py_recv_callback_c = mpi4py_recv_func_type( mpi4py_recv_callback )
def set_mpi4py_recv_callback():
    global mpi4py_recv_callback_c
    mdi.MDI_Set_Mpi4py_Recv_Callback( mpi4py_recv_callback_c )

##################################################
# MPI4Py Send Callback                           #
##################################################

# define the type of the callback function
mpi4py_send_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.POINTER(ctypes.c_byte), # buf (ctypes.c_void_p?)
                                         ctypes.c_int, # count
                                         ctypes.c_int, # datatype
                                         ctypes.c_int, # destination
                                         ctypes.c_int) # mdi_comm

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Send_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Send_Callback.argtypes = [mpi4py_send_func_type]

# define the python callback function
def mpi4py_send_callback(buf, count, datatype, destination, mdi_comm):
    global mpi4py_comms

    # determine the data type
    if datatype == MDI_INT_NUMPY or datatype == MDI_INT:
        mpi_type = MPI.INT
        datasize = ctypes.sizeof( ctypes.c_int )
    elif datatype == MDI_DOUBLE_NUMPY or datatype == MDI_DOUBLE:
        mpi_type = MPI.DOUBLE
        datasize = ctypes.sizeof( ctypes.c_double )
    elif datatype == MDI_CHAR:
        mpi_type = MPI.CHAR
        datasize = ctypes.sizeof( ctypes.c_char )
    else:
        raise Exception("MDI Error: MDI type not recognized")

    # get a numpy representation of the data
    nparray = np.ctypeslib.as_array(buf, shape=[ count * datasize ])

    comm = mpi4py_comms[mdi_comm]
    comm.Send([nparray, mpi_type], dest=destination)
    return 0

# define the python function that will set the callback function in c
mpi4py_send_callback_c = mpi4py_send_func_type( mpi4py_send_callback )
def set_mpi4py_send_callback():
    global mpi4py_send_callback_c
    mdi.MDI_Set_Mpi4py_Send_Callback( mpi4py_send_callback_c )

##################################################
# MPI4Py Size Callback                           #
##################################################

# define the type of the callback function
mpi4py_size_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.c_int) # comm_flag

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Size_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Size_Callback.argtypes = [mpi4py_size_func_type]

# define the python callback function
def mpi4py_size_callback(comm_flag):
    try:
        comm = get_mpi_comm_from_flag( comm_flag )
        return comm.Get_size()
    except Exception:
        return -1

# define the python function that will set the callback function in c
mpi4py_size_callback_c = mpi4py_size_func_type( mpi4py_size_callback )
def set_mpi4py_size_callback():
    global mpi4py_size_callback_c
    mdi.MDI_Set_Mpi4py_Size_Callback( mpi4py_size_callback_c )

##################################################
# MPI4Py Rank Callback                           #
##################################################

# define the type of the callback function
mpi4py_rank_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.c_int) # comm_flag

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Rank_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Rank_Callback.argtypes = [mpi4py_rank_func_type]

# define the python callback function
def mpi4py_rank_callback(comm_flag):
    try:
        comm = get_mpi_comm_from_flag( comm_flag )
        return comm.Get_rank()
    except Exception:
        return -1

# define the python function that will set the callback function in c
mpi4py_rank_callback_c = mpi4py_rank_func_type( mpi4py_rank_callback )
def set_mpi4py_rank_callback():
    global mpi4py_rank_callback_c
    mdi.MDI_Set_Mpi4py_Rank_Callback( mpi4py_rank_callback_c )

##################################################
# MPI4Py Gather Names Callback                   #
##################################################

# define the type of the callback function
mpi4py_gather_names_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.POINTER(ctypes.c_char), # buf
                                         ctypes.POINTER(ctypes.c_char)) # names

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Gather_Names_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Gather_Names_Callback.argtypes = [mpi4py_gather_names_func_type]

# define the python callback function
def mpi4py_gather_names_callback(buf, names):
    global world_comm
    world_size = world_comm.Get_size()

    # Create numpy arrays from the C pointers
    buf_np = np.ctypeslib.as_array(buf, shape=[MDI_NAME_LENGTH])
    names_np = np.ctypeslib.as_array(names, shape=[MDI_NAME_LENGTH * world_size])

    # Gather the names
    world_comm.Allgather([buf_np, MPI.CHAR], [names_np, MPI.CHAR])

    return 0

# define the python function that will set the callback function in c
mpi4py_gather_names_callback_c = mpi4py_gather_names_func_type( mpi4py_gather_names_callback )
def set_mpi4py_gather_names_callback():
    global mpi4py_gather_names_callback_c
    mdi.MDI_Set_Mpi4py_Gather_Names_Callback( mpi4py_gather_names_callback_c )

##################################################
# MPI4Py Barrier Callback                        #
##################################################

# define the type of the callback function
mpi4py_barrier_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.c_int) # comm_flag

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Barrier_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Barrier_Callback.argtypes = [mpi4py_barrier_func_type]

# define the python callback function
def mpi4py_barrier_callback(comm_flag):
    global world_comm
    global intra_code_comm

    # get the correct communicator, based on the comm_flag
    if comm_flag == 0: # use world_comm
        comm = world_comm
    elif comm_flag == 1: # use the code intra_comm
        comm = intra_code_comm
    else:
        raise Exception("MDI Error: Unknown comm flag in mpi4py_barrier_callback")

    if comm:
        comm.Barrier()
    else:
        raise Exception("MDI Error: Unable to find mpi communicator in mpi4py_barrier_callback")

    return 0

# define the python function that will set the callback function in c
mpi4py_barrier_callback_c = mpi4py_barrier_func_type( mpi4py_barrier_callback )
def set_mpi4py_barrier_callback():
    global mpi4py_barrier_callback_c
    mdi.MDI_Set_Mpi4py_Barrier_Callback( mpi4py_barrier_callback_c )

##################################################
# MPI4Py Split Callback                          #
##################################################

# define the type of the callback function
mpi4py_split_func_type = ctypes.CFUNCTYPE(ctypes.c_int, # return
                                         ctypes.c_int, # color
                                         ctypes.c_int, # key
                                         ctypes.c_int, # mdi_comm
                                         ctypes.c_int) # comm_flag

# define the c function that allows the callback function to be set
mdi.MDI_Set_Mpi4py_Split_Callback.restype = ctypes.c_int
mdi.MDI_Set_Mpi4py_Split_Callback.argtypes = [mpi4py_split_func_type]

# define the python callback function
def mpi4py_split_callback(color, key, mdi_comm, comm_flag):
    global world_comm
    global intra_code_comm

    # get the correct communicator, based on the comm_flag
    if comm_flag == 0: # create an inter-code communicator
        mpi4py_comms[mdi_comm] = world_comm.Split(color, key)
    elif comm_flag == 1: # create an intra-code communicator
        intra_code_comm = world_comm.Split(color, key)
    else:
        raise Exception("MDI Error: Unknown comm flag in mpi4py_split_callback")

    return 0

# define the python function that will set the callback function in c
mpi4py_split_callback_c = mpi4py_split_func_type( mpi4py_split_callback )
def set_mpi4py_split_callback():
    global mpi4py_split_callback_c
    mdi.MDI_Set_Mpi4py_Split_Callback( mpi4py_split_callback_c )



# MDI_Init
mdi.MDI_Init.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_void_p]
mdi.MDI_Init.restype = ctypes.c_int
def MDI_Init(arg1, comm):
    global world_comm
    global intra_code_comm

    # prepend the _language option, so that MDI knows this is a Python code
    arg1 = "_language Python " + arg1

    command = arg1.encode('utf-8')
    if comm is None:
        mpi_communicator_ptr = None
    else:
        if use_mpi4py:
            world_comm = comm
            intra_code_comm = comm
            mpi_communicator = MPI._addressof(comm)
            mpi_communicator_ptr = ctypes.c_void_p(mpi_communicator)

            # send basic information about the MPI communicator to the MDI libarary
            mpi_rank = comm.Get_rank()
            mpi_world_size = comm.Get_size()
            mdi.MDI_Set_World_Rank(mpi_rank)
            mdi.MDI_Set_World_Size(mpi_world_size)
        else:
            raise Exception("MDI Error: An MPI communicator was passed to MPI_Init, but MPI4Py is not found")

    # determine if the communication method is MPI
    args = arg1.split()
    mdi_method = None
    for i in range(len(args)):
        if args[i] == "-method" and i < len(args) - 1:
            mdi_method = args[i+1]
    if not mdi_method:
        raise Exception("MDI Error: Unable to find -method option")

    # set the MPI4Py callback functions
    set_mpi4py_recv_callback()
    set_mpi4py_send_callback()
    set_mpi4py_size_callback()
    set_mpi4py_rank_callback()
    set_mpi4py_gather_names_callback()
    set_mpi4py_barrier_callback()
    set_mpi4py_split_callback()

    # if using MPI, ensure that numpy is available
    if mdi_method == "MPI":
        if not use_numpy:
            raise Exception("MDI Error: When using the MPI communication method, numpy must be available")

    # call MDI_Init
    ret = mdi.MDI_Init(ctypes.c_char_p(command), mpi_communicator_ptr )
    if ret != 0:
        raise Exception("MDI Error: MDI_Init failed")

    return ret

def MDI_Get_Intra_Code_MPI_Comm():
    global intra_code_comm
    return intra_code_comm

# MDI_Accept_Communicator
mdi.MDI_Accept_Communicator.argtypes = [ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Accept_Communicator.restype = ctypes.c_int
def MDI_Accept_Communicator():
    comm = ctypes.c_int()
    ret = mdi.MDI_Accept_Communicator(ctypes.byref(comm))
    if ret != 0:
        raise Exception("MDI Error: MDI_Accept_Communicator failed")
    return comm.value

# MDI_Send
mdi.MDI_Send.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
mdi.MDI_Send.restype = ctypes.c_int
def MDI_Send(arg1, arg2, arg3, arg4):
    if (arg3 == MDI_INT):
        arg_type = ctypes.c_int
        mdi_type = MDI_INT
    elif (arg3 == MDI_DOUBLE):
        arg_type = ctypes.c_double
        mdi_type = MDI_DOUBLE
    elif (arg3 == MDI_CHAR):
        arg_type = ctypes.c_char
        mdi_type = MDI_CHAR
    elif (arg3 == MDI_INT_NUMPY):
        if not use_numpy:
            raise Exception("MDI Error: Attempting to use a Numpy array, but the Numpy package was not found")
        arg_type = ctypes.c_int
        data = arg1.astype(np.int32)
        data = data.ctypes.data_as(ctypes.c_char_p)
        mdi_type = MDI_INT
    elif (arg3 == MDI_DOUBLE_NUMPY):
        if not use_numpy:
            raise Exception("MDI Error: Attempting to use a Numpy array, but the Numpy package was not found")
        arg_type = ctypes.c_double
        data = arg1.astype(np.float64)
        data = data.ctypes.data_as(ctypes.c_char_p)
        mdi_type = MDI_DOUBLE
    else:
        raise Exception("MDI Error: Unrecognized datatype in MDI_Send")

    if arg3 == MDI_CHAR:
        data_temp = arg1.encode('utf-8')
        data = ctypes.c_char_p(data_temp)

    elif arg3 == MDI_INT or arg3 == MDI_DOUBLE:
        if not isinstance(arg1, list):
            if arg2 == 1:
                if arg3 == MDI_DOUBLE:
                    data_temp = ctypes.pointer((ctypes.c_double)(arg1))
                elif arg3 == MDI_INT:
                    data_temp = ctypes.pointer((ctypes.c_int)(arg1))
                data = ctypes.cast(data_temp, ctypes.POINTER(ctypes.c_char))
            else:
                raise Exception("MDI Error: MDI_Send requires a list if length != 1 and datatype = MDI_INT or MDI_DOUBLE")
        else:
            data_temp = (arg_type*arg2)(*arg1)
            data = ctypes.cast(data_temp, ctypes.POINTER(ctypes.c_char))

    ret = mdi.MDI_Send(data, arg2, ctypes.c_int(mdi_type), arg4)
    if ret != 0:
        raise Exception("MDI Error: MDI_Send failed")

# MDI_Recv
mdi.MDI_Recv.restype = ctypes.c_int
def MDI_Recv(arg2, arg3, arg4):
    if (arg3 == MDI_INT):
        mdi.MDI_Recv.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
        arg_type = ctypes.c_int
        mdi_type = MDI_INT
    elif (arg3 == MDI_DOUBLE):
        mdi.MDI_Recv.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
        arg_type = ctypes.c_double
        mdi_type = MDI_DOUBLE
    elif (arg3 == MDI_CHAR):
        mdi.MDI_Recv.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
        arg_type = ctypes.c_char
        mdi_type = MDI_CHAR
    elif (arg3 == MDI_INT_NUMPY):
        if not use_numpy:
            raise Exception("MDI Error: Attempting to use a Numpy array, but the Numpy package was not found")
        mdi.MDI_Recv.argtypes = [np.ctypeslib.ndpointer(dtype=np.int32, ndim=1, flags='C_CONTIGUOUS'), 
                                 ctypes.c_int, ctypes.c_int, ctypes.c_int]
        arg_type = ctypes.c_int
        mdi_type = MDI_INT
    elif (arg3 == MDI_DOUBLE_NUMPY):
        if not use_numpy:
            raise Exception("MDI Error: Attempting to use a Numpy array, but the Numpy package was not found")
        mdi.MDI_Recv.argtypes = [np.ctypeslib.ndpointer(dtype=np.float64, ndim=1, flags='C_CONTIGUOUS'), 
                                 ctypes.c_int, ctypes.c_int, ctypes.c_int]
        arg_type = ctypes.c_double
        mdi_type = MDI_DOUBLE

    if (arg3 == MDI_DOUBLE_NUMPY):
        arg1 = np.zeros(arg2, dtype='float64')
    elif (arg3 == MDI_INT_NUMPY):
        arg1 = np.zeros(arg2, dtype='int32')
    elif (arg3 == MDI_INT or arg3 == MDI_DOUBLE or arg3 == MDI_CHAR):
        arg_size = ctypes.sizeof(arg_type)
        arg1 = (ctypes.c_char*(arg2*arg_size))()
    else:
        raise Exception("MDI Error: Unrecognized datatype in MDI_Recv")
    ret = mdi.MDI_Recv(arg1, arg2, ctypes.c_int(mdi_type), arg4)
    if ret != 0:
        raise Exception("MDI Error: MDI_Recv failed")

    if (arg3 == MDI_INT_NUMPY):
        return arg1
    elif (arg3 == MDI_DOUBLE_NUMPY):
        return arg1

    result = ctypes.cast(arg1, ctypes.POINTER(arg_type*arg2)).contents

    if (arg3 == MDI_CHAR):
        # if this is an MDI_CHAR, convert it to a python string
        presult = ctypes.cast(result, ctypes.c_char_p).value
        presult = presult.decode('utf-8')
    else:
        if arg2 == 1:
            presult = result[0]
        else:
            presult = [ result[i] for i in range(arg2) ]

    return presult

# MDI_Send_Command
mdi.MDI_Send_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int]
mdi.MDI_Send_Command.restype = ctypes.c_int
def MDI_Send_Command(arg1, arg2):
    command = arg1.encode('utf-8')
    ret = mdi.MDI_Send_Command(ctypes.c_char_p(command), arg2)
    if ret != 0:
        raise Exception("MDI Error: MDI_Send_Command failed")

# MDI_Recv_Command
mdi.MDI_Recv_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int]
mdi.MDI_Recv_Command.restype = ctypes.c_int
def MDI_Recv_Command(arg2): 
    arg_size = ctypes.sizeof(ctypes.c_char)
    arg1 = (ctypes.c_char*(MDI_COMMAND_LENGTH*arg_size))()

    ret = mdi.MDI_Recv_Command(arg1, arg2)
    if ret != 0:
        raise Exception("MDI Error: MDI_Recv_Command failed")

    result = ctypes.cast(arg1, ctypes.POINTER(ctypes.c_char*MDI_COMMAND_LENGTH)).contents
    presult = ctypes.cast(result, ctypes.c_char_p).value
    presult = presult.decode('utf-8')

    # delete all state associated with this code
    if presult == "EXIT":
        delete_code_state(arg2)

    return presult

# MDI_Conversion_Factor
mdi.MDI_Conversion_Factor.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_double)]
mdi.MDI_Conversion_Factor.restype = ctypes.c_int
def MDI_Conversion_Factor(arg1, arg2):
    in_unit = arg1.encode('utf-8')
    out_unit = arg2.encode('utf-8')
    conversion = ctypes.c_double()
    ret = mdi.MDI_Conversion_Factor(ctypes.c_char_p(in_unit), ctypes.c_char_p(out_unit), ctypes.byref(conversion))
    if ret != 0:
        raise Exception("MDI Error: MDI_Conversion_Factor failed")
    return conversion.value

# MDI_Get_Role
mdi.MDI_Get_Role.argtypes = [ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Get_Role.restype = ctypes.c_int
def MDI_Get_Role():
    role = ctypes.c_int()
    ret = mdi.MDI_Get_Role(ctypes.byref(role))
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_Role failed")
    return role.value


#####################################
# Callback functions                #
#####################################
def MDI_Execute_Command_py(command, comm, class_obj):
    global execute_command_dict

    command_cast = ctypes.cast(command, ctypes.POINTER(ctypes.c_char*MDI_COMMAND_LENGTH)).contents
    command_py = ctypes.cast(command_cast, ctypes.c_char_p).value
    command_py = command_py.decode('utf-8')

    # get the current code
    current_code = MDI_Get_Current_Code()

    class_obj_real = execute_command_dict[current_code][1]
    ret = execute_command_dict[current_code][0](command_py, comm, class_obj_real)

    if command_py == "EXIT":
        delete_code_state(comm)
    return ret

# MDI_Set_Execute_Command_Func
# NOTE: Do we need to use WINFUNCTYPE on Windows?
execute_command_func_type = ctypes.CFUNCTYPE(ctypes.c_int, ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_void_p)
MDI_Execute_Command_c = execute_command_func_type( MDI_Execute_Command_py )
mdi.MDI_Set_Execute_Command_Func.argtypes = [execute_command_func_type, ctypes.c_void_p]
mdi.MDI_Set_Execute_Command_Func.restype = ctypes.c_int
def MDI_Set_Execute_Command_Func(func, class_obj):
    global execute_command_dict

    current_code = MDI_Get_Current_Code()

    # store the generic execute command function for future use
    execute_command_dict[current_code] = ( func, class_obj )

    # this is just a dummy pointer; the actual object is stored in execute_command_dict
    class_obj_pointer = ctypes.c_void_p()

    ret = mdi.MDI_Set_Execute_Command_Func( MDI_Execute_Command_c, class_obj_pointer )
    if ret != 0:
        raise Exception("MDI Error: MDI_Set_Execute_Command_Func failed")



##################################################
# Node / Command / Callback management functions #
##################################################
# MDI_Register_Node
mdi.MDI_Register_Node.argtypes = [ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Register_Node.restype = ctypes.c_int
def MDI_Register_Node(arg1):
    node = arg1.encode('utf-8')
    ret = mdi.MDI_Register_Node(ctypes.c_char_p(node))
    if ret != 0:
        raise Exception("MDI Error: MDI_Register_Node failed")

    return ret

# MDI_Check_Node_Exists
mdi.MDI_Check_Node_Exists.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Check_Node_Exists.restype = ctypes.c_int
def MDI_Check_Node_Exists(arg1, arg2):
    node = arg1.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_int)
    flag = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Check_Node_Exists(ctypes.c_char_p(node), arg2, flag)
    if ret != 0:
        raise Exception("MDI Error: MDI_Check_Node_Exists failed")
    flag_cast = ctypes.cast(flag, ctypes.POINTER(ctypes.c_int)).contents

    return flag_cast.value

# MDI_Get_NNodes
mdi.MDI_Get_NNodes.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Get_NNodes.restype = ctypes.c_int
def MDI_Get_NNodes(arg2):
    arg_size = ctypes.sizeof(ctypes.c_int)
    nnodes = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Get_NNodes(arg2, nnodes)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_NNodes failed")
    nnodes_cast = ctypes.cast(nnodes, ctypes.POINTER(ctypes.c_int)).contents

    return nnodes_cast.value

# MDI_Get_Node
mdi.MDI_Get_Node.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Get_Node.restype = ctypes.c_int
def MDI_Get_Node(index, arg2): 
    arg_size = ctypes.sizeof(ctypes.c_char)
    node_name = (ctypes.c_char*(MDI_COMMAND_LENGTH*arg_size))()

    ret = mdi.MDI_Get_Node(index, arg2, node_name)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_Node failed")

    result = ctypes.cast(node_name, ctypes.POINTER(ctypes.c_char*MDI_COMMAND_LENGTH)).contents
    presult = ctypes.cast(result, ctypes.c_char_p).value
    presult = presult.decode('utf-8')
    return presult

# MDI_Register_Command
mdi.MDI_Register_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Register_Command.restype = ctypes.c_int
def MDI_Register_Command(arg1, arg2):
    node = arg1.encode('utf-8')
    command = arg2.encode('utf-8')
    ret = mdi.MDI_Register_Command(ctypes.c_char_p(node), ctypes.c_char_p(command))
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_Callback failed")

    return ret

# MDI_Check_Command_Exists
mdi.MDI_Check_Command_Exists.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Check_Command_Exists.restype = ctypes.c_int
def MDI_Check_Command_Exists(arg1, command_name, arg2):
    node = arg1.encode('utf-8')
    command = command_name.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_int)
    flag = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Check_Command_Exists(ctypes.c_char_p(node), ctypes.c_char_p(command), arg2, flag)
    if ret != 0:
        raise Exception("MDI Error: MDI_Check_Command_Exists failed")
    flag_cast = ctypes.cast(flag, ctypes.POINTER(ctypes.c_int)).contents

    return flag_cast.value

# MDI_Get_NCommands
mdi.MDI_Get_NCommands.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Get_NCommands.restype = ctypes.c_int
def MDI_Get_NCommands(arg1, arg2):
    node = arg1.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_int)
    ncommands = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Get_NCommands(ctypes.c_char_p(node), arg2, ncommands)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_NCommands failed")
    ncommands_cast = ctypes.cast(ncommands, ctypes.POINTER(ctypes.c_int)).contents

    return ncommands_cast.value

# MDI_Get_Command
mdi.MDI_Get_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Get_Command.restype = ctypes.c_int
def MDI_Get_Command(node_name, index, arg2): 
    node = node_name.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_char)
    command_name = (ctypes.c_char*(MDI_COMMAND_LENGTH*arg_size))()

    ret = mdi.MDI_Get_Command(ctypes.c_char_p(node), index, arg2, command_name)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_Command failed")

    return c_ptr_to_py_str(command_name, MDI_COMMAND_LENGTH)

# MDI_Register_Callback
mdi.MDI_Register_Callback.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Register_Callback.restype = ctypes.c_int
def MDI_Register_Callback(arg1, arg2):
    node = arg1.encode('utf-8')
    callback = arg2.encode('utf-8')
    ret =  mdi.MDI_Register_Callback(ctypes.c_char_p(node), ctypes.c_char_p(callback))
    if ret != 0:
        raise Exception("MDI Error: MDI_Register_Callback failed")

    return ret

# MDI_Check_Callback_Exists
mdi.MDI_Check_Callback_Exists.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Check_Callback_Exists.restype = ctypes.c_int
def MDI_Check_Callback_Exists(arg1, callback_name, arg2):
    node = arg1.encode('utf-8')
    callback = callback_name.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_int)
    flag = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Check_Callback_Exists(ctypes.c_char_p(node), ctypes.c_char_p(callback), arg2, flag)
    if ret != 0:
        raise Exception("MDI Error: MDI_Check_Callback_Exists failed")
    flag_cast = ctypes.cast(flag, ctypes.POINTER(ctypes.c_int)).contents

    return flag_cast.value

# MDI_Get_NCallbacks
mdi.MDI_Get_NCallbacks.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
mdi.MDI_Get_NCallbacks.restype = ctypes.c_int
def MDI_Get_NCallbacks(arg1, arg2):
    node = arg1.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_int)
    ncallbacks = (ctypes.c_int*arg_size)()

    ret = mdi.MDI_Get_NCallbacks(ctypes.c_char_p(node), arg2, ncallbacks)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_NCallbacks failed")
    ncallbacks_cast = ctypes.cast(ncallbacks, ctypes.POINTER(ctypes.c_int)).contents

    return ncallbacks_cast.value

# MDI_Get_Callback
mdi.MDI_Get_Callback.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Get_Callback.restype = ctypes.c_int
def MDI_Get_Callback(node_name, index, arg2): 
    node = node_name.encode('utf-8')

    arg_size = ctypes.sizeof(ctypes.c_char)
    callback_name = (ctypes.c_char*(MDI_COMMAND_LENGTH*arg_size))()

    ret = mdi.MDI_Get_Callback(ctypes.c_char_p(node), index, arg2, callback_name)
    if ret != 0:
        raise Exception("MDI Error: MDI_Get_Callback failed")

    return c_ptr_to_py_str(callback_name, MDI_COMMAND_LENGTH)
