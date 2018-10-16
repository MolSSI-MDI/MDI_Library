""" Example of wrapping cos function from math.h using ctypes. """

import os 
dir_path = os.path.dirname(os.path.realpath(__file__))

import ctypes

try:
    import numpy as np
    import numpy.ctypeslib as npct
    use_numpy = True
except ImportError:
    use_numpy = False

# get the name of the MDI library
mdi_name_file = open(dir_path + "/mdi_name","r")
mdi_name = mdi_name_file.read()

# load the MDI library
mdi = ctypes.CDLL(dir_path + "/" + mdi_name)

# MDI Variables
MDI_COMMAND_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_COMMAND_LENGTH")
MDI_NAME_LENGTH = ctypes.c_int.in_dll(mdi, "MDI_NAME_LENGTH")
MDI_INT = ctypes.c_int.in_dll(mdi, "MDI_INT")
MDI_DOUBLE = ctypes.c_int.in_dll(mdi, "MDI_DOUBLE")
MDI_CHAR = ctypes.c_int.in_dll(mdi, "MDI_CHAR")

# Unit conversions
MDI_METER_TO_BOHR = ctypes.c_double.in_dll(mdi, "MDI_METER_TO_BOHR").value
MDI_ANGSTROM_TO_BOHR = ctypes.c_double.in_dll(mdi, "MDI_ANGSTROM_TO_BOHR").value
MDI_SECOND_TO_AUT = ctypes.c_double.in_dll(mdi, "MDI_SECOND_TO_AUT").value
MDI_PICOSECOND_TO_AUT = ctypes.c_double.in_dll(mdi, "MDI_PICOSECOND_TO_AUT").value
MDI_NEWTON_TO_AUF = ctypes.c_double.in_dll(mdi, "MDI_NEWTON_TO_AUF").value
MDI_JOULE_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_JOULE_TO_HARTREE").value
MDI_KJ_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_KJ_TO_HARTREE").value
MDI_KJPERMOL_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_KJPERMOL_TO_HARTREE").value
MDI_KCALPERMOL_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_KCALPERMOL_TO_HARTREE").value
MDI_EV_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_EV_TO_HARTREE").value
MDI_RYDBERG_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_RYDBERG_TO_HARTREE").value
MDI_KELVIN_TO_HARTREE = ctypes.c_double.in_dll(mdi, "MDI_KELVIN_TO_HARTREE").value



# MDI_Init
mdi.MDI_Init.argtypes = [ctypes.c_int]
mdi.MDI_Init.restype = ctypes.c_int
def MDI_Init(arg):
    return mdi.MDI_Init(arg)

# MDI_Open
mdi.MDI_Open.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_char)]
mdi.MDI_Open.restype = ctypes.c_int
def MDI_Open(arg1, arg2, arg3):
    return mdi.MDI_Open(arg1, arg2, arg3)

# MDI_Accept_Connection
mdi.MDI_Accept_Connection.argtypes = [ctypes.c_int]
mdi.MDI_Accept_Connection.restype = ctypes.c_int
def MDI_Accept_Connection(arg):
    return mdi.MDI_Accept_Connection(arg)

# MDI_Send
mdi.MDI_Send.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
mdi.MDI_Send.restype = ctypes.c_int
def MDI_Send(arg1, arg2, arg3, arg4):

    if (arg3 == MDI_INT):
        arg_type = ctypes.c_int
    elif (arg3 == MDI_DOUBLE):
        arg_type = ctypes.c_double
    elif (arg3 == MDI_CHAR):
        arg_type = ctypes.c_char
    arg_size = ctypes.sizeof(arg_type)

    arg1_ = (ctypes.c_char*(arg2*arg_size))()

    arg_value = ctypes.cast(arg1, ctypes.POINTER(arg_type*arg2)).contents
    if arg2 == 1:
        arg_value = arg1
    else:
        for i in range(arg2):
            arg_value[i] = arg1[i]

    return mdi.MDI_Send(arg1_, arg2, arg3, arg4)

# MDI_Recv
mdi.MDI_Recv.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
mdi.MDI_Recv.restype = ctypes.c_int
def MDI_Recv(arg2, arg3, arg4):

    if (arg3 == MDI_INT):
        arg_type = ctypes.c_int
    elif (arg3 == MDI_DOUBLE):
        arg_type = ctypes.c_double
    elif (arg3 == MDI_CHAR):
        arg_type = ctypes.c_char
    arg_size = ctypes.sizeof(arg_type)

    arg1 = (ctypes.c_char*(arg2*arg_size))()
    ret = mdi.MDI_Recv(arg1, arg2, arg3, arg4)

    result = ctypes.cast(arg1, ctypes.POINTER(arg_type*arg2)).contents
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
    return mdi.MDI_Send_Command(ctypes.c_char_p(command), arg2)

# MDI_Recv_Command
mdi.MDI_Recv_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int]
mdi.MDI_Recv_Command.restype = ctypes.c_int
def MDI_Recv_Command(arg2):
    ret = mdi.MDI_Recv_Command(arg1, arg2)
    return arg1
