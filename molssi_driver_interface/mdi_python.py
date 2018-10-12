""" Example of wrapping cos function from math.h using ctypes. """

import os 
dir_path = os.path.dirname(os.path.realpath(__file__))

import ctypes

# find and load the library

# OSX or linux
#from ctypes.util import find_library
#libm = ctypes.cdll.LoadLibrary(find_library('m'))

# Windows
# from ctypes import windll
# libm = cdll.msvcrt

# get the name of the MDI library
mdi_name_file = open(dir_path + "/mdi_name","r")
mdi_name = mdi_name_file.read()

# load the MDI library
mdi = ctypes.CDLL(dir_path + "/" + mdi_name)



# set the argument type
#libm.cos.argtypes = [ctypes.c_double]
# set the return type
#libm.cos.restype = ctypes.c_double

#def cos_func(arg):
#    ''' Wrapper for cos from math.h '''
#    return libm.cos(arg)

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
def MDI_Open(arg):
    return mdi.MDI_Open(arg)

# MDI_Accept_Connection
mdi.MDI_Accept_Connection.argtypes = [ctypes.c_int]
mdi.MDI_Accept_Connection.restype = ctypes.c_int
def MDI_Accept_Connection(arg):
    return mdi.MDI_Accept_Connection(arg)

# MDI_Send
mdi.MDI_Send.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
mdi.MDI_Send.restype = ctypes.c_int
def MDI_Send(arg):
    return mdi.MDI_Send(arg)

# MDI_Recv
mdi.MDI_Recv.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int, ctypes.c_int, ctypes.c_int]
mdi.MDI_Recv.restype = ctypes.c_int
def MDI_Recv(arg):
    return mdi.MDI_Recv(arg)

# MDI_Send_Command
mdi.MDI_Send_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int]
mdi.MDI_Send_Command.restype = ctypes.c_int
def MDI_Send_Command(arg):
    return mdi.MDI_Send_Command(arg)

# MDI_Recv_Command
mdi.MDI_Recv_Command.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_int]
mdi.MDI_Recv_Command.restype = ctypes.c_int
def MDI_Recv_Command(arg):
    return mdi.MDI_Recv_Command(arg)
