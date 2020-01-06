import sys
import time
import pytest

try: # Check for local build
    import MDI_Library as mdi
except: # Check for installed package
    import mdi

# Initialize MDI
mdi.MDI_Init("-name driver -role DRIVER -method TEST", None)
comm = mdi.MDI_Accept_Communicator()

# Test MDI_Send and MDI_Recv
mdi.MDI_Send([1, 2], 2, mdi.MDI_INT, comm)
mdi.MDI_Recv(2, mdi.MDI_INT, comm)

# Test using incorrect MDI datatypes
with pytest.raises(Exception):
    mdi.MDI_Send([1, 2], 2, -1234, comm)
with pytest.raises(Exception):
    mdi.MDI_Recv(2, -1234, comm)

# Test registering a node correctly
mdi.MDI_Register_Node("REALNODE")

comm = mdi.MDI_NULL_COMM
long_name = "LONG_NAME_________________________________________________________"

# Test invalid node names
with pytest.raises(Exception):
    mdi.MDI_Register_Node(long_name)
with pytest.raises(Exception):
    mdi.MDI_Check_Node_Exists(long_name, comm)
assert mdi.MDI_Check_Node_Exists("FAKENODE", comm) == 0
with pytest.raises(Exception):
    mdi.MDI_Get_Node(1, comm)

# Test using invalid command names
assert mdi.MDI_Check_Command_Exists("REALNODE","CMDNAME", comm) == 0
with pytest.raises(Exception):
    mdi.MDI_Check_Command_Exists(long_name, "NAME", comm)
with pytest.raises(Exception):
    mdi.MDI_Check_Command_Exists("NAME",long_name, comm)
with pytest.raises(Exception):
    mdi.MDI_Check_Command_Exists("FAKENODE","NAME", comm)
with pytest.raises(Exception):
    mdi.MDI_Get_NCommands(long_name, comm)
with pytest.raises(Exception):
    mdi.MDI_Get_NCommands("FAKENODE", comm)
with pytest.raises(Exception):
    mdi.MDI_Get_Command("FAKENODE", 0, comm)
with pytest.raises(Exception):
    mdi.MDI_Get_Command("REALNODE", 0, comm)

# Test using invalid callback names
mdi.MDI_Check_Callback_Exists("REALNODE","CBKNAME", comm) == 0
with pytest.raises(Exception):
    mdi.MDI_Check_Callback_Exists(long_name, "NAME", comm)
with pytest.raises(Exception):
    mdi.MDI_Check_Callback_Exists("NAME", long_name, comm)
with pytest.raises(Exception):
    mdi.MDI_Check_Callback_Exists("FAKENODE","NAME", comm)
with pytest.raises(Exception):
    mdi.MDI_Get_NCallbacks(long_name, comm)
with pytest.raises(Exception):
    mdi.MDI_Get_NCallbacks("FAKENODE", comm)
with pytest.raises(Exception):
    mdi.MDI_Get_Callback("FAKENODE", 0, comm)
with pytest.raises(Exception):
    mdi.MDI_Get_Callback("REALNODE", 0, comm)
