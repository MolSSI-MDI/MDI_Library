import os
import sys
import glob
import subprocess
import pytest
import time

build_dir = "../build"

sys.path.append(build_dir)

def format_return(input_string):
    my_string = input_string.decode('utf-8')

    # remove any \r special characters, which sometimes are added on Windows
    my_string = my_string.replace('\r','')

    return my_string

def import_mdi():
    try: # Check for local build
        sys.path.append('../build')
        import MDI_Library as mdi
        try:
            testvar = mdi.MDI_COMMAND_LENGTH
        except AttributeError:
            import mdi
    except ImportError: # Check for installed package
        import mdi
    return mdi


##########################
# Error Tests            #
##########################

def test_uninitialized():
    mdi = import_mdi()
    comm = mdi.MDI_COMM_NULL

    # Test exceptions when MDI is not initialized
    with pytest.raises(Exception):
        mdi.MDI_Accept_Communicator()
    with pytest.raises(Exception):
        mdi.MDI_Send([1, 2], 2, mdi.MDI_INT, comm)
    with pytest.raises(Exception):
        mdi.MDI_Recv(2, mdi.MDI_INT, comm)
    with pytest.raises(Exception):
        mdi.MDI_Send_Command("<VERSION", comm)
    with pytest.raises(Exception):
        mdi.MDI_Recv_Command(comm)
    with pytest.raises(Exception):
        mdi.MDI_Register_Node("TESTNODE")
    with pytest.raises(Exception):
        mdi.MDI_Check_Node_Exists("TESTNODE", comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_Node(0, comm, "TESTNODE")
    with pytest.raises(Exception):
        mdi.MDI_Get_NNodes(comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_Node(0, comm)
    with pytest.raises(Exception):
        mdi.MDI_Register_Command("TESTNODE", "TESTCOMM")
    with pytest.raises(Exception):
        mdi.MDI_Check_Command_Exists("TESTNODE", "TESTCOMM", comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_NCommands("TESTNODE", comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_Command("TESTNODE", 0, comm)
    with pytest.raises(Exception):
        mdi.MDI_Register_Callback("TESTNODE", "TESTCALL")
    with pytest.raises(Exception):
        mdi.MDI_Check_Callback_Exists("TESTNODE", "TESTCALL", comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_NCallbacks("TESTNODE", comm)
    with pytest.raises(Exception):
        mdi.MDI_Get_Callback("TESTNODE", 0, comm)

def test_test_method():

    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_tmethod.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    expected_err = """Cannot register node name with length greater than MDI_COMMAND_LENGTH
Node name is greater than MDI_COMMAND_LENGTH
Vector accessed out-of-bounds
Error in MDI_Get_node: vector_get failed
Node name is greater than MDI_COMMAND_LENGTH
Cannot chcek command name with length greater than MDI_COMMAND_LENGTH
Could not find the node
Node name is greater than MDI_COMMAND_LENGTH
Could not find the node
MDI_Get_Command could not find the requested node
MDI_Get_Command failed because the command does not exist
Node name is greater than MDI_COMMAND_LENGTH
Cannot check callback name with length greater than MDI_COMMAND_LENGTH
Could not find the node
Node name is greater than MDI_COMMAND_LENGTH
Could not find the node
MDI_Get_Command could not find the requested node
MDI_Get_Command failed because the command does not exist
"""

    assert driver_err == expected_err
    assert driver_out == ""

def test_init_errors():
    # Test running with no -method option
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_method.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = ""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with no -name option
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_name.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: -name option not provided
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with no -role option
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_role.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: -role option not provided
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with no -port option for a DRIVER using TCP
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_port_d.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: -port option not provided
MDI method on_selection function failed
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with no -port option for an ENGINE using TCP
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_port_e.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: -port option not provided
MDI method on_selection function failed
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with no -hostname option for an ENGINE using TCP
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_no_hostname.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: -hostname option not provided
MDI method on_selection function failed
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with a fake option
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_fake_opt.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Unrecognized option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with a fake method
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_fake_method.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Method not recognized
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test running with a fake role
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_fake_role.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Role not recognized
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -role argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_role.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -role option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -method argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_method.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = ""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -name argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_name.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -name option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -hostname argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_hostname.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -hostname option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -port argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_port.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -port option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -out argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_out.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -out option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -_language argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_language.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -_language option
Error in MDI_Init_with_options
"""
    assert driver_err == expected_err
    assert driver_out == ""
