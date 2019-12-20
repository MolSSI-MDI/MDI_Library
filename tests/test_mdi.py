import os
import sys
import glob
import subprocess
try: # Check for local build
    sys.path.append('../build')
    import MDI_Library as mdi
except ImportError: # Check for installed package
    import mdi

build_dir = "../build"

sys.path.append(build_dir)

# Output expected from each of the drivers
driver_out_expected_py = """ Engine name: MM
NATOMS: 10
"""


# Includes flags to prevent warning messages
mpiexec_general = "mpiexec "
mpiexec_mca = "mpiexec --mca btl_base_warn_component_unused 0 "

def format_return(input_string):
    my_string = input_string.decode('utf-8')

    # remove any \r special characters, which sometimes are added on Windows
    my_string = my_string.replace('\r','')

    return my_string

##########################
# LIBRARY Method         #
##########################

def test_cxx_cxx_lib():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_lib_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method LIBRARY"],
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_f90_lib():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_lib_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method LIBRARY"],
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_py_lib():
    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/lib_py.py", "-mdi", "-role DRIVER -name driver -method LIBRARY"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    expected = '''Start of driver
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
NATOMS: 10
'''

    assert driver_err == ""
    assert driver_out == expected

def test_py_py_lib_mpi():
    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","2",sys.executable, "../build/lib_py.py", "-mdi", "-role DRIVER -name driver -method LIBRARY"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    expected = '''Start of driver
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
NATOMS: 10
NATOMS: 20
'''

    assert driver_err == ""
    assert driver_out == expected



##########################
# MPI Method             #
##########################

def test_cxx_cxx_mpi():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_out == " Engine name: MM\n"
    assert driver_err == ""

def test_cxx_f90_mpi():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_cxx_py_mpi():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_cxx_mpi():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_f90_mpi():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_py_mpi():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_cxx_mpi():
    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_f90_mpi():
    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_py_mpi():
    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
 
    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"



##########################
# TCP Method             #
##########################

def test_cxx_cxx_tcp():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_cxx_f90_tcp():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_cxx_py_tcp():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"], 
                                   cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_cxx_tcp():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE,  stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_f90_tcp():
    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_f90_py_tcp():
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen([sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"],
                                   cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_cxx_tcp():
    global driver_out_expected_py

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py

def test_py_f90_tcp():
    global driver_out_expected_py

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen([engine_name, "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py

def test_py_py_tcp():
    global driver_out_expected_py

    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port 8021"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen([sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost"],
                                   cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
#    assert driver_out == " Engine name: MM\n"
    assert driver_out == driver_out_expected_py
