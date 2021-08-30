import os
import sys
import glob
import subprocess
import pytest
import time

build_dir = "../build"

driver_out_expected_f90 = """ Engine name: MM
 NNODES:            2
 NODE: @FORCES
 NCOMMANDS:            3
 COMMAND: >FORCES
 NCALLBACKS:            1
 CALLBACK: >FORCES
"""

# Output expected from each of the drivers
driver_out_expected_py = """ Engine name: MM
NNODES: 2
NODE: @FORCES
NCOMMANDS: 3
COMMAND: >FORCES
NCALLBACKS: 1
CALLBACK: >FORCES
NATOMS: 10
COORDS: [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9]
FORCES: [0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29]
FORCES_B: [0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29]
"""


# Includes flags to prevent warning messages
mpiexec_name = "mpiexec"
mpiexec_mca = "mpiexec --mca btl_base_warn_component_unused 0 "
port = 60000

def format_return(input_string):
    my_string = input_string.decode('utf-8')

    # remove any \r special characters, which sometimes are added on Windows
    my_string = my_string.replace('\r','')

    return my_string

# Remove lines that are associated with known / irrelevant warning messages
def parse_stderr(input_string):
    my_string = format_return(input_string)

    ignored_messages = [ "hwloc x86 backend cannot work under Valgrind, disabling.\n",
                         "May be reenabled by dumping CPUIDs with hwloc-gather-cpuid\n",
                         "and reloading them under Valgrind with HWLOC_CPUID_PATH.\n", ]

    for message in ignored_messages:
        my_string = my_string.replace(message, '')

    return my_string

def get_valgrind_options(valgrind):
    if valgrind:
        return ["valgrind",
                "-v",
                "--log-file=" + os.path.dirname(os.path.realpath(__file__)) + "/valgrind_%p_%n.txt",
                "--leak-check=full",
                "--show-leak-kinds=definite",
                "--errors-for-leak-kinds=definite",
                "--trace-children=yes",
                "--track-origins=yes",
                "--error-exitcode=1",
                "--gen-suppressions=all",
                "--suppressions=" + os.path.dirname(os.path.realpath(__file__)) + "/valgrind.supp" ]
    else:
        return []

# Construct launch command correctly, respecting whether the code(s) should be launched with mpiexec or srun
def get_command_line(valgrind=False, nproc1=None, command1=None, nproc2=1, command2=None):
    if command1 is None:
        raise Exception("Error in test_mdi.py script: get_command_line called without command1 argument")
    if nproc2 != 1 and command2 is None:
        raise Exception("Error in test_mdi.py script: get_command_line called with nproc2 but without command2")

    command_line = get_valgrind_options(valgrind)

    if nproc1 is not None:
        command_line += [str(mpiexec_name), "-n", str(nproc1),]
    command_line += [str(command1),]

    if command2 is not None:
        command_line += [":", "-n", str(nproc2), str(command2),]

    return command_line

##########################
# Plugin Tests           #
##########################

def test_cxx_cxx_plug(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name,
                                    "-driver_nranks", "0",
                                    "-plugin_nranks", "1",
                                    "-plugin_name", "engine_cxx",
                                    "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0

def test_cxx_cxx_plug_mpi(valgrind):
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_command = get_command_line(
        valgrind=valgrind,
        nproc1=2,
        command1=[driver_name,
                  "-driver_nranks", "0",
                  "-plugin_nranks", "2",
                  "-plugin_name", "engine_cxx",
                  "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path),
        ],
    )
    driver_proc = subprocess.Popen(driver_command,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0

def test_cxx_f90_plug(valgrind):
    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_command = get_command_line(
        valgrind=valgrind,
        command1=[driver_name,
                  "-driver_nranks", "0",
                  "-plugin_nranks", "1",
                  "-plugin_name", "engine_f90",
                  "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path),
        ],
    )

    # run the calculation
    driver_proc = subprocess.Popen(driver_command,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0

def test_cxx_f90_plug_mpi(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2",
                                    driver_name,
                                    "-driver_nranks", "0",
                                    "-plugin_nranks", "2",
                                    "-plugin_name", "engine_f90",
                                    "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0

def test_cxx_py_plug(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name,
                                    "-driver_nranks", "0",
                                    "-plugin_nranks", "1",
                                    "-plugin_name", "engine_py",
                                    "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_path)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0

def test_cxx_py_plug_mpi(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_plug_cxx*")[0]

    # get the directory of the plugins
    repo_path = os.path.dirname( os.path.dirname(os.path.realpath(__file__)) )
    build_path = os.path.join( repo_path, "build" )

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2",
                                    driver_name,
                                    "-driver_nranks", "0",
                                    "-plugin_nranks", "2",
                                    "-plugin_name", "engine_py",
                                    "-mdi", "-role DRIVER -name driver -method LINK -plugin_path " + str(build_path)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_path)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    expected = '''I am engine instance: 1
 Engine name: MM
'''

    assert driver_err == ""
    assert driver_out == expected
    assert driver_proc.returncode == 0


##########################
# MPI Method             #
##########################

def test_cxx_cxx_mpi(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_out == " Engine name: MM\n"
    assert driver_err == ""
    assert driver_proc.returncode == 0

def test_cxx_cxx_mpi21(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","2",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0

def test_cxx_cxx_mpi12(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","2",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0

def test_cxx_cxx_mpi_serial(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_serial_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_out == " Engine name: MM\n"
    assert driver_err == ""
    assert driver_proc.returncode == 0

def test_cxx_f90_mpi(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0

def test_cxx_py_mpi(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0

def test_f90_cxx_mpi(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0

def test_f90_f90_mpi(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0

def test_f90_py_mpi(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",driver_name, "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0

def test_py_cxx_mpi(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0

def test_py_f90_mpi(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",engine_name,"-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0

def test_py_py_mpi(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])
 
    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0

def test_py_py_mpi_serial(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","1",sys.executable,"driver_py.py", 
                                    "-mdi", "-role DRIVER -name driver -method MPI","-nompi",":",
                                    "-n","1",sys.executable,"engine_py.py",
                                    "-mdi","-role ENGINE -name MM -method MPI","-nompi"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])
 
    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0



##########################
# TCP Method             #
##########################

def test_cxx_cxx_tcp(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_cxx_cxx_tcp_mpi12(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","2",engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_cxx_cxx_tcp_mpi21(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name,"-n","2",driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_cxx_f90_tcp(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_cxx_py_tcp(valgrind):
    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)],
                                   cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_f90_cxx_tcp(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE,  stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_f90_f90_tcp(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_f90_f90_tcp_mpi12(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2", engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_f90_f90_tcp_mpi21(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the names of the driver and engine codes, which include a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2", driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_f90_py_tcp(valgrind):
    global driver_out_expected_f90

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the driver code, which includes a .exe extension on Windows
    driver_name = glob.glob("../build/driver_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [driver_name, "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)],
                                   cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_f90
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_py_cxx_tcp(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_cxx*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_py_f90_tcp(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_f90*")[0]

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [engine_name, "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)])
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_py_py_tcp(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_tup = engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])
    engine_out = format_return(engine_tup[0])
    engine_err = parse_stderr(engine_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert engine_err == ""
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_py_py_tcp_mpi12(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2", sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_tup = engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])
    engine_out = format_return(engine_tup[0])
    engine_err = parse_stderr(engine_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert engine_err == ""
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0

def test_py_py_tcp_mpi21(valgrind):
    global driver_out_expected_py

    valgrind_options = get_valgrind_options(valgrind)

    # run the calculation
    driver_proc = subprocess.Popen(valgrind_options +
                                   [mpiexec_name, "-n", "2", sys.executable, "../build/driver_py.py", "-mdi", "-role DRIVER -name driver -method TCP -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    engine_proc = subprocess.Popen(valgrind_options +
                                   [sys.executable, "../build/engine_py.py", "-mdi", "-role ENGINE -name MM -method TCP -hostname localhost -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    engine_tup = engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])
    engine_out = format_return(engine_tup[0])
    engine_err = parse_stderr(engine_tup[1])

    assert driver_err == ""
    assert driver_out == driver_out_expected_py
    assert engine_err == ""
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0




##########################
# i-PI Tests             #
##########################

@pytest.mark.skipif(os.name == 'nt',
                    reason="the i-PI engine does not work on Windows")
def test_py_cxx_ipi():
    global driver_out_expected_py

    # get the name of the engine code, which includes a .exe extension on Windows
    engine_name = glob.glob("../build/engine_ipi_cxx*")[0]

    # start the driver subprocess
    driver_proc = subprocess.Popen([sys.executable, "../build/driver_ipicomp_py.py", "-mdi", "-role DRIVER -name driver -method TCP -ipi -port " + str(port)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)

    # Ensure that the driver has started, since i-PI requires that the driver is listening when the engines attempt to connect
    time.sleep(3)

    # start the engine subprocess
    engine_proc = subprocess.Popen([engine_name, "-port", str(port), "-hostname", "localhost"])

    # receive the output from the subprocesses
    driver_tup = driver_proc.communicate()
    engine_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = parse_stderr(driver_tup[1])

    assert driver_err == ""
    #assert driver_out == driver_out_expected_py
    assert driver_proc.returncode == 0
    assert engine_proc.returncode == 0
