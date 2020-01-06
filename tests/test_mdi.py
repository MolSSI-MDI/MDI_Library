import os
import sys
import glob
import subprocess
import pytest
try: # Check for local build
    sys.path.append('../build')
    import MDI_Library as mdi
except ImportError: # Check for installed package
    import mdi

build_dir = "../build"

sys.path.append(build_dir)

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
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method LIB"],
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
    driver_proc = subprocess.Popen([driver_name, "-mdi", "-role DRIVER -name driver -method LIB"],
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])

    assert driver_err == ""
    assert driver_out == " Engine name: MM\n"

def test_py_py_lib():
    # run the calculation
    driver_proc = subprocess.Popen([sys.executable, "../build/lib_py.py", "-mdi", "-role DRIVER -name driver -method LIB"],
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
    driver_proc = subprocess.Popen(["mpiexec","-n","2",sys.executable, "../build/lib_py.py", "-mdi", "-role DRIVER -name driver -method LIB"],
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
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

def test_f90_f90_mpi():
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

def test_f90_py_mpi():
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

def test_py_cxx_mpi():
    global driver_out_expected_py

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
    assert driver_out == driver_out_expected_py

def test_py_f90_mpi():
    global driver_out_expected_py

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
    assert driver_out == driver_out_expected_py

def test_py_py_mpi():
    global driver_out_expected_py

    # run the calculation
    driver_proc = subprocess.Popen(["mpiexec","-n","1",sys.executable,"driver_py.py", "-mdi", "-role DRIVER -name driver -method MPI",":",
                                    "-n","1",sys.executable,"engine_py.py","-mdi","-role ENGINE -name MM -method MPI"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()

    # convert the driver's output into a string
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
 
    assert driver_err == ""
    assert driver_out == driver_out_expected_py



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
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

def test_f90_f90_tcp():
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

def test_f90_py_tcp():
    global driver_out_expected_f90

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
    assert driver_out == driver_out_expected_f90

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



##########################
# Unit Conversions Tests #
##########################

def test_unit_conversions_py():

    # Test all charge conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_charge", "atomic_unit_of_charge") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_charge", "coulomb") == pytest.approx(1.6021766208e-19)
    assert mdi.MDI_Conversion_Factor("coulomb", "atomic_unit_of_charge") == pytest.approx(1.0 / 1.6021766208e-19)
    assert mdi.MDI_Conversion_Factor("coulomb", "coulomb") == pytest.approx(1.0)

    # Test some energy conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "atomic_unit_of_energy") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "calorie") == pytest.approx(1.0420039967034203e-18)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "electron_volt") == pytest.approx(27.211386245988066)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "hartree") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "inverse_meter_energy") == pytest.approx(21947463.136319984)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "joule") == pytest.approx(4.35974465e-18)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "kelvin_energy") == pytest.approx(315775.02480406954)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "kilocalorie") == pytest.approx(1.0420039967034203e-21)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "kilocalorie_per_mol") == pytest.approx(627.5094737775374)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "kilojoule") == pytest.approx(4.3597446499999996e-21)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "kilojoule_per_mol") == pytest.approx(2625.4996382852164)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy", "rydberg") == pytest.approx(2.0)
    assert mdi.MDI_Conversion_Factor("calorie", "atomic_unit_of_energy") == pytest.approx(1.0 / 1.0420039967034203e-18)
    assert mdi.MDI_Conversion_Factor("electron_volt", "atomic_unit_of_energy") == pytest.approx(1.0 / 27.211386245988066)
    assert mdi.MDI_Conversion_Factor("hartree", "atomic_unit_of_energy") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("inverse_meter_energy", "atomic_unit_of_energy") == pytest.approx(1.0 / 21947463.136319984)
    assert mdi.MDI_Conversion_Factor("joule", "atomic_unit_of_energy") == pytest.approx(1.0 / 4.35974465e-18)
    assert mdi.MDI_Conversion_Factor("kelvin_energy", "atomic_unit_of_energy") == pytest.approx(1.0 / 315775.02480406954)
    assert mdi.MDI_Conversion_Factor("kilocalorie", "atomic_unit_of_energy") == pytest.approx(1.0 / 1.0420039967034203e-21)
    assert mdi.MDI_Conversion_Factor("kilocalorie_per_mol", "atomic_unit_of_energy") == pytest.approx(1.0 / 627.5094737775374)
    assert mdi.MDI_Conversion_Factor("kilojoule", "atomic_unit_of_energy") == pytest.approx(1.0 / 4.3597446499999996e-21)
    assert mdi.MDI_Conversion_Factor("kilojoule_per_mol", "atomic_unit_of_energy") == pytest.approx(1.0 / 2625.4996382852164)
    assert mdi.MDI_Conversion_Factor("rydberg", "atomic_unit_of_energy") == pytest.approx(0.5)

    # Test all force conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_force", "atomic_unit_of_force") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_force", "newton") == pytest.approx(3.753838631429819e-15)
    assert mdi.MDI_Conversion_Factor("newton", "atomic_unit_of_force") == pytest.approx(1.0 / 3.753838631429819e-15)
    assert mdi.MDI_Conversion_Factor("newton", "newton") == pytest.approx(1.0)

    # Test some length conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "angstrom") == pytest.approx(0.52917721067)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "atomic_unit_of_length") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "bohr") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "meter") == pytest.approx(5.29177210903e-11)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "nanometer") == pytest.approx(5.29177210903e-2)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_length", "picometer") == pytest.approx(5.29177210903e+1)
    assert mdi.MDI_Conversion_Factor("angstrom", "atomic_unit_of_length") == pytest.approx(1.0 / 0.52917721067)
    assert mdi.MDI_Conversion_Factor("bohr", "atomic_unit_of_length") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("meter", "atomic_unit_of_length") == pytest.approx(1.0 / 5.29177210903e-11)
    assert mdi.MDI_Conversion_Factor("nanometer", "atomic_unit_of_length") == pytest.approx(1.0 / 5.29177210903e-2)
    assert mdi.MDI_Conversion_Factor("picometer", "atomic_unit_of_length") == pytest.approx(1.0 / 5.29177210903e+1)

    # Test all mass conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_mass", "atomic_unit_of_mass") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_mass", "kilogram") == pytest.approx(9.10938356e-31)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_mass", "gram") == pytest.approx(9.10938356e-28)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_mass", "atomic_mass_unit") == pytest.approx(0.0005485799093287202)
    assert mdi.MDI_Conversion_Factor("kilogram", "atomic_unit_of_mass") == pytest.approx(1.0 / 9.10938356e-31)
    assert mdi.MDI_Conversion_Factor("kilogram", "kilogram") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("kilogram", "gram") == pytest.approx(1000.0)
    assert mdi.MDI_Conversion_Factor("kilogram", "atomic_mass_unit") == pytest.approx(6.022140858549162e+26)
    assert mdi.MDI_Conversion_Factor("gram", "atomic_unit_of_mass") == pytest.approx(1.0 / 9.10938356e-28)
    assert mdi.MDI_Conversion_Factor("gram", "kilogram") == pytest.approx(0.001)
    assert mdi.MDI_Conversion_Factor("gram", "gram") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("gram", "atomic_mass_unit") == pytest.approx(6.0221408585491626e+23)
    assert mdi.MDI_Conversion_Factor("atomic_mass_unit", "atomic_unit_of_mass") == pytest.approx(1.0 / 0.0005485799093287202)
    assert mdi.MDI_Conversion_Factor("atomic_mass_unit", "kilogram") == pytest.approx(1.66053904e-27)
    assert mdi.MDI_Conversion_Factor("atomic_mass_unit", "gram") == pytest.approx(1.66053904e-24)
    assert mdi.MDI_Conversion_Factor("atomic_mass_unit", "atomic_mass_unit") == pytest.approx(1.0)

    # Test all time conversions
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_time", "atomic_unit_of_time") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_time", "picosecond") == pytest.approx(2.4188843265857007e-05)
    assert mdi.MDI_Conversion_Factor("atomic_unit_of_time", "second") == pytest.approx(2.4188843265857007e-17)
    assert mdi.MDI_Conversion_Factor("picosecond", "atomic_unit_of_time") == pytest.approx(1.0 / 2.4188843265857007e-05)
    assert mdi.MDI_Conversion_Factor("picosecond", "picosecond") == pytest.approx(1.0)
    assert mdi.MDI_Conversion_Factor("picosecond", "second") == pytest.approx(1.0e-12)
    assert mdi.MDI_Conversion_Factor("second", "atomic_unit_of_time") == pytest.approx(1.0 / 2.4188843265857007e-17)
    assert mdi.MDI_Conversion_Factor("second", "picosecond") == pytest.approx(1.0e+12)
    assert mdi.MDI_Conversion_Factor("second", "second") == pytest.approx(1.0)

    # Test exceptions for unrecognized units
    with pytest.raises(Exception):
        assert mdi.MDI_Conversion_Factor("fake_unit","bohr")
    with pytest.raises(Exception):
        assert mdi.MDI_Conversion_Factor("angstrom","")

    # Test exceptions for inconsistent unit types
    with pytest.raises(Exception):
        assert mdi.MDI_Conversion_Factor("atomic_unit_of_energy","atomic_unit_of_time")
    with pytest.raises(Exception):
        assert mdi.MDI_Conversion_Factor("meter","calorie")



##########################
# Error Tests            #
##########################

def test_uninitialized():
    comm = mdi.MDI_NULL_COMM

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
MDI_Get_Node unable to find node
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
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test leaving off the -driver_name argument
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_noarg_driver_name.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """Error in MDI_Init: Argument missing from -driver_name option
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
"""
    assert driver_err == expected_err
    assert driver_out == ""

    # Test double initialization
    driver_proc = subprocess.Popen([sys.executable, "../build/ut_init_double.py"],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=build_dir)
    driver_tup = driver_proc.communicate()
    driver_out = format_return(driver_tup[0])
    driver_err = format_return(driver_tup[1])
    expected_err = """MDI_Init called after MDI was already initialized
"""
    assert driver_err == expected_err
    assert driver_out == ""
