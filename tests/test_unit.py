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
# Unit Conversions Tests #
##########################

def test_unit_conversions_py():
    mdi = import_mdi()

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
