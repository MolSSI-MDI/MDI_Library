import sys
import pytest


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
    except ImportError: # Check for installed package
        import mdi
    return mdi


##########################
# Unit Conversions Tests #
##########################


def test_atomic_number():
    mdi = import_mdi()

    assert mdi.MDI_String_to_Atomic_Number("H") == 1
    assert mdi.MDI_String_to_Atomic_Number("He") == 2
    assert mdi.MDI_String_to_Atomic_Number("Li") == 3