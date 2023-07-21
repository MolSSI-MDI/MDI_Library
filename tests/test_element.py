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


def test_atomic_number():
    mdi = import_mdi()

    assert mdi.MDI_String_to_atomic_number("H") == 1
    assert mdi.MDI_String_to_atomic_number("He") == 2
    assert mdi.MDI_String_to_atomic_number("Li") == 3
    assert mdi.MDI_String_to_atomic_number("Se") == 34
    assert mdi.MDI_String_to_atomic_number("Os") == 76

def test_atomic_number_unrecognized():
    mdi = import_mdi()

    with pytest.raises(Exception):
        mdi.MDI_String_to_atomic_number("YY")
