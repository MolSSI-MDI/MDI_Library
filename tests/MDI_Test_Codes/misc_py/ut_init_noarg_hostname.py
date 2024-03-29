import sys
import time
import pytest

try: # Check for local build
    import MDI_Library as mdi
    testvar = mdi.MDI_COMMAND_LENGTH
except: # Check for installed package
    import mdi

with pytest.raises(Exception):
    mdi.MDI_Init("-name driver -role DRIVER -method TCP -port 8000 -hostname")
