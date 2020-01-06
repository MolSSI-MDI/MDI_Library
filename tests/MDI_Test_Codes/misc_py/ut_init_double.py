import sys
import time
import pytest

try: # Check for local build
    import MDI_Library as mdi
except: # Check for installed package
    import mdi

mdi.MDI_Init("-name driver -role DRIVER -method TEST", None)
with pytest.raises(Exception):
    mdi.MDI_Init("-name driver -role DRIVER -method TEST", None)
