## Usage

To use QCEngine as an MDI engine, follow these steps:

Install QCEngine, following the [QCEngine installation guide](https://molssi.github.io/QCEngine/install.html).
Prepare a QCEngine input file that specifies the initial molecular geometry of the system and any other desired settings. At the end of the input file, create an "MDIServer" object and call the "start()" method on it. For example:

```python
import qcengine as qcng
import qcelemental as qcel

mol = qcel.models.Molecule.from_data("""
O  0.0  0.000  -0.129
H  0.0 -1.494  1.027
H  0.0  1.494  1.027
""")

o = qcng.MDIServer(mdi_options = "-role ENGINE -name QM -method TCP -port 8021 -hostname localhost",
                   program = "psi4",
                   molecule = mol,
                   model = {"method": "SCF", "basis": "sto-3g"},
                   keywords = {"scf_type": "df"})
o.start()
``` 

Run the input script as normal. **Note that there is NOT a "--mdi" runtime option.**