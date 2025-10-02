(QCEngine-target)=
# QCEngine

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

## MDI Mechanic QCEngine report

This repo presents test results for the MDI interface implementation in the QCEngine code.

To view the README.md offline, it is suggested that you use grip (i.e., `pip install grip`).

[yaml]: <> ( prepend )

### Basic Functionality Tests

This section provides the results of several tests performed by MDI Mechanic that are intended to verify that this engine meets the most basic requirements of MDI.
Any functioning MDI engine must successfully pass all of these tests.
The tests are performed in the listed order, and a failure for one test causes all subsequent tests to be skipped and marked as failed.

Developers seeking to implement or maintain MDI support in an engine should resolve the first failed test (if any) and then generate a new report in order to confirm that the test is passed successfully.
Additional information about each test, as well as advice on how to resolve a test if it fails, can be obtained by clicking the test's status badge.
If all of these tests are succussfull, developers are encouraged to begin implementing support for any additional MDI nodes and MDI commands that are appropriate for the engine.

[comment]: <> (Badges are downloaded from shields.io, i.e.:)
[comment]: <> (curl https://img.shields.io/badge/-working-success --output report/badges/-working-success.svg)

1. [![validate_engine](report/dynamic_badges/step_engine_build.svg)](mdimechanic.yml) The engine builds successfully
2. [![min_mdi](report/dynamic_badges/step_min_engine.svg)](report/markdown/minimalistic.md) The engine supports minimalistic MDI communication
3. [![errors_correctly](report/dynamic_badges/step_unsupported.svg)](mdimechanic.yml) The engine correctly responds to unsupported MDI commands
4. [![completed_analysis](report/dynamic_badges/step_mdi_nodes.svg)](mdimechanic.yml) Full analysis of the engine's supported nodes and commands is available

### Nodes

The graph indicates which nodes have been implemented in this engine and the connections between them.

![node_graph](report/graphs/node-report.gv.svg)

### Commands

The following table indicates which MDI Standard commands are supported by this engine at each node.
Supported commands are indicated in green, while unsupported commands are indicated in gray.

[travis]: <> ( supported_commands )
### Supported Commands

| | @DEFAULT |
| ------------- | ------------- |
| &lt;@ | ![command](report/badges/box-brightgreen.svg) |
| &lt;CELL | ![command](report/badges/box-lightgray.svg) |
| &lt;CELL_DISPL | ![command](report/badges/box-lightgray.svg) |
| &lt;CHARGES | ![command](report/badges/box-lightgray.svg) |
| &lt;COORDS | ![command](report/badges/box-brightgreen.svg) |
| &lt;DIMENSIONS | ![command](report/badges/box-lightgray.svg) |
| &lt;ELEC_MULT | ![command](report/badges/box-brightgreen.svg) |
| &lt;ELEMENTS | ![command](report/badges/box-brightgreen.svg) |
| &lt;ENERGY | ![command](report/badges/box-lightgray.svg) |
| &lt;FORCES | ![command](report/badges/box-lightgray.svg) |
| &lt;KE | ![command](report/badges/box-lightgray.svg) |
| &lt;KE_ELEC | ![command](report/badges/box-lightgray.svg) |
| &lt;KE_NUC | ![command](report/badges/box-lightgray.svg) |
| &lt;MASSES | ![command](report/badges/box-brightgreen.svg) |
| &lt;NAME | ![command](report/badges/box-brightgreen.svg) |
| &lt;NATOMS | ![command](report/badges/box-brightgreen.svg) |
| &lt;PE | ![command](report/badges/box-lightgray.svg) |
| &lt;PE_ELEC | ![command](report/badges/box-lightgray.svg) |
| &lt;PE_NUC | ![command](report/badges/box-lightgray.svg) |
| &lt;STRESS | ![command](report/badges/box-lightgray.svg) |
| &lt;TOTCHARGE | ![command](report/badges/box-brightgreen.svg) |
| &lt;VELOCITIES | ![command](report/badges/box-lightgray.svg) |
| &gt;+FORCES | ![command](report/badges/box-lightgray.svg) |
| &gt;CELL | ![command](report/badges/box-lightgray.svg) |
| &gt;CELL_DISPL | ![command](report/badges/box-lightgray.svg) |
| &gt;CHARGES | ![command](report/badges/box-lightgray.svg) |
| &gt;COORDS | ![command](report/badges/box-brightgreen.svg) |
| &gt;ELEC_MULT | ![command](report/badges/box-brightgreen.svg) |
| &gt;ENERGY | ![command](report/badges/box-lightgray.svg) |
| &gt;FORCES | ![command](report/badges/box-lightgray.svg) |
| &gt;MASSES | ![command](report/badges/box-brightgreen.svg) |
| &gt;STRESS | ![command](report/badges/box-lightgray.svg) |
| &gt;TOTCHARGE | ![command](report/badges/box-brightgreen.svg) |
| &gt;VELOCITIES | ![command](report/badges/box-lightgray.svg) |
| @ | ![command](report/badges/box-lightgray.svg) |
| @INIT_MC | ![command](report/badges/box-lightgray.svg) |
| @INIT_MD | ![command](report/badges/box-lightgray.svg) |
| @INIT_OPTG | ![command](report/badges/box-lightgray.svg) |
| EXIT | ![command](report/badges/box-lightgray.svg) |

### Acknowledgements

Badges are obtained from the ![shields.io](https://shields.io/) project.
