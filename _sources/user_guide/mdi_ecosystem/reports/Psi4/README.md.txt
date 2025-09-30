(Psi4-target)=
# Psi4

## Usage

To use Psi4 as an engine, follow these steps:

1. Install MDI as a Python package, using either Conda (conda install -c conda-forge pymdi) or pip (pip install pymdi).
2. Acquire and compile Psi4, following the Psi4 installation guide. Installing via Conda is recommended.
Prepare a Psi4 input file that specifies the initial molecular geometry of the system and any other desired settings. At the end of the input file, call the energy command with the mdi=True optional argument. For example:

```
molecule {
O 4.92718 4.99809 1.10308
H 5.80905 5.31488 0.90767
H 4.87602 4.14701 0.667997
}

energy("scf/sto-3g", mdi=True)
```
Launch Psi4 using the "--mdi" runtime option, which is described in Launching Codes with the MDI Library. For example:

```bash
psi4 -i psi4.dat --mdi "-role ENGINE -name my_name -method TCP -port 8021 -hostname localhost" &
```


## MDI Mechanic Psi4 report

This repo presents test results for the MDI interface implementation in the Psi4 code.

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
| &lt;@ | ![command](report/badges/box-lightgray.svg) |
| &lt;CELL | ![command](report/badges/box-lightgray.svg) |
| &lt;CELL_DISPL | ![command](report/badges/box-lightgray.svg) |
| &lt;CHARGES | ![command](report/badges/box-brightgreen.svg) |
| &lt;COORDS | ![command](report/badges/box-brightgreen.svg) |
| &lt;DIMENSIONS | ![command](report/badges/box-brightgreen.svg) |
| &lt;ELEC_MULT | ![command](report/badges/box-brightgreen.svg) |
| &lt;ELEMENTS | ![command](report/badges/box-brightgreen.svg) |
| &lt;ENERGY | ![command](report/badges/box-brightgreen.svg) |
| &lt;FORCES | ![command](report/badges/box-brightgreen.svg) |
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
