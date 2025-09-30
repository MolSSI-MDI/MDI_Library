(LAMMPS-target)=
# LAMMPS


## Usage

To use LAMMPS as an engine, follow these steps:

Acquire and compile LAMMPS, following the [LAMMPS build guide](https://lammps.sandia.gov/doc/Build.html). You will need to enable the [USER-MDI package](https://docs.lammps.org/Build_extras.html) when building LAMMPS.
Prepare a LAMMPS input file that specifies the initial molecular geometry of the system, the force field to be used, and any other desired settings. At the end of the input file, call the mdi/engine command, which does not accept any arguments. For example:

```
units           real
neigh_modify    delay 0 every 1 check yes
atom_style      full
bond_style      harmonic
angle_style     harmonic
pair_style      lj/cut/coul/long 10.0
pair_modify     mix arithmetic
kspace_style    pppm 1e-4
special_bonds   amber

atom_modify     sort 0 0

read_data       lammps.data

timestep        1.0

dump            1 all custom 1 dump.lammpstrj id element xu yu zu
dump            2 all custom 1 dump.force id element fx fy fz
dump            3 all xyz 1 dump.xyz
dump_modify     1 element O H
dump_modify     2 element O H

thermo_style    multi
thermo          1

fix             1 all nvt temp 300.0 300.0 70.0
fix             2 all shake 0.0001 20 0 b 1 a 1

mdi/engine
```

Launch LAMMPS using the "-mdi" runtime option, which is described in Launching Codes with the MDI Library. For example:

```bash
lammps -in lammps.in -mdi "-role ENGINE -name my_name -method TCP -port 8021 -hostname localhost" &
```



[![Build Status](https://github.com/MolSSI-MDI/MDI_lammps/workflows/CI/badge.svg)](https://github.com/MolSSI-MDI/MDI_lammps/actions/)

## MDI Mechanic LAMMPS report

This repo presents test results for the MDI interface implementation in the LAMMPS code.

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

| | @COORDS | @DEFAULT | @ENDSTEP | @FORCES | @INIT_MD | @INIT_OPTG |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| &lt;@ | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) |
| &lt;CELL | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;CELL_DISPL | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;CHARGES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;COORDS | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;DIMENSIONS | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;ELEC_MULT | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;ELEMENTS | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;ENERGY | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;FORCES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;KE | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;KE_ELEC | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;KE_NUC | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;MASSES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;NAME | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) |
| &lt;NATOMS | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;PE | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;PE_ELEC | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;PE_NUC | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;STRESS | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;TOTCHARGE | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &lt;VELOCITIES | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;+FORCES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;CELL | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;CELL_DISPL | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;CHARGES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;COORDS | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;ELEC_MULT | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;ENERGY | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;FORCES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;MASSES | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;STRESS | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;TOTCHARGE | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| &gt;VELOCITIES | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| @ | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-brightgreen.svg) |
| @INIT_MC | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| @INIT_MD | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| @INIT_OPTG | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-brightgreen.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |
| EXIT | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) | ![command](report/badges/box-lightgray.svg) |

### Acknowledgements

Badges are obtained from the ![shields.io](https://shields.io/) project.
