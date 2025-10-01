
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

