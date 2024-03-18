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
