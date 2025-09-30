# Driver Use Tutorial: Running an AIMD Simulation

:::{admonition} Prerequisites

Completion of this tutoral requires installing Docker.
Docker is a platform that allows you to run software in a container, which is a standalone environment with everything needed to run the software, including the software itself and all its dependencies.
If you don't have Docker installed already, we recommend downloading and installing [Docker Desktop](https://www.docker.com/products/docker-desktop) for your operating system.

You will also need to have `mdimechanic`. 
We recommend using an environment manager like `conda` or `pipenv` to install `mdimechanic` in a clean environment.

To install `mdimechanic`, you can use the following command:

```bash
pip install mdimechanic
```

We also recommend using `git`, though it is not absolutely necessary.

:::

In this tutorial we will use an MDI driver to perform a simple *Ab Initio* Molecular Dynamics (AIMD) simulation, using Quantum ESPRESSO (QE) to calculate forces and LAMMPS to update the atomic coordinates each time step.
In an AIMD simulation, the forces on the atoms are calculated using quantum mechanics, and the atomic coordinates are updated using classical mechanics.
Thus, we will use a QM code (in this case, Quantum ESPRESSO) to calculate the forces, and an MM code (in this case, LAMMPS) to perform the time integration.

LAMMPS is a code that simulates chemical systems using molecular mechanics (MM) force fields, which are computationally very efficient to evaluate and can be used for lengthy dynamics simulations of large systems.
Quantum ESPRESSO is a code that runs quantum mechanics (QM) calculations, which are much more computationally expensive, but can be applied much more straigtforwardly to chemically reactive systems and other specific use cases.

MM codes like LAMMPS often offer time integration features that are not widely available in QM codes, so it can be desirable to be able to mix the time integration functionality of an MM code with the force evaluation functionality of a QM code.

Although we will be using QE and LAMMPS as the MDI engines in this tutorial, you could do similar calculations using other MDI engines. This means you could switch out LAMMPS for another MM code, or QE for another QM code, and the MDI driver would still work as long as you had files to set up the input of the new codes.

A list of codes that support MDI can be found in [mdi_ecosystem](#mdi-ecosystem).

## Obtaining Tutorial Materials

To get started with this tutorial, first download the necessary files using `git` (note: You can also download the files as a zip file from the GitHub repository if you don't have `git` installed using [this link](https://github.com/janash/MDI_AIMD_user_tutorial/archive/refs/heads/main.zip)):


:::{tab-set-code}
```bash
git clone https://github.com/janash/MDI_AIMD_user_tutorial.git
cd MDI_AIMD_user_tutorial
```
:::

Often in computational chemistry, one of the hardest steps to performing a calculation is setting up and compiling the software.
Fortunately, we have compiled Docker images that contain the MDI-enabled codes we will use in this tutorial and that you can use for other calculations. 
This means you do not have to compile the codes, you just have to install Docker and "pull" (download) the images.
The `mdimechanic` software that you installed in step 1 will do this for you when we run the simulation.

## Setting Up the AIMD Simulation

When using an already-made MDI driver, you will be using a software package that someone else has prepared that performs some kind of molecular science calculation.
In this case, the MDI Driver performs ab-initio molecular dynamics (AIMD) calculations.
As far as the MDI driver is concerned, the only thing you need to know is how to run it and what files it needs to run.

For the AIMD Driver we will use today:

* You will need to specify what program you would like to use for the QM portion.
* You will need to provide input files for the QM portion that match your choice of QM program.
* You will need to specify what program you would like it to use for MM portion.
* You will need to provide input files for the MM portion that match your choice of MM program.
* You will need an input for MDIMechanic.

The MDI driver will then take care of the rest.

This means you have freedom over your input parameters for each program, and freedom over the input system that you simulate. 
For a calculation that is not pre-prepared, preparing your input files can be a time-consuming part of the calculation.

For the purposes of this tutorial, we will use prepared input files to simulate a small water system using Quantum ESPRESSO and LAMMPS. 
You can find input files for this simulation in `simulation_files/starting` in the repository you just cloned.

When viewing the contents of this directory, you will see inputs for lammps (`lammps.in`, `lammps.data`) and input files for Quantum ESPRESSO (`qe.in`, `pseudo` - directory containing pseudopotential parameters).
If you wanted to simulate a different system using AIMD, you would use the same driver, but change these input files.

:::{admonition} Input File Considerations
:class: caution

In order to run this simulation, we have to make sure our input files for the quantum program
and our input files for the molecular dynamics program are compatible with each other.
This means that the systems for both must have the same number of atoms, and the atoms must be in the same order.

If using the AIMD Driver, you should also be careful that your boundary conditions match in both programs. In our tutorial, we are using a system with periodic boundary conditions in both programs, and have been careful to set the system up so that the box sizes match. 
:::

## Running the AIMD Simulation
After you have the input files, you can run the AIMD simulation using the MDI driver and MDI Mechanic.
When we use this AIMD Driver, LAMMPS and Quantum ESPRESSO will both be running, but the Driver will be sending information between the two programs. 
We will use MDI Mechanic to launch the processes.

This Driver works in the following way:

1. Both programs are initialized with their respective input files. In the AIMD Driver, the initial coordinates. are read from your LAMMPS input.
2. Coordinates are sent from LAMMPS to Quantum ESPRESSO to calculate forces.
3. Forces are calculated in our water system using Quantum ESPRESSO.
4. The AIMD Driver retrieves the forces from Quantum ESPRESSO and replaces the forces in the LAMMPS program for that time-step with these forces.
5. LAMMPS is used to update the atomic coordinates using the forces from Quantum ESPRESSO.
6. The AIMD Driver retrieves the new atomic coordinates from LAMMPS and sends them to Quantum ESPRESSO to calculate the forces for the next time step.

Our output will be the atomic coordinates and forces at each time step written by LAMMPS, which we can use to analyze the dynamics of our water system.

To start our simulation, we'll first copy our starting files to a new directory to ensure that we don't overwrite them. 
Note that you must use this exact file name for the configuration of our tutorial.

:::{tab-set-code}
```bash
mkdir simulation_files/working
cp -r simulation_files/starting/* simulation_files/working
```
:::

From the top level of your cloned repository, run the following command to start the AIMD simulation:

:::{tab-set-code}
```bash 
mdimechanic run --name aimd
```
:::

This will start an AIMD simulation. 
You may have to wait a minute or two for this to run. 
During this time, you will not see any message to the screen, but you should see files changing in your `simulation_files/working` directory.

## What did `mdimechanic` do?

As you wait for your simulation to run, you might want to take a look at the `mdimechanic` command you just ran. 

Open the file `mdimechanic.yml` in your text editor to view the input to MDI.

:::{tab-set-code}
```mdimechanic.yml
code_name: 'MDI_AIMD_Driver'
docker:
  image_name: ''

run_scripts:
  aimd:
    containers:
      aimd:
        image: 'janash/aimd_driver:slim'
        script:
          - echo "Starting AIMD driver"
          - export PATH=$PATH:/repo/build/MDI_AIMD_Driver/build/MDI_AIMD_Driver
          - cd simulation_files/working
          - MDI_AIMD_Driver -mdi "-role DRIVER -name driver -method TCP -port 8021"
      lammps:
        image: 'janash/mdi-lammps:slim'
        script:
          - cd simulation_files/working
          - lmp_mpi -mdi "-role ENGINE -name MM -method TCP -port 8021 -hostname aimd" -in lammps.in > lammps.out
      qe:
        image: 'janash/mdi-qe:slim'
        script:
          - export PATH=$PATH:/repo/build/q-e/MDI/src
          - export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/repo/build/q-e/MDI/build/mdi_build-prefix/src/mdi_build-build/MDI_Library
          - cd simulation_files/working
          - qemdi.x -mdi "-role ENGINE -name QM -method TCP -port 8021 -hostname aimd" -in qe.in > qe.out

```
:::

This file tells `mdimechanic` how to run the simulation.
The important part of this file is in the `run_scripts` section.
Here, we have three containers: `aimd`, `lammps`, and `qe`.
Each container has an `image` that tells `mdimechanic` which Docker image to use for that container.
Each image is a standalone program that is ready to run with MDI and downloaded the first time you run the simulation. 
This takes the place of you having to compile the programs yourself.

The `script` section of each container tells `mdimechanic` what commands to run in each container.
At the end of each script, you will see a command that starts each program that will look very similar to other launch commands you may have used with these programs in the past. 
This time, however, you will see an extra flag `-mdi` that enables the MDI interface for the program.
For example, the LAMMPS command has the usual input and output for LAMMPS, but also has the flag `-mdi "-role ENGINE -name MM -method TCP -port 8021 -hostname aimd"`.

```bash
lmp_mpi -mdi "-role ENGINE -name MM -method TCP -port 8021 -hostname aimd" -in lammps.in > lammps.out
```

When using the MolSSI Driver Interface, the programs have a couple ways that they can communicate with each other. MDI allows communication between programs over TCP/IP or Message Passing Interface (MPI).
TCP/IP operates through network ports, which act like designated channels on a server for specific types of network traffic. 
MDI allows computatoinal chemistry code to communicate on-the-fly (as they run) with other codes through these ports.
You will see that in our example, the codes have all been set up to communicate over TCP/IP with the port number `8021`.

:::{admonition} What's going on under the hood?
:class: tip

If you're familiar with Docker, you may wonder more about what MDI Mechanic is actually doing. MDI Mechanic sets up files to run Docker Compose to orchestrate the running of the Docker containers. You can see the generated Docker Compose file in the `.mdimechanic/.temp` directory in repostiorty.
MDI Mechanic generates these based on the YAML for every run.

:::

## Examining the Output
At the end of the simulation, you should see an output similar to the following:

```
Running a custom calculation with MDI Mechanic.
====================================================
================ Output from Docker ================
====================================================
Attaching to aimd-1, lammps-1, qe-1
aimd-1    | Starting AIMD driver
aimd-1    | Engine name: QM
aimd-1    | Engine name: MM
aimd-1    | timestep: 0 -0.0978346 -137.599
aimd-1    | timestep: 1 -0.100167 -137.602
aimd-1    | timestep: 2 -0.103482 -137.607
aimd-1    | timestep: 3 -0.109034 -137.61
aimd-1    | timestep: 4 -0.114992 -137.611
aimd-1    | timestep: 5 -0.115626 -137.614
aimd-1    | timestep: 6 -0.107206 -137.621
aimd-1    | timestep: 7 -0.0914601 -137.63
aimd-1    | timestep: 8 -0.0743942 -137.637
aimd-1    | timestep: 9 -0.0616925 -137.641
aimd-1 exited with code 0
lammps-1 exited with code 0
qe-1 exited with code 0

====================================================
============== End Output from Docker ==============
====================================================
```

Your `simulation_files/working` directory should now contain a number of files, including `dump.lammpstrj`, `log.lammps` and `qe.out`. 
If you are familiar with any of these programs, the output files will look the same as they usually do.
However, this time, the trajectory produced by LAMMPS was generated using forces calculated by Quantum ESPRESSO!

## Analyzing the Results

You can now continue with analysis as you would normally analyze your simulation output files.

## Extending to Other Drivers

After completing this tutorial, you may wonder how you can use MDI, other drivers, and other codes. 
As mentioned earlier, in the tutorial, there are several methods to using MDI. 
In this tutorial, we have covered a containerization approach using MDI Mechanic.
In order to use this approach, you need to have Docker installed and prepare an `mdimechanic.yml` file.

You then specify the container images for your programs, and the commands to run them with the MDI interface. 
You can see a list of available MDI-enabled codes in the [MDI Ecosystem](#mdi-ecosystem) or check out MDI's DockerHub page for a list of MDI-enabled images.
