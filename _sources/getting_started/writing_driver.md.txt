# Driver Development Tutorial: Developing an MDI Driver for Running ab initio MD

Open the file called `aimd.cpp`.
It contains the following code:

:::{tab-set-code}
```c++
#include <iostream>
#include <mpi.h>
#include <stdexcept>
#include <string.h>
#include "mdi.h"

using namespace std;

int main(int argc, char **argv) {

  // Initialize the MPI environment
  MPI_Comm world_comm;
  MPI_Init(&argc, &argv);

  // Initialize MDI
  if ( MDI_Init(&argc, &argv) ) {
    throw std::runtime_error("The MDI library was not initialized correctly.");
  }

  // Confirm that MDI was initialized successfully
  int initialized_mdi;
  if ( MDI_Initialized(&initialized_mdi) ) {
    throw std::runtime_error("MDI_Initialized failed.");
  }
  if ( ! initialized_mdi ) {
    throw std::runtime_error("MDI not initialized: did you provide the -mdi option?.");
  }

  // Get the correct MPI intra-communicator for this code
  if ( MDI_MPI_get_world_comm(&world_comm) ) {
    throw std::runtime_error("MDI_MPI_get_world_comm failed.");
  }

  // Connect to the engines
  // <YOUR CODE GOES HERE>

  // Perform the simulation
  // <YOUR CODE GOES HERE>

  // Send the "EXIT" command to each of the engines
  // <YOUR CODE GOES HERE>

  // Synchronize all MPI ranks
  MPI_Barrier(world_comm);
  MPI_Finalize();

  return 0;
}
```
:::

The first few lines of code simply initialize both MPI and and MDI.
Don't worry if you don't have access to an MPI library - the code will just fall back to a set of dummy MPI functions provided in `STUBS_MPI`.
The call to the `MDI_MPI_get_world_comm()` function obtains from MDI an MPI intra-communicator that spans all MPI ranks associated with the driver.
This call is important when using the MPI protocol for MDI communication, because in that context, `MPI_COMM_WORLD` spans all ranks associated with both the driver and the engine(s).
In general, an MDI-enabled code should call `MDI_MPI_get_world_comm()` immediately after calling `MDI_Init()`, and the resulting MPI intra-communicator should be used instead of `MPI_COMM_WORLD` throughout the remainder of the code.

After initializing MDI, we need to connect the driver to its engines.
For this particular tutorial, we will use a QM engine to calculate forces and an MM engine to update the atomic coordinates each timestep.
In the MDI standard, engines request a connection to a driver.
The driver will need to call the `MDI_Accept_communicator()` function to accept each connection.
The general format of this functional call looks like this:

:::{tab-set-code}
```c++
MDI_Comm comm;
MDI_Accept_communicator(&comm);
```
:::

This will assign a new MDI communicator to \c comm, which functions very similarly to an MPI communicator and is used in certain MDI function calls to identify which engine is expected to send/receive data to/from the driver.
Our AIMD driver will connect to two different engines, so we will be calling \c MDI_Accept_communicator() twice.
We don't know the order in which the engines will request a connection to the driver, so we will need some way to determine which engine is the QM code and which engine is the MM code.

This can be accomplished through the use of the `<NAME` command.
The entire MDI standard is built around the idea that drivers can send "commands" to engines, each of which is defined by the MDI standard and has a specific outcome.
The `<NAME` command tells the engine to send the driver a string of length `MDI_NAME_LENGTH` that identifies the engine.
The end-user is responsible for indicating the name of each engine at runtime, using the `-name` command line argument that was described in the user_tutorial.
As authors of a driver, we will need to decide what we expect each of the engines to be named and clearly document that decision for the benefit of the end-users.
For the purpose of this tutorial, we will expect the quantum mechanics code to be called "QM" and the molecular mechanics code to be named "MM".

Sending the `<NAME` command to an engine and then receiving the engine's name can be done as follows:

:::{tab-set-code}
```c++
char* code_name = new char[MDI_NAME_LENGTH];
MDI_Send_command("<NAME", comm);
MDI_Recv(code_name, MDI_NAME_LENGTH, MDI_CHAR, comm);
\endcode

The following code accepts connections from two engines and assigns the communicator from the engine named "MM" to `mm_comm` and assigns the communicator from the engine named "QM" to `qm_comm`.
Replace the comment that reads `// Connect to the engines` with:

\code
  // Connect to the engines
  MDI_Comm mm_comm = MDI_COMM_NULL;
  MDI_Comm qm_comm = MDI_COMM_NULL;
  int nengines = 2;
  for (int iengine=0; iengine < nengines; iengine++) {
    MDI_Comm comm;
    MDI_Accept_communicator(&comm);
 
    // Determine the name of this engine
    char* engine_name = new char[MDI_NAME_LENGTH];
    MDI_Send_command("<NAME", comm);
    MDI_Recv(engine_name, MDI_NAME_LENGTH, MDI_CHAR, comm);
 
    cout << "Engine name: " << engine_name << endl;
 
    if ( strcmp(engine_name, "MM") == 0 ) {
      if ( mm_comm != MDI_COMM_NULL ) {
	throw runtime_error("Accepted a communicator from a second MM engine.");
      }
      mm_comm = comm;
    }
    else if ( strcmp(engine_name, "QM") == 0 ) {
      if ( qm_comm != MDI_COMM_NULL ) {
	throw runtime_error("Accepted a communicator from a second QM engine.");
      }
      qm_comm = comm;
    }
    else {
      throw runtime_error("Unrecognized engine name.");
    }
 
    delete[] engine_name;
  }
```
:::

We are now ready to use MDI to orchestrate an AIMD simulation.
In broad terms, during each dynamics iteration we will send a set of atomic coordinates from the MM engine to the QM engine, order the QM engine to send the driver the corresponding atomic forces, send those forces to the MM engine, and order the MM engine to perform a time step.

Keep in mind that the driver doesn't know anything about the simulated system beyond what it can query by sending MDI commands.
MDI engines initialize basic information about the system using whatever input file format the engine's developer chose.
When the MM and QM engines are launched, they will each read their standard input files, perform basic initialization tasks, and then request a connection to the driver.
The driver can request information about the engine's system by sending certain MDI commands.
For example, to determine the number of atoms in the MM engine's system, you could do:

:::{tab-set-code}
```c++
  // Receive the number of atoms from the MM engine
  int natoms;
  MDI_Send_command("<NATOMS", mm_comm);
  MDI_Recv(&natoms, 1, MDI_INT, mm_comm);
\endcode

Similarly, to learn the coordinates of the atoms in the MM engine's system, you could do:

\code
  // Receive the coordinates from the MM engine
  double coords[3*natoms];
  MDI_Send_command("<COORDS", mm_comm);
  MDI_Recv(&coords, 3*natoms, MDI_DOUBLE, mm_comm);
```
:::

You could then update the coordinates of the QM engine's system:

:::{tab-set-code}
```c++
  // Send the coordinates to the QM engine
  MDI_Send_command(">COORDS", qm_comm);
  MDI_Send(&coords, 3*natoms, MDI_DOUBLE, qm_comm);
```
:::

:::{admonition} Prerequisites
As we will discuss later, this driver assumes that the engines have both been initialized with the same number of atoms, the same atom types and ordering of atom types, and the same cell dimensions.
:::

The following code will handle all of the work associated with driving an AIMD simulation
Replace the comment that reads `// Perform the simulation` with:

:::{tab-set-code}
```c++
  // Perform the simulation
  int niterations = 10;  // Number of MD iterations
  int natoms;
  double qm_energy;
  double mm_energy;
 
  // Receive the number of atoms from the MM engine
  MDI_Send_command("<NATOMS", mm_comm);
  MDI_Recv(&natoms, 1, MDI_INT, mm_comm);
 
  // Allocate the arrays for the coordinates and forces
  double coords[3*natoms];
  double forces[3*natoms];

  // Have the MM engine initialize a new MD simulation
  MDI_Send_command("@INIT_MD", mm_comm);
 
  // Perform each iteration of the simulation
  for (int iiteration = 0; iiteration < niterations; iiteration++) {

    // Receive the coordinates from the MM engine
    MDI_Send_command("<COORDS", mm_comm);
    MDI_Recv(&coords, 3*natoms, MDI_DOUBLE, mm_comm);
 
    // Send the coordinates to the QM engine
    MDI_Send_command(">COORDS", qm_comm);
    MDI_Send(&coords, 3*natoms, MDI_DOUBLE, qm_comm);
 
    // Have the MM engine proceed to the @FORCES node
    MDI_Send_command("@FORCES", mm_comm);
 
    // Get the QM energy
    MDI_Send_command("<ENERGY", qm_comm);
    MDI_Recv(&qm_energy, 1, MDI_DOUBLE, qm_comm);
 
    // Get the MM energy
    MDI_Send_command("<ENERGY", mm_comm);
    MDI_Recv(&mm_energy, 1, MDI_DOUBLE, mm_comm);
 
    // Receive the forces from the QM engine
    MDI_Send_command("<FORCES", qm_comm);
    MDI_Recv(&forces, 3*natoms, MDI_DOUBLE, qm_comm);
 
    // Send the forces to the MM engine
    MDI_Send_command(">FORCES", mm_comm);
    MDI_Send(&forces, 3*natoms, MDI_DOUBLE, mm_comm);
 
    // Have the MM engine proceed to the @COORDS node, which completes the timestep
    MDI_Send_command("@COORDS", mm_comm);
 
    cout << "timestep: " << iiteration << " " << mm_energy << " " << qm_energy << endl;
  }
```
:::

The above code does the following:
* Queries the number of atoms from the MM engine and allocates appropriately sized arrays for the coordinates and forces
* Orders the MM engine to initialize a new MD simulation
* Begins an iterative loop over MD iterations
  * Receives the atomic coordinates from the MM engine and updates the QM engine with them
  * Receives the energy of the QM and MM systems
  * Receives the atomic forces from the QM engine and sends them to the MM engine
  * Orders the MM engine update the atomic coordinates

Finally, we should send an MDI command to cause the engines to exit.
Replace the comment that reads `// Send the "EXIT" command to each of the engines` with the following:

:::{tab-set-code}
```c++
  // Send the "EXIT" command to each of the engines
  MDI_Send_command("EXIT", mm_comm);
  MDI_Send_command("EXIT", qm_comm);
```
:::

## Driver Compilation

The cookiecutter came with everything you need to build with CMake.
To compile, simply navigate to the directory where you want the driver to be built, then do the following, replacing `<driver_top_directory>` with the path to top directory of your driver repository.

:::{tab-set-code}
```c++
cmake <driver_top_directory>
make
```
:::


## Using the Driver

To test the driver, you will need access to a QM engine and an MM engine.
The user tutorial describes how to compile QE and LAMMPS for this purpose.

You will also need appropriate input files for a test simulation.
The test files in the `MDI_AIMD_Driver` repository (see the user tutorial) will work fine, as will the scripts.
You can simply edit `MDI_AIMD_Driver/tests/locations/MDI_AIMD_Driver` to point to your new repository and run the scripts in the manner described in the user tutorial.

## Final Notes

Note that although we used QE as the QM code and LAMMPS as the MM code, we swap out QE for any QM engine with MDI support or LAMMPS for any MM engine with MDI support.
Furthermore, nothing about our AIMD driver strictly requires that the code named "QM" actually corresponds to a quantum mechanics code.
You could, for example, use LAMMPS as the QM code, while another instance of LAMMPS serves as the MM code.
Doing so would allow you to run  a calculation that will produce to same results as simply running an MD simulation entirely within a single instance of LAMMPS.
Although not generally useful for production runs, this can be a good way to benchmark the cost of the computational overhead introduced by using our driver as a middleman between two codes.

In other cases, it may be desirable to use two different MM codes as the engines if, for example, you wish to use a force field from one MM code and a thermostat from another MM code.
The only requirement on the engines is that the "QM" code supports all of the MDI commands sent by the AIMD driver to the "QM" engine, and that the "MM" code supports all of the MDI commands sent by the AIMD driver to the "MM" engine.

