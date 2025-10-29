# Launching Plugins

As described in Subsection `mdi_plugin_overview`, it is important for the MDI Plugin System to enable highly granular control of communication between the driver and the plugin.
For example, robust and general implementations of the AIMD, NEB, metadynamics, and QM/MM methods all require that the driver be able to modify and/or query the plugin's forces and nuclear coordinates at one or more points *within a single time integration step*.
The specific points at which this interaction must occur will be dependent upon the time integrator being employed by the plugin; in the case of time integration algorithms that involve multiple force evaluations within a single timestep, it may be necessary for the driver and the plugin to share information at numerous points within each step.

The MDI Plugin System facilitates this through the use of callback functions, which are used when plugins are "launched" via an `MDI_Launch_plugin` function.
When this function is called, the MDI Library first dynamically loads the plugin, in the same manner as the `MDI_Open_plugin` function.
Instead of executing an `MDI_Plugin_open_<plugin_name>` function, `MDI_Launch_plugin` executes a function called `MDI_Plugin_launch_<plugin_name>` that must also be defined by the plugin.
This function causes the plugin to begin executing in much the same manner as an external engine: it must at some point enter a `@DEFAULT` node, in the same manner as any external MDI engine, and it can also optionally enter other nodes at subsequent points in its execution.
In the same manner as an external MDI engine, each node must call `MDI_Recv_command` to receive commands from the driver, and must then respond to each received command in accordance with the MDI Standard.

Meanwhile, the MDI Library executes a callback function that is defined by the driver (called `driver_callback_func`) whenever the "launched" plugin enters a new node.
Within this callback function, the driver can use `MDI_Send_command`, `MDI_Send`, `MDI_Rec`, and other MDI functions to interact with the plugin.
The `MDI_Plugin_launch_<plugin_name>` function (and, by extension, the `MDI_Launch_plugin` function) does not return until the `driver_callback_func` callback instructs the launched plugin to exit by sending the `EXIT` command.

The code below provides an example of a simple Python driver that is designed to use an MDI plugin.
This particular driver is designed to work with a plugin that can run MD simulations, and performs the trivially simple operation of setting all the nuclear forces to zero at each iteration of the MD simulation.
As a result, the MD plugin will run a simulation in which the particles are all non-interacting.
Although the utility of this particular application is minimal, it illustrates the basic operation of the MDI Plugin System, and can straightforwardly be extended to support more relevant MDI drivers.

```python
from mpi4py import MPI
import mdi

# Wrapper function for the driver's callback function
def driver_callback_func(mpi_comm, mdi_comm, driver_callback_object):
    return driver_callback_object.callback(mpi_comm, mdi_comm)

class MDIDriver:
    def __init__(self):
        self.natoms = 0
        self.niterations = 0

    # Callback function that is executed by driver_callback_func
    def callback(self, mpi_comm, mdi_comm):
        # Query the name of the plugin's current node
        mdi.MDI_Send_Command("<@", mdi_comm)
        node = mdi.MDI_Recv(mdi.MDI_COMMAND_LENGTH, mdi.MDI_CHAR, mdi_comm)

        if node == "@DEFAULT":
            # Request the number of atoms in the plugin's system
            mdi.MDI_Send_Command("<NATOMS", mdi_comm)
            self.natoms = mdi.MDI_Recv(1, mdi.MDI_INT, mdi_comm)

            # Send the "@INIT_MD" command to the engine
            mdi.MDI_Send_Command("@INIT_MD", mdi_comm)

        elif node == "@FORCES":
            self.niterations += 1

            # Set the plugin's forces to zero
            forces = [ 0.0 for i in range( 3 * self.natoms ) ]
            mdi.MDI_Send_Command(">FORCES", mdi_comm)
            mdi.MDI_Send(forces, 3 * self.natoms, mdi.MDI_DOUBLE, mdi_comm)

            if self.niterations < 1000:
                # Proceed to the next node in the MD simulation
                mdi.MDI_Send_Command("@", mdi_comm)
            else:
                # End the MD simulation and instruct the plugin to exit
                mdi.MDI_Send_Command("EXIT", mdi_comm)

        else:
            # Proceed to the next node in the MD simulation
            mdi.MDI_Send_Command("@", mdi_comm)

        return 0

# Obtain plugin_name, plugin_options, and mdi_options from user
...

driver_callback_object = MDIDriver()
mpi_comm = MPI.COMM_WORLD

mdi.MDI_Init(mdi_options)
mdi.MDI_Launch_plugin(plugin_name, plugin_options,
    mpi_comm, driver_callback_func, driver_callback_object)
```


In lines 53-54 above, the driver calls the `MDI_Launch_plugin` function.
The first argument (`plugin_name`) indicates the name of the plugin that should be launched; the MDI Library will search for a plugin with this name in a user-specified location.
The second argument (`plugin_options`) provides any options that are required for the initialization process of the specific plugin that is being launched; these are defined by the plugin itself, and will typically be provided by the end-user.
For example, some plugins might require that the end-user specify the location of an input file that will be read by the plugin.
The third argument (`mpi_comm`) is an MPI communicator that spans all the ranks that will execute the plugin's code; all of these ranks will be used to run the plugin in a parallel context.
The fourth argument (`driver_callback_func`) provides a callback function that is defined by the driver and will be executed by the plugin.
In order to ensure cross-language compatibility, this callback function cannot be a class member; however, the driver can also pass a (`driver_callback_object`) to the `MDI_Launch_plugin`) function.
When the plugin calls the `driver_callback_func` function, it will pass the `driver_callback_object` as an argument to that function.
This makes it possible for the `driver_callback_func` to serve as a simple wrapper around a class member of the `driver_callback_object`, if desired.
The `driver_callback_func` in the above code is simply a wrapper function around the `callback` member of the class `MDIDriver`.

The `MDIDriver.callback` function is thus executed every time the plugin enters a node.
The callback accepts as arguments an MPI intra-communicator (`mpi_comm`) that spans all the ranks that are assigned to run the plugin, and an MDI communicator (`mdi_comm`) that can be used in subsequent MDI Library calls.
The function begins by querying the name of the plugin node from which it was called, by sending the `<@` command.
It then takes a specific set of actions depending upon the node.
If the plugin is at the `@DEFAULT` node, the driver queries the number of atoms in the plugin's system, and then sends the command to proceed to the `@INIT_MD` node, which the MDI Standard defines as being a node that engines must enter upon initializing an MD simulation.
This causes the plugin to initialize an MD simulation.
If the plugin is at the `@FORCES` node - which is defined as happening immediately after evaluation of the nuclear forces - the driver simply sets all of the forces computed by the plugin to zero, and then depending on the number of force evaluations the plugin has completed, either uses the `@` command to instruct the plugin to continue to the next node (thus continuing the MD simulation), or commands the plugin to exit.
At all other nodes (including the `@INIT_MD` node), the driver simply commands the plugin to proceed to the next node in its normal program flow, which causes the MD simulation to proceed.

Every time the driver's callback function executes, the last command it sends to the plugin instructs it to leave its current node (as is the case with the `@INIT_MD`, `@`, and `EXIT` commands).
The MDI Plugin System requires this behavior; because the driver's callback function is called whenever the plugin enters a new node, the callback function must end by providing instruction regarding the next node it should seek out.

The code above could be quickly adapted to the inter-process interoperability paradigm.
This would involve acquiring an MDI communicator through a call to the `MDI_Accept_communicator` function instead of `MDI_Launch_plugin`.
After establishing a connection with the engine, the code in `MDIDriver.callback` could be reused for the inter-process use case; it would only be necessary for the driver to repeatedly execute the `MDIDriver.callback` function until the driver sends the `EXIT` command (which in this case, happens when the `MDIDriver.niterations` property is 1000).
The ability to simultaneously support both an inter-process paradigm and an intra-process paradigm is a unique feature that arises as a direct result of the high-level of abstraction the MDI Library enforces over the underlying mechanisms by which drivers and engines/plugins interact.

Moreover, if an existing code has a library interface, but does not include explicit MDI support, it is in some circumstances possible to make it usable as an MDI engine by writing a simple Python wrapper script around the existing library interface.
This script can provide support for the code to be used within both the inter-process and intra-process paradigms, leveraging the previously-mentioned code-reuse of MDI calls for the two paradigms.
LAMMPS currently employs this strategy by including MDI wrappers for three QM codes (LATTE, NWChem, and PySCF) in its distribution.
These codes implement Fortran, C, and Python library interfaces, respectively, but do not currently have native MDI support.
In the future, the MDI development team will explore additional opportunities to broaden the availability of MDI engines through simple wrapper scripts.















