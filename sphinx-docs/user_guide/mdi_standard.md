(mdi_standard)=
# MDI Standard

The MDI Standard provides a straightforward, API-like method for enabling interoperability among computational molecular sciences codes.
It uses a driver/engine model in which a driver code controls the high-level program flow of one or more engine codes.
The driver exercises this control through the use of "commands" that are defined by the MDI Standard.
The MDI Standard is made up of a set of commands that can be used to control the program flow of an engine code.
Commands are available that correspond to a variety of tasks, such as "receive a new set of nuclear coordinates from me" (`send_coords`), "start an MD simulation" (`init_md`) and "send me the forces on the nuclei" (`recv_forces`).
The MDI standard defines the standard_units "units", data types, and formatting of any data communicated between codes in response to a command.

Components of the MDI Standard are summarized below.


## Commands
A comprehensive list of commands forms the core of the MDI Standard, enabling detailed control over the simulation process. 
Commands are designed to cover various aspects of simulations, such as node management, data exchange (e.g., coordinates, forces, cell parameters), and control of simulation parameters. 
This structure ensures that drivers can precisely control engine codes, facilitating complex multi-code simulations.

You can see a full list of commands in the [API Documentation](../api/mdi_standard/index).

## Nodes
One of the powerful features of the MDI Standard is that it permits drivers to take advantage of existing implementations of time integrators and geometry optimizers in MDI engines.
In the MDI Standard, you can have your driver intercept the execution of an engine at specific points in the simulation, called "nodes," and issue new commands to the engine.
In particular, the `init_md` and `init_optg` commands cause an engine to begin a molecular dynamics trajectory or a geometry optimization, respectively.
Upon receiving one of these commands, an engine will perform the corresponding simulation without requiring further instruction from the driver, except that it will pause at certain "nodes" and listen for new commands.

The MDI Standard defines several nodes.
Their names and when they occur are as follows:

* **@DEFAULT** - Upon initially connecting to the driver. 
* **@INIT_MC** - Upon initializing a new Monte Carlo simulation. 
* **@INIT_MD** - Upon initializing a new molecular dynamics simulation. 
* **@INIT_OPTG** - Upon initializing a new geometry optimization. 

Not all MDI engines are required to support each of these nodes, 
with the exception of the @DEFAULT node, which every MDI engine must support.
Additional nodes may be defined by individual codes.
Examples of possible code-specific nodes include:

* **@PRE-FORCES** - After calculating all contributions to the atomic forces, **except those associated with a constraint algorithm like SHAKE or RATTLE. 
* **@FORCES** - After calculating all contributions to the atomic forces. 
* **@COORDS** - After updating the atomic coordinates.

Several MDI commands enable a driver to control the program flow of an engine through different nodes.
The `send_node` command instructs the engine to send the name of its node,
while the `next_node` command instructs the engine to proceed in its simulation until it reaches the next node.
In addition, there are commands associated with each type of node (pre-forces_node, forces_node, coords_node) which instruct the engine to proceed in its simulation until it reaches the next node of that particular type.

## Units
To maintain consistency and prevent numerical instabilities, all physical quantities communicated via MDI must be expressed in atomic units. 
The `MDI_Conversion_Factor()` function is provided for necessary unit conversions, ensuring self-consistency across different codes and enhancing the standard's usability in a variety of computational scenarios.

## Constants
The MDI Standard defines a set of constants accessible through the MDI Library, including identifiers for communication methods, roles within the MDI framework, data types, and maximum lengths for code and node names. These constants provide a foundation for consistent and efficient communication between MDI-compliant codes.

(standard_extension)=
## Extending the MDI Standard

Although the MDI Standard has been designed to enable a wide range of potential applications, it is inevitable that some use cases will require functionality that is not yet supported by the standard.
Typically, supporting such use cases will simply require addition of one or more new command_list "commands" to the MDI Standard.
Users who find themselves in need of a new MDI command are encouraged to create an <a href="https://github.com/MolSSI-MDI/MDI_Library/issues">issue</a> describing their needs or submit a <a href="https://github.com/MolSSI-MDI/MDI_Library/pulls">pull request</a> suggesting a solution.

```{toctree}
:hidden:

constants
nodes
commands

```


