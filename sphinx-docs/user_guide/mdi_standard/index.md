(mdi_standard_user)=
# MDI Standard

**Notes** move everythng thta is on own page to this page. We don't need multiple sections here.
Command List should be in API Section under MDI Standard.
Add modes of running to user guide.

The MDI Standard provides a straightforward, API-like method for enabling interoperability among computational molecular sciences codes.
It uses a driver/engine model in which a driver code controls the high-level program flow of one or more engine codes.
The driver exercises this control through the use of command_list "commands" that are defined by the MDI Standard.
Commands are available that correspond to a variety of tasks, such as "receive a new set of nuclear coordinates from me" (`send_coords`), "start an MD simulation" (`init_md`) and "send me the forces on the nuclei" (`recv_forces`).
The MDI standard defines the standard_units "units", data types, and formatting of any data communicated between codes in response to a command.

Components of the MDI Standard are summarized below.
See the additional pages in this section for details on each component.

## Nodes
The MDI Standard allows drivers to leverage existing time integrators and geometry optimizers within MDI engines. 
Engines can autonomously execute simulations, pausing at defined "nodes" to listen for new commands. 
These nodes facilitate structured communication and control flow between drivers and engines, enhancing flexibility in simulation management.
Nodes are located within MDI Engines. 
If you are writing an MDI Driver, you will interact will nodes by telling your script which nodes to "listen" at.
You can read more about nodes, including seeing a list of all nodes on the [nodes page](nodes).

## Commands
A comprehensive list of commands forms the core of the MDI Standard, enabling detailed control over the simulation process. 
Commands are designed to cover various aspects of simulations, such as node management, data exchange (e.g., coordinates, forces, cell parameters), and control of simulation parameters. 
This structure ensures that drivers can precisely control engine codes, facilitating complex multi-code simulations.

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


