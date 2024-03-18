(standard_nodes_sec)=
##  Nodes

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

A specific MD implementation might progress from a `@PRE-FORCES` node, to the `@FORCES` node, to the `@COORDS` node, and then repeat the cycle; however, this behavior is **not guaranteed**.
Engines are permitted to pass through nodes in whatever order the implementation dictates.
When writing drivers, it is a best practice to avoid assumptions about the ordering or frequency of nodes.

The MDI Library provides several functions that allow codes to define and communicate information about which nodes are supported:

  - `MDI_Register_Node()`
  - `MDI_Check_Node_Exists()`
  - `MDI_Get_NNodes()`
  - `MDI_Get_Node()`

Engines are not required to support every MDI command at every node.
The MDI Library provides several functions that allow codes to define and communicate information about which commands are supported at specific nodes:

  - `MDI_Register_Command()`
  - `MDI_Check_Command_Exists()`
  - `MDI_Get_NCommands()`
  - `MDI_Get_Command()`

