# Opening Plugins

"Opening" plugins is a simple process for developers of MDI drivers, and is accomplished by calling an `MDI_Open_plugin` function.
This function has slightly different forms depending on the language (i.e., C/C++, Fortran, or Python), but its arguments always include (1) the name of the desired plugin, (2) any plugin-specific options needed to configure the plugin (for example, a path to an input file that is read as part of the plugin's initialization process), and (3) an MPI intra-communicator that spans all ranks that the plugin's functions should be executed on.
Depending on the needs of the driver, the plugin can be configured to execute on all of the driver's ranks, or on a particular subset of ranks.
The `MDI_Open_plugin` function provides the driver with an MDI communicator upon return, which can be used to interact with the plugin using the same set of MDI Library functions that enable communication with external engines.

For example, the driver can use the `MDI_Send_command` function to "send" commands to the plugin, while using the `MDI_Send` and `MDI_Recv` functions to "send" and "receive" data, respectively.
Internally, the MDI Library simply stores data that is "sent" via these commands within temporary buffers.
When a code calls `MDI_Recv`, the MDI Library copies the stored data into a receive buffer supplied by the code.
MDI adopts this strategy in order to establish a standardized interface that allows drivers to interact with engines in a consistent manner, regardless of whether they are operating within an inter-process or intra-process interoperability regime.
In the case of MPI-parallelized codes, data sent and received via MDI communication functions is only communicated on a single MPI rank; MDI does not currently enable distributed communication between drivers and engines.
This is true for both the inter-process and intra-process regimes, and may require that parallelized codes perform MPI gather or scatter operations on data that is sent or received, respectively, via MDI.

Plugins are required to define a standardized `execute_command` function that must accept the name of a command as an argument, and then respond to the command in the manner described by the MDI Standard.
This is the same behavior that the MDI Standard requires of external engines whenever they receive a command via `MDI_Recv_command`, and it is generally possible to utilize the same code to support both the plugin and engine use cases; the plugin use case simply requires that this functionality be accessible via a standardized function interface.
The MDI Library calls the `execute_command` function whenever it is necessary for the plugin to respond to a command from the driver.
For example, if the driver sends the command that requests the name of the plugin's current node (i.e., `<@`), the plugin's `execute_command` must respond by calling `MDI_Send` to send the name of the current node to the driver.

Plugins are also required to define an `MDI_Plugin_open_<plugin_name>` function (where `<plugin_name>` is replaced by the name of the plugin).
This function must perform any initialization that is necessary before the plugin's `execute_command` function can be called, and it must register the \lstinline{execute_command} function with the MDI Library.
Finally, the plugin must define an `MDI_Plugin_close_<plugin_name>` function that the MDI Library executes when the plugin is sent the `EXIT` command, and which performs any operations necessary for the plugin to exit cleanly and without memory leaks.
The primary actions required to add plugin support to an existing MDI engine are thus to (1) ensure that the code can be compiled as a shared library, or that it is a Python script, (2) define an `execute_command` function (which may simply involve defining a standard function interface for accessing existing code), (3) define an `MDI_Plugin_open_<plugin_name>` function, and (4) define an `MDI_Plugin_close_<plugin_name>` function.

Multiple plugins can be opened simultaneously using `MDI_Open_plugin`, with each one being given its own MPI intra-communicator to define the ranks it will operate on.
These MPI intra-communicators can span overlapping sets of MPI ranks, if desired; this makes it possible for drivers to implement a wide range of parallelization techniques without any limitations from MDI.
Whenever the driver interacts with a plugin through an MDI function, the function must be called by all of the ranks that span the plugin's MPI intra-communicator; this ensures that the MDI Library is able to call the plugin's `execute_command` function with all the ranks that should execute it.

A key limitation associated with the use of `MDI_Open_plugin` is that plugins it opens can only respond to commands from the `@DEFAULT` node.
This is because the MDI Node System is designed to allow access to low-level simulation operations at a granular level (for example, manipulation of a time integration process at several points within each time integration step).
Access to these operations through any plugin interface would require either (1) careful structuring of the plugin to provide API access to such low-level operations as a primary design goal, or (2) implementation of a system of callback functions.
The process of implementing option (1) within an existing code base can potentially require large-scale refactoring of the code, and is thus not consistent with the goals of the MDI Project, which seeks to simplify the process of achieving interoperability in existing codes.
Option (2) offers a substantial degree of flexibility, and has been utilized to good effect in codes such as LAMMPS and OpenMM; this is also the approach taken by MDI.
The MDI Plugin System provides an approach by which drivers can define a callback function that is executed whenever a plugin enters an MDI node.
This approach is described in the [Launching Plugins](launching_plugins) page.












