# Technical Details of the Plugin System

This page describes certain technical nuances of the plugin system that are not essential for typical developers who simply wish to use the MDI plugin system to interoperate codes using MDI, but may be useful for individuals who wish to understand the MDI implementation details more closely.

## Plugin FLow of Control

As described in the [Opening Plugins](opening_plugins) and [Launching Plugins](launching_plugins) pages, the MDI Plugin System largely abstracts the differences between plugins and external engines.
This feature is highly advantageous from the perspective of developers of MDI-enabled codes, who only need to learn a single API for both the inter-process and intra-process paradigms, and can often reuse large amounts of code when supporting both use cases.
Underneath the MDI Library's layer of abstraction, the two use cases are extremely different, especially in the case of "launched" plugins.
In order to make plugins interact via MDI in a way that superficially appears similar to engines, the MDI Library must engage in careful and sophisticated management of the flow of control between the driver and the plugin.

The basic sequence of control flow when a driver launches a plugin is as follows:

1. The driver calls `MDI_Launch_plugin`, which locates and opens the plugin, and then calls the plugin's `MDI_Plugin_launch_<plugin_name>` function.

2. The `MDI_Plugin_launch_<plugin_name>` function registers the `execute_command` callback function with the MDI Library, and then enters the `@DEFAULT` node, in which it will call the `MDI_Recv_command` function.

3. The `MDI_Recv_command` function calls the `driver_callback_func`.

4. When the `driver_callback_func` calls `MDI_Send_command`, the MDI Library executes the plugin's `execute_command` callback function.
In the case of commands that require the driver to send information to the engine before the engine is able to respond (i.e., `>FORCES`, or other MDI commands that begin with a `>` sign), the MDI Library does not call the `execute_command` callback function until the driver sends the required information with a call to `MDI_Send`.
Because the `driver_callback_func` and `execute_command` functions are executed on the same set of ranks, "sending" and "receiving" data only involves simple memory operations.
Data passed to `MDI_Send` calls is temporarily copied to buffers maintained by the MDI Library until it is retrieved by a corresponding `MDI_Recv` call.
In the case of commands that cause the plugin to leave its current node (such as `@INIT_MD`, `@`, and `EXIT`), the MDI Library does not call `execute_command`, but only stores the command in memory.

5. Before the `driver_callback_func` function returns, it is required to send a command via `MDI_Send_command` that instructs the plugin to leave its current node (such as `@INIT_MD`, `@`, and `EXIT`).
After `driver_callback_func` returns, the `MDI_Recv_command` function returns this command to the plugin.

6. The plugin responds to the command returned by the `MDI_Recv_command` function.
This will cause it to either continue execution until it enters another node, or to exit if it receives the `EXIT` command.
The plugin's `MDI_Plugin_launch_<plugin_name>` function only returns after the `EXIT` command is received, or when the plugin encounters an error.

7. At this point the call to `MDI_Launch_plugin` returns, and the ranks that called it are available to call `MDI_Launch_plugin` again, if desired.

The above process enables intra-process implementations to use the same MDI Library function calls that are employed for inter-process calculations, while also taking full advantage of the granular level of control afforded by the MDI Node System.
Developers of MDI-enabled codes do not need to be cognizant of the underlying complexities of the control flow; the MDI Library handles this automatically.

One restriction associated with the callback system described above is that any given MPI rank can only launch a single MDI plugin at a time.
There is no restriction on the number of plugins that can be simultaneously launched on different ranks, or the number of plugins that a particular rank can launch sequentially, but the driver's callback function is not permitted to call `MDI_Launch_plugin` in a nested fashion.
By contrast, "opened" plugins follow a much simpler flow of control, and there is no restriction upon the number of plugins that can be simultaneously opened on a single rank.
It is also possible to launch a single plugin and open one or more plugins on the same set of ranks simultaneously; whereas a `driver_callback_func` cannot call `MDI_Launch_plugin`, it *can* call `MDI_Open_plugin` or interact with previously opened plugins.



## Enabling Independent Compilation of MDI-Supporting Codes

The MDI Plugin System is unusual among existing plugin frameworks, in that it is designed to enable interoperability between existing codes that were not necessarily planned with the intention of operating with the context of a plugin framework.
The MDI Library performs numerous tasks to make this interoperability practical, including transforming data structures when necessitated by different language conventions, providing consistent unit conversions, handling the dynamic loading of shared libraries, enforcing a standardized API for interaction between the codes, *etc*.
For these reasons, both the driver and any plugins must link against the MDI Library.

One of the primary design goals of the MDI Plugin System is that it should be possible to compile the driver and any plugins independently; this feature is important for ensuring that plugins can be swapped in and out at runtime.
Unfortunately, independent compilation of drivers and plugins introduces the possibility that some of the compiled codes might be linked against different, independently compiled builds of the MDI Library.
These MDI Library builds might correspond to different versions of the MDI Library, or they might be compiled with different configuration options.

As a general rule, when a code opens a shared library, and the code and the library link against independently compiled builds of a common dependency, there is a potential for undesirable and unpredictable behavior associated with resolution of any symbols declared within the common dependency.
The exact outcome will depend sensitively upon the specific manner in which the two builds of the dependency were compiled; in some cases, the global state associated with each build of the dependency will be stored separately in memory (with each build having an independent set of global variables), whereas in other cases, the global state associated with the two builds may correspond to the same locations in memory (and thus be shared between the two builds).
More nuanced outcomes are also possible.
Furthermore, if the two builds of the common dependency correspond to different version numbers of the dependency, additional problems can arise due to inconsistencies in the application binary interface (ABI) of the two versions.


This sort of inconsistent and unportable behavior is unacceptable within the context of the MDI Library.
To address this challenge, the MDI Library maintains tight control over all data it manages.
*All* persistent data managed by the MDI Library is contained within a single pointer to a global data structure, which is allocated by the driver.
Upon opening or launching a plugin, this pointer is passed to the plugin, so that both the driver's build of the MDI Library and the plugin's build of the MDI Library are forced to use the same top-level global data structure.
In order to future-proof against the introduction of ABI incompatibilities in new versions of the MDI Library, this top-level global data structure is little more than a collection of pointers, with each one pointing to the MDI-managed data associated with a particular code base (*i.e.*, the driver or a plugin).
The MDI Library maintains the data associated with each of these code bases separately, greatly reducing the potential for ABI incompatibilities in cases where the driver and plugin link against different MDI Library versions.
For cases in which data must be shared between potentially different builds of the MDI Library (such as when the driver sends information to a plugin), the data is stored within a data structure that is intended to remain stable across MDI Library versions.

The above strategy essentially partitions the MDI Library's global memory into separate allocations that are associated with each instance of a driver/plugin that is linked against the MDI Library, with each allocation potentially corresponding to a different MDI version; however, this is not *per se* sufficient to future-proof against changes in the MDI Library.
It is also necessary to ensure that the correct version of each MDI Library function operates on the correct version of the MDI Library data.
Additional safeguards are thus required to enforce consistency in the resolution of the MDI Library's function symbols; this is accomplished through the careful storage and sharing of function pointers.
When the driver initializes the MDI Library, the driver's build of the MDI Library stores function pointers to certain MDI Library functions.
When a plugin is opened or launched, the plugin's build of the MDI Library stores function pointers to these same library functions, and communicates them with the driver.
If the functions defined by the two MDI Library builds resolve to different memory locations, the function pointers allow each build to explicitly call the functions defined by the other build.
This enables the driver's build of the MDI Library to pass control to the plugin's build of the MDI Library, and *vice versa*.

These safeguards represent an extensive and sophisticated effort to enable independent compilation of MDI-supporting codes in a way that is robust with respect to typical compile-time choices.
Although it is impossible to guarantee consistency under all possible circumstances (for example, situations in which different builds of the MDI Library are compiled for different machine architectures), the MDI Library is capable of self-correcting numerous issues associated with inconsistencies in symbol resolution, which represent one of the most troublesome complications associated with the usage of shared libraries.












