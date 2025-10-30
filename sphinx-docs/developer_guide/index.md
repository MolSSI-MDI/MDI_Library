# Developer Guide

The MDI Library supports two main strategies for interoperability:

- **Inter-process interoperability:** In this approach, the driver and the engine(s) are launched as separate processes, with the MDI Library establishing on-the-fly communication between the drivers and engines via either TCP/IP sockets or MPI.

- **Intra-process interoperability:** In this approach, the engine(s) are compiled as MDI plugins, and the driver accesses the functionality of these plugins through a standardized API. This respresents a tighter form of coupling, as the driver and engine code(s) run on the same set of processes.

The pages in this section describe both of these strategies in greater detail, beginning with the inter-process approach.

```{toctree}
:hidden:

writing_driver
plugin_system
opening_plugins
launching_plugins
plugin_nuances

```
