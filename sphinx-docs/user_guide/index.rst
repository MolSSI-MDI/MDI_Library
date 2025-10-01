User Guide
-----------

MDI uses a driver/engine paradigm in which drivers orchestrate complex simulations by controlling engines through the use of an API-like command set that is defined by the MDI Standard. 
A driver will typically implement one or more high-level methods, such as advanced sampling or QM/MM, while relying on one or more engines to perform lower-level operations, such as energy and force evaluation.

In total, MDI consists of the following components:

* **Engines**, which are codes capable of responding to commands from an external driver.
* **Drivers**, which are codes that control the high-level program flow of one or more other codes.
* **The MDI Standard**, which is an API-like definition of a set of commands that can be sent from a driver to an engine, and that cause the engine to respond in a clearly defined way.
* **The MDI Library**, which is a library that enables inter-code communication in compliance with the MDI Standard.

Examples of MDI Engines are specially compiled versions of LAMMPS, Quantum ESPRESSO, Psi4, and many others.
You can see a list of available engines in the :doc:`MDI Ecosystem <mdi_ecosystem/index>`.

This User Guide is intended to provide an overview of the MDI Standard and the MDI Library.
The User Guide also provides information on modes of running MDI-enabled calculations.

.. toctree::
   :maxdepth: 1
   :hidden:

   installation
   mdi_standard
   mdi_ecosystem/index
