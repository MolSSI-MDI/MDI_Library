# MDI Standard 

The MDI Standard provides a straightforward, API-like method for enabling interoperability among computational molecular sciences codes. It uses a driver/engine model in which a driver code controls the high-level program flow of one or more engine codes. The driver exercises this control through the use of commands that are defined by the MDI Standard. Commands are available that correspond to a variety of tasks, such as "receive a new set of nuclear coordinates from me" (>COORDS), "start an MD simulation" (@INIT_MD) and "send me the forces on the nuclei" (<FORCES). The MDI standard defines the units, data types, and formatting of any data communicated between codes in response to a command.

```{toctree}
:maxdepth: 1

mdi_standard_category
../../../_tags/tagsindex
```
