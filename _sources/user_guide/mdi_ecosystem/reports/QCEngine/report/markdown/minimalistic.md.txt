# Minimalistic MDI Communication Test

This test verifies that the engine is capable of basic communication with an external engine.



## Failure Resolution

If this test fails, ensure that you have followed each of the steps outlined below.




### Step 7: Add Support for Additional Nodes

Whenever you add a new node, you must also add a call to `MDI_Register_Node()` to register support for that node.
Commands are registered separately for each node, so any commands that are supported at the new node must be registered for it.
For example, if you implement five nodes, and each of them supports the `EXIT` command, you will need to call the `MDI_Register_Command()` function five times, each time with a different node as the first argument and with `EXIT` as the second argument.
