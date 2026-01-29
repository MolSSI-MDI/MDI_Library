//! Raw FFI bindings to the MolSSI Driver Interface (MDI) library.
//!
//! This crate provides unsafe, low-level bindings to the MDI C library.
//! For a safe, idiomatic Rust interface, use the `mdi` crate instead.
//!
//! # Example
//!
//! ```ignore
//! use mdi_sys::*;
//! use std::ffi::CString;
//!
//! unsafe {
//!     let options = CString::new("-role DRIVER -name driver -method TCP -port 8021").unwrap();
//!     MDI_Init_with_options(options.as_ptr());
//! }
//! ```

#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

use libc::{c_char, c_double, c_int, c_void};

// =============================================================================
// Type Definitions
// =============================================================================

/// MDI communicator handle
pub type MDI_Comm = c_int;

/// MDI datatype handle
pub type MDI_Datatype = c_int;

/// Callback type for driver node callbacks
pub type MDI_Driver_node_callback_t = Option<extern "C" fn(*mut c_void, c_int, *mut c_void) -> c_int>;

/// Callback type for Fortran driver node callbacks
pub type MDI_Driver_node_callback_f90_t = Option<extern "C" fn(*mut c_void, c_int, *mut c_void) -> c_int>;

/// Callback type for execute command
pub type MDI_Execute_command_callback_t = Option<extern "C" fn(*mut c_void, MDI_Comm, *mut c_void) -> c_int>;

// =============================================================================
// Constants
// =============================================================================

// Version information
pub const MDI_MAJOR_VERSION: c_int = 1;
pub const MDI_MINOR_VERSION: c_int = 4;
pub const MDI_PATCH_VERSION: c_int = 36;

// String length constants
pub const MDI_COMMAND_LENGTH: c_int = 256;
pub const MDI_NAME_LENGTH: c_int = 256;
pub const MDI_LABEL_LENGTH: c_int = 64;

// Null communicator
pub const MDI_COMM_NULL: MDI_Comm = 0;

// Data types
pub const MDI_INT: MDI_Datatype = 1;
pub const MDI_INT8_T: MDI_Datatype = 7;
pub const MDI_INT16_T: MDI_Datatype = 8;
pub const MDI_INT32_T: MDI_Datatype = 9;
pub const MDI_INT64_T: MDI_Datatype = 10;
pub const MDI_UINT8_T: MDI_Datatype = 11;
pub const MDI_UINT16_T: MDI_Datatype = 12;
pub const MDI_UINT32_T: MDI_Datatype = 13;
pub const MDI_UINT64_T: MDI_Datatype = 14;
pub const MDI_DOUBLE: MDI_Datatype = 2;
pub const MDI_CHAR: MDI_Datatype = 3;
pub const MDI_FLOAT: MDI_Datatype = 4;
pub const MDI_BYTE: MDI_Datatype = 6;

// Communication methods
pub const MDI_TCP: c_int = 1;
pub const MDI_MPI: c_int = 2;
pub const MDI_LINK: c_int = 3;
pub const MDI_TEST: c_int = 4;

// Role types
pub const MDI_DRIVER: c_int = 1;
pub const MDI_ENGINE: c_int = 2;

// Language types
pub const MDI_LANGUAGE_C: c_int = 1;
pub const MDI_LANGUAGE_FORTRAN: c_int = 2;
pub const MDI_LANGUAGE_PYTHON: c_int = 3;

// =============================================================================
// External Functions
// =============================================================================

extern "C" {
    // =========================================================================
    // Initialization Functions
    // =========================================================================

    /// Initialize MDI with command-line arguments
    ///
    /// # Arguments
    /// * `argc` - Pointer to argument count
    /// * `argv` - Pointer to argument vector
    ///
    /// # Returns
    /// 0 on success, non-zero on error
    pub fn MDI_Init(argc: *mut c_int, argv: *mut *mut *mut c_char) -> c_int;

    /// Initialize MDI with an options string
    ///
    /// # Arguments
    /// * `options` - Options string (e.g., "-role DRIVER -name driver -method TCP")
    ///
    /// # Returns
    /// 0 on success, non-zero on error
    pub fn MDI_Init_with_options(options: *const c_char) -> c_int;

    /// Initialize MDI with command-line arguments (alternative)
    pub fn MDI_Init_with_argv(argc: *mut c_int, argv: *mut *mut *mut c_char) -> c_int;

    /// Initialize a new code instance
    pub fn MDI_Init_code() -> c_int;

    /// Check if MDI has been initialized
    ///
    /// # Arguments
    /// * `flag` - Output: 1 if initialized, 0 otherwise
    pub fn MDI_Initialized(flag: *mut c_int) -> c_int;

    /// Check if a new communicator is available
    ///
    /// # Arguments
    /// * `flag` - Output: 1 if communicator available, 0 otherwise
    pub fn MDI_Check_for_communicator(flag: *mut c_int) -> c_int;

    /// Get the current code ID
    pub fn MDI_Get_Current_Code() -> c_int;

    /// Get the intra-code rank
    pub fn MDI_Get_intra_rank(intra_rank_out: *mut c_int) -> c_int;

    // =========================================================================
    // Communication Functions
    // =========================================================================

    /// Accept a new MDI communicator
    ///
    /// # Arguments
    /// * `comm` - Output: The accepted communicator handle
    pub fn MDI_Accept_Communicator(comm: *mut MDI_Comm) -> c_int;

    /// Accept a new MDI communicator (lowercase variant)
    pub fn MDI_Accept_communicator(comm: *mut MDI_Comm) -> c_int;

    /// Send data through MDI
    ///
    /// # Arguments
    /// * `buf` - Buffer containing data to send
    /// * `count` - Number of elements to send
    /// * `datatype` - Type of data being sent
    /// * `comm` - Communicator to send through
    pub fn MDI_Send(buf: *const c_void, count: c_int, datatype: MDI_Datatype, comm: MDI_Comm) -> c_int;

    /// Receive data through MDI
    ///
    /// # Arguments
    /// * `buf` - Buffer to receive data into
    /// * `count` - Number of elements to receive
    /// * `datatype` - Type of data being received
    /// * `comm` - Communicator to receive from
    pub fn MDI_Recv(buf: *mut c_void, count: c_int, datatype: MDI_Datatype, comm: MDI_Comm) -> c_int;

    /// Send a command string
    ///
    /// # Arguments
    /// * `buf` - Command string to send
    /// * `comm` - Communicator to send through
    pub fn MDI_Send_Command(buf: *const c_char, comm: MDI_Comm) -> c_int;

    /// Send a command string (lowercase variant)
    pub fn MDI_Send_command(buf: *const c_char, comm: MDI_Comm) -> c_int;

    /// Receive a command string
    ///
    /// # Arguments
    /// * `buf` - Buffer to receive command into (must be at least MDI_COMMAND_LENGTH)
    /// * `comm` - Communicator to receive from
    pub fn MDI_Recv_Command(buf: *mut c_char, comm: MDI_Comm) -> c_int;

    /// Receive a command string (lowercase variant)
    pub fn MDI_Recv_command(buf: *mut c_char, comm: MDI_Comm) -> c_int;

    // =========================================================================
    // Role and Method Functions
    // =========================================================================

    /// Get the role of this code (DRIVER or ENGINE)
    ///
    /// # Arguments
    /// * `role` - Output: MDI_DRIVER or MDI_ENGINE
    pub fn MDI_Get_Role(role: *mut c_int) -> c_int;

    /// Get the role (lowercase variant)
    pub fn MDI_Get_role(role: *mut c_int) -> c_int;

    /// Get the communication method for a communicator
    pub fn MDI_Get_method(method: *mut c_int, comm: MDI_Comm) -> c_int;

    /// Get a communicator by index
    pub fn MDI_Get_communicator(comm: *mut MDI_Comm, index: c_int) -> c_int;

    // =========================================================================
    // Node Management Functions
    // =========================================================================

    /// Register a node
    pub fn MDI_Register_Node(node_name: *const c_char) -> c_int;

    /// Register a node (lowercase variant)
    pub fn MDI_Register_node(node_name: *const c_char) -> c_int;

    /// Check if a node exists
    pub fn MDI_Check_Node_Exists(
        node_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Check if a node exists (lowercase variant)
    pub fn MDI_Check_node_exists(
        node_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Get the number of nodes
    pub fn MDI_Get_NNodes(comm: MDI_Comm, nnodes: *mut c_int) -> c_int;

    /// Get the number of nodes (lowercase variant)
    pub fn MDI_Get_nnodes(comm: MDI_Comm, nnodes: *mut c_int) -> c_int;

    /// Get a node name by index
    pub fn MDI_Get_Node(index: c_int, comm: MDI_Comm, name: *mut c_char) -> c_int;

    /// Get a node name by index (lowercase variant)
    pub fn MDI_Get_node(index: c_int, comm: MDI_Comm, name: *mut c_char) -> c_int;

    // =========================================================================
    // Command Management Functions
    // =========================================================================

    /// Register a command for a node
    pub fn MDI_Register_Command(node_name: *const c_char, command_name: *const c_char) -> c_int;

    /// Register a command (lowercase variant)
    pub fn MDI_Register_command(node_name: *const c_char, command_name: *const c_char) -> c_int;

    /// Check if a command exists
    pub fn MDI_Check_Command_Exists(
        node_name: *const c_char,
        command_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Check if a command exists (lowercase variant)
    pub fn MDI_Check_command_exists(
        node_name: *const c_char,
        command_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Get the number of commands for a node
    pub fn MDI_Get_NCommands(
        node_name: *const c_char,
        comm: MDI_Comm,
        ncommands: *mut c_int,
    ) -> c_int;

    /// Get the number of commands (lowercase variant)
    pub fn MDI_Get_ncommands(
        node_name: *const c_char,
        comm: MDI_Comm,
        ncommands: *mut c_int,
    ) -> c_int;

    /// Get a command name by index
    pub fn MDI_Get_Command(
        node_name: *const c_char,
        index: c_int,
        comm: MDI_Comm,
        name: *mut c_char,
    ) -> c_int;

    /// Get a command name (lowercase variant)
    pub fn MDI_Get_command(
        node_name: *const c_char,
        index: c_int,
        comm: MDI_Comm,
        name: *mut c_char,
    ) -> c_int;

    // =========================================================================
    // Callback Management Functions
    // =========================================================================

    /// Register a callback for a node
    pub fn MDI_Register_Callback(node_name: *const c_char, callback_name: *const c_char) -> c_int;

    /// Register a callback (lowercase variant)
    pub fn MDI_Register_callback(node_name: *const c_char, callback_name: *const c_char) -> c_int;

    /// Check if a callback exists
    pub fn MDI_Check_Callback_Exists(
        node_name: *const c_char,
        callback_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Check if a callback exists (lowercase variant)
    pub fn MDI_Check_callback_exists(
        node_name: *const c_char,
        callback_name: *const c_char,
        comm: MDI_Comm,
        flag: *mut c_int,
    ) -> c_int;

    /// Get the number of callbacks for a node
    pub fn MDI_Get_NCallbacks(
        node_name: *const c_char,
        comm: MDI_Comm,
        ncallbacks: *mut c_int,
    ) -> c_int;

    /// Get the number of callbacks (lowercase variant)
    pub fn MDI_Get_ncallbacks(
        node_name: *const c_char,
        comm: MDI_Comm,
        ncallbacks: *mut c_int,
    ) -> c_int;

    /// Get a callback name by index
    pub fn MDI_Get_Callback(
        node_name: *const c_char,
        index: c_int,
        comm: MDI_Comm,
        name: *mut c_char,
    ) -> c_int;

    /// Get a callback name (lowercase variant)
    pub fn MDI_Get_callback(
        node_name: *const c_char,
        index: c_int,
        comm: MDI_Comm,
        name: *mut c_char,
    ) -> c_int;

    // =========================================================================
    // Unit Conversion Functions
    // =========================================================================

    /// Get the conversion factor between units
    ///
    /// # Arguments
    /// * `in_unit` - Input unit name
    /// * `out_unit` - Output unit name
    /// * `conv` - Output: Conversion factor
    pub fn MDI_Conversion_Factor(
        in_unit: *const c_char,
        out_unit: *const c_char,
        conv: *mut c_double,
    ) -> c_int;

    /// Get conversion factor (lowercase variant)
    pub fn MDI_Conversion_factor(
        in_unit: *const c_char,
        out_unit: *const c_char,
        conv: *mut c_double,
    ) -> c_int;

    // =========================================================================
    // Utility Functions
    // =========================================================================

    /// Convert an element symbol to atomic number
    pub fn MDI_String_to_atomic_number(
        element_symbol: *const c_char,
        atomic_number: *mut c_int,
    ) -> c_int;

    // =========================================================================
    // MPI Functions
    // =========================================================================

    /// Get the MPI world communicator
    pub fn MDI_MPI_get_world_comm(world_comm: *mut c_void) -> c_int;

    /// Set the MPI world communicator
    pub fn MDI_MPI_set_world_comm(world_comm: *mut c_void) -> c_int;

    // =========================================================================
    // Plugin Functions
    // =========================================================================

    /// Launch a plugin
    pub fn MDI_Launch_plugin(
        plugin_name: *const c_char,
        options: *const c_char,
        mpi_comm_ptr: *mut c_void,
        driver_node_callback: MDI_Driver_node_callback_t,
        driver_callback_object: *mut c_void,
    ) -> c_int;

    /// Open a plugin (get a communicator to it)
    pub fn MDI_Open_plugin(
        plugin_name: *const c_char,
        options: *const c_char,
        mpi_comm_ptr: *mut c_void,
        mdi_comm_ptr: *mut MDI_Comm,
    ) -> c_int;

    /// Close a plugin
    pub fn MDI_Close_plugin(mdi_comm: MDI_Comm) -> c_int;

    /// Set the execute command function
    pub fn MDI_Set_Execute_Command_Func(
        generic_command: Option<extern "C" fn(*const c_char, MDI_Comm, *mut c_void) -> c_int>,
        class_object: *mut c_void,
    ) -> c_int;

    /// Set the execute command function (lowercase variant)
    pub fn MDI_Set_execute_command_func(
        generic_command: Option<extern "C" fn(*const c_char, MDI_Comm, *mut c_void) -> c_int>,
        class_object: *mut c_void,
    ) -> c_int;

    /// Set the plugin state
    pub fn MDI_Set_plugin_state(state: *mut c_void) -> c_int;

    /// Set the plugin state (internal)
    pub fn MDI_Set_plugin_state_internal(state: *mut c_void) -> c_int;

    /// Get the plugin argument count
    pub fn MDI_Plugin_get_argc(argc_ptr: *mut c_int) -> c_int;

    /// Get the plugin argument vector
    pub fn MDI_Plugin_get_argv(argv_ptr: *mut *mut *mut c_char) -> c_int;

    /// Get plugin arguments as a single string
    pub fn MDI_Plugin_get_args(args_ptr: *mut *mut c_char) -> c_int;

    /// Get a specific plugin argument
    pub fn MDI_Plugin_get_arg(index: c_int, arg_ptr: *mut *mut c_char) -> c_int;

    // =========================================================================
    // MPI4Py Callback Functions
    // =========================================================================

    /// Set MPI4Py receive callback
    pub fn MDI_Set_Mpi4py_Recv_Callback(
        mpi4py_recv: Option<extern "C" fn(*mut c_void, c_int, c_int, c_int, MDI_Comm) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py send callback
    pub fn MDI_Set_Mpi4py_Send_Callback(
        mpi4py_send: Option<extern "C" fn(*mut c_void, c_int, c_int, c_int, MDI_Comm) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py allgather callback
    pub fn MDI_Set_Mpi4py_Allgather_Callback(
        mpi4py_allgather: Option<extern "C" fn(*mut c_void, *mut c_void) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py gather names callback
    pub fn MDI_Set_Mpi4py_Gather_Names_Callback(
        mpi4py_gather_names: Option<extern "C" fn(*mut c_void, *mut c_void, *mut c_int, *mut c_int) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py split callback
    pub fn MDI_Set_Mpi4py_Split_Callback(
        mpi4py_split: Option<extern "C" fn(c_int, c_int, MDI_Comm, c_int) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py rank callback
    pub fn MDI_Set_Mpi4py_Rank_Callback(
        mpi4py_rank: Option<extern "C" fn(c_int) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py size callback
    pub fn MDI_Set_Mpi4py_Size_Callback(
        mpi4py_size: Option<extern "C" fn(c_int) -> c_int>,
    ) -> c_int;

    /// Set MPI4Py barrier callback
    pub fn MDI_Set_Mpi4py_Barrier_Callback(
        mpi4py_barrier: Option<extern "C" fn(c_int) -> c_int>,
    ) -> c_int;

    /// Set launch plugin callback
    pub fn MDI_Set_Launch_Plugin_Callback(
        launch_plugin: Option<extern "C" fn(*mut c_void, *mut c_void, *mut c_void, c_int) -> c_int>,
    ) -> c_int;

    // =========================================================================
    // Internal Functions (used by MDI internally)
    // =========================================================================

    /// Report an error
    pub fn mdi_error(message: *const c_char);

    /// Set world size
    pub fn MDI_Set_World_Size(world_size_in: c_int);

    /// Set world rank
    pub fn MDI_Set_World_Rank(world_rank_in: c_int);

    /// Get Python plugin MPI world pointer
    pub fn MDI_Get_python_plugin_mpi_world_ptr(
        python_plugin_mpi_world_ptr_ptr: *mut *mut c_void,
        state_in: *mut c_void,
    ) -> c_int;

    /// Set callback for code destruction
    pub fn MDI_Set_on_destroy_code(func: Option<extern "C" fn(c_int) -> c_int>) -> c_int;

    /// Set plugin language
    pub fn MDI_Set_plugin_language(language: c_int, plugin_state: *mut c_void) -> c_int;

    /// Set language execute command function
    pub fn MDI_Set_language_execute_command(
        execute_command: Option<extern "C" fn(*mut c_void, MDI_Comm, *mut c_void) -> c_int>,
    ) -> c_int;

    /// Get language execute command function
    pub fn MDI_Get_language_execute_command(
        language_execute_command: *mut MDI_Execute_command_callback_t,
        comm: MDI_Comm,
    ) -> c_int;

    /// Get language driver callback
    pub fn MDI_Get_language_driver_callback(
        language_driver_callback: *mut MDI_Driver_node_callback_f90_t,
    ) -> c_int;

    /// Set language driver callback
    pub fn MDI_Set_language_driver_callback(
        callback: MDI_Driver_node_callback_f90_t,
    ) -> c_int;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants() {
        assert_eq!(MDI_COMMAND_LENGTH, 256);
        assert_eq!(MDI_NAME_LENGTH, 256);
        assert_eq!(MDI_COMM_NULL, 0);
        assert_eq!(MDI_DRIVER, 1);
        assert_eq!(MDI_ENGINE, 2);
    }
}
