//! Safe Rust bindings to the MolSSI Driver Interface (MDI) library.
//!
//! This crate provides a safe, idiomatic Rust interface to the MDI library,
//! which enables communication between computational chemistry codes.
//!
//! # Example: Driver
//!
//! ```ignore
//! use mdi::{Mdi, Role, Method};
//!
//! fn main() -> mdi::Result<()> {
//!     // Initialize MDI as a driver using TCP
//!     Mdi::init_with_options("-role DRIVER -name driver -method TCP -port 8021")?;
//!
//!     // Accept a connection from an engine
//!     let comm = Mdi::accept_communicator()?;
//!
//!     // Send a command
//!     comm.send_command("<ENERGY")?;
//!
//!     // Receive the energy
//!     let energy: f64 = comm.recv_scalar()?;
//!     println!("Energy: {}", energy);
//!
//!     Ok(())
//! }
//! ```
//!
//! # Example: Engine
//!
//! ```ignore
//! use mdi::{Mdi, Role};
//!
//! fn main() -> mdi::Result<()> {
//!     // Initialize MDI as an engine
//!     Mdi::init_with_options("-role ENGINE -name engine -method TCP -hostname localhost -port 8021")?;
//!
//!     // Register supported nodes and commands
//!     Mdi::register_node("@DEFAULT")?;
//!     Mdi::register_command("@DEFAULT", "<ENERGY")?;
//!
//!     // Accept connection from driver
//!     let comm = Mdi::accept_communicator()?;
//!
//!     // Receive and process commands
//!     loop {
//!         let command = comm.recv_command()?;
//!         match command.as_str() {
//!             "<ENERGY" => {
//!                 let energy = 42.0f64;
//!                 comm.send_scalar(energy)?;
//!             }
//!             "EXIT" => break,
//!             _ => {}
//!         }
//!     }
//!
//!     Ok(())
//! }
//! ```

mod comm;
mod datatype;
mod error;

pub use comm::Communicator;
pub use datatype::{DataType, MdiData};
pub use error::{Error, Result};

use mdi_sys::*;
use std::ffi::CString;

/// MDI role: either DRIVER or ENGINE
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    /// Driver code that controls the execution
    Driver,
    /// Engine code that performs calculations
    Engine,
}

impl From<i32> for Role {
    fn from(value: i32) -> Self {
        match value {
            x if x == MDI_DRIVER => Role::Driver,
            x if x == MDI_ENGINE => Role::Engine,
            _ => panic!("Invalid MDI role: {}", value),
        }
    }
}

impl From<Role> for i32 {
    fn from(role: Role) -> i32 {
        match role {
            Role::Driver => MDI_DRIVER,
            Role::Engine => MDI_ENGINE,
        }
    }
}

/// MDI communication method
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Method {
    /// TCP/IP socket communication
    Tcp,
    /// MPI-based communication
    Mpi,
    /// Direct library linking
    Link,
    /// Test mode
    Test,
}

impl From<i32> for Method {
    fn from(value: i32) -> Self {
        match value {
            x if x == MDI_TCP => Method::Tcp,
            x if x == MDI_MPI => Method::Mpi,
            x if x == MDI_LINK => Method::Link,
            x if x == MDI_TEST => Method::Test,
            _ => panic!("Invalid MDI method: {}", value),
        }
    }
}

impl From<Method> for i32 {
    fn from(method: Method) -> i32 {
        match method {
            Method::Tcp => MDI_TCP,
            Method::Mpi => MDI_MPI,
            Method::Link => MDI_LINK,
            Method::Test => MDI_TEST,
        }
    }
}

/// Main MDI interface
pub struct Mdi;

impl Mdi {
    /// Initialize MDI with an options string.
    ///
    /// # Arguments
    /// * `options` - Options string containing role, name, method, etc.
    ///
    /// # Example options strings:
    /// * Driver: `"-role DRIVER -name driver -method TCP -port 8021"`
    /// * Engine: `"-role ENGINE -name engine -method TCP -hostname localhost -port 8021"`
    pub fn init_with_options(options: &str) -> Result<()> {
        let c_options = CString::new(options).map_err(|_| Error::InvalidString)?;
        let ret = unsafe { MDI_Init_with_options(c_options.as_ptr()) };
        if ret != 0 {
            return Err(Error::InitializationFailed);
        }
        Ok(())
    }

    /// Check if MDI has been initialized
    pub fn is_initialized() -> Result<bool> {
        let mut flag: i32 = 0;
        let ret = unsafe { MDI_Initialized(&mut flag) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(flag != 0)
    }

    /// Get the role of this code (Driver or Engine)
    pub fn get_role() -> Result<Role> {
        let mut role: i32 = 0;
        let ret = unsafe { MDI_Get_role(&mut role) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(Role::from(role))
    }

    /// Accept a new communicator from a connecting code
    pub fn accept_communicator() -> Result<Communicator> {
        let mut comm: MDI_Comm = MDI_COMM_NULL;
        let ret = unsafe { MDI_Accept_communicator(&mut comm) };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(Communicator::new(comm))
    }

    /// Check if a new communicator is available (non-blocking)
    pub fn check_for_communicator() -> Result<bool> {
        let mut flag: i32 = 0;
        let ret = unsafe { MDI_Check_for_communicator(&mut flag) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(flag != 0)
    }

    /// Get a communicator by index
    pub fn get_communicator(index: i32) -> Result<Communicator> {
        let mut comm: MDI_Comm = MDI_COMM_NULL;
        let ret = unsafe { MDI_Get_communicator(&mut comm, index) };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(Communicator::new(comm))
    }

    /// Register a node for this engine
    pub fn register_node(node_name: &str) -> Result<()> {
        let c_name = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let ret = unsafe { MDI_Register_node(c_name.as_ptr()) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(())
    }

    /// Register a command for a node
    pub fn register_command(node_name: &str, command_name: &str) -> Result<()> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let c_cmd = CString::new(command_name).map_err(|_| Error::InvalidString)?;
        let ret = unsafe { MDI_Register_command(c_node.as_ptr(), c_cmd.as_ptr()) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(())
    }

    /// Register a callback for a node
    pub fn register_callback(node_name: &str, callback_name: &str) -> Result<()> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let c_cb = CString::new(callback_name).map_err(|_| Error::InvalidString)?;
        let ret = unsafe { MDI_Register_callback(c_node.as_ptr(), c_cb.as_ptr()) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(())
    }

    /// Get the conversion factor between two units
    ///
    /// # Example
    /// ```ignore
    /// let factor = Mdi::conversion_factor("angstrom", "bohr")?;
    /// ```
    pub fn conversion_factor(from_unit: &str, to_unit: &str) -> Result<f64> {
        let c_from = CString::new(from_unit).map_err(|_| Error::InvalidString)?;
        let c_to = CString::new(to_unit).map_err(|_| Error::InvalidString)?;
        let mut conv: f64 = 0.0;
        let ret = unsafe { MDI_Conversion_factor(c_from.as_ptr(), c_to.as_ptr(), &mut conv) };
        if ret != 0 {
            return Err(Error::ConversionFailed);
        }
        Ok(conv)
    }

    /// Convert an element symbol to its atomic number
    ///
    /// # Example
    /// ```ignore
    /// let z = Mdi::element_to_atomic_number("C")?;  // Returns 6
    /// ```
    pub fn element_to_atomic_number(symbol: &str) -> Result<i32> {
        let c_symbol = CString::new(symbol).map_err(|_| Error::InvalidString)?;
        let mut atomic_number: i32 = 0;
        let ret = unsafe { MDI_String_to_atomic_number(c_symbol.as_ptr(), &mut atomic_number) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(atomic_number)
    }

    /// Get the intra-code MPI rank
    pub fn get_intra_rank() -> Result<i32> {
        let mut rank: i32 = 0;
        let ret = unsafe { MDI_Get_intra_rank(&mut rank) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(rank)
    }
}

/// Version information for the MDI library
pub mod version {
    use mdi_sys::{MDI_MAJOR_VERSION, MDI_MINOR_VERSION, MDI_PATCH_VERSION};

    /// Major version number
    pub const MAJOR: i32 = MDI_MAJOR_VERSION;
    /// Minor version number
    pub const MINOR: i32 = MDI_MINOR_VERSION;
    /// Patch version number
    pub const PATCH: i32 = MDI_PATCH_VERSION;

    /// Get the version as a string
    pub fn as_string() -> String {
        format!("{}.{}.{}", MAJOR, MINOR, PATCH)
    }
}

/// Constants for MDI string lengths
pub mod constants {
    use mdi_sys::{MDI_COMMAND_LENGTH, MDI_LABEL_LENGTH, MDI_NAME_LENGTH};

    /// Maximum length of an MDI command
    pub const COMMAND_LENGTH: usize = MDI_COMMAND_LENGTH as usize;
    /// Maximum length of an MDI name
    pub const NAME_LENGTH: usize = MDI_NAME_LENGTH as usize;
    /// Maximum length of an MDI label
    pub const LABEL_LENGTH: usize = MDI_LABEL_LENGTH as usize;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(version::MAJOR, 1);
        assert!(version::MINOR >= 4);
    }

    #[test]
    fn test_role_conversion() {
        assert_eq!(i32::from(Role::Driver), MDI_DRIVER);
        assert_eq!(i32::from(Role::Engine), MDI_ENGINE);
        assert_eq!(Role::from(MDI_DRIVER), Role::Driver);
        assert_eq!(Role::from(MDI_ENGINE), Role::Engine);
    }

    #[test]
    fn test_method_conversion() {
        assert_eq!(i32::from(Method::Tcp), MDI_TCP);
        assert_eq!(i32::from(Method::Mpi), MDI_MPI);
        assert_eq!(Method::from(MDI_TCP), Method::Tcp);
    }
}
