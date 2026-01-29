//! MDI Communicator for sending and receiving data

use crate::datatype::MdiData;
use crate::error::{Error, Result};
use crate::{constants, Method};
use mdi_sys::*;
use std::ffi::{CStr, CString};

/// An MDI communicator for communication with another code
#[derive(Debug, Clone, Copy)]
pub struct Communicator {
    handle: MDI_Comm,
}

impl Communicator {
    /// Create a new communicator from a raw handle
    pub(crate) fn new(handle: MDI_Comm) -> Self {
        Self { handle }
    }

    /// Get the raw MDI_Comm handle
    pub fn raw(&self) -> MDI_Comm {
        self.handle
    }

    /// Check if this is a null communicator
    pub fn is_null(&self) -> bool {
        self.handle == MDI_COMM_NULL
    }

    /// Get the communication method used by this communicator
    pub fn get_method(&self) -> Result<Method> {
        let mut method: i32 = 0;
        let ret = unsafe { MDI_Get_method(&mut method, self.handle) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(Method::from(method))
    }

    // =========================================================================
    // Send/Receive Operations
    // =========================================================================

    /// Send a command string
    pub fn send_command(&self, command: &str) -> Result<()> {
        let c_command = CString::new(command).map_err(|_| Error::InvalidString)?;
        let ret = unsafe { MDI_Send_command(c_command.as_ptr(), self.handle) };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(())
    }

    /// Receive a command string
    pub fn recv_command(&self) -> Result<String> {
        let mut buffer = vec![0u8; constants::COMMAND_LENGTH];
        let ret = unsafe { MDI_Recv_command(buffer.as_mut_ptr() as *mut i8, self.handle) };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }

        // Find the null terminator and convert to String
        let c_str = unsafe { CStr::from_ptr(buffer.as_ptr() as *const i8) };
        Ok(c_str.to_string_lossy().trim_end().to_string())
    }

    /// Send a single scalar value
    pub fn send_scalar<T: MdiData>(&self, value: T) -> Result<()> {
        let ret = unsafe {
            MDI_Send(
                &value as *const T as *const libc::c_void,
                1,
                T::mdi_datatype(),
                self.handle,
            )
        };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(())
    }

    /// Receive a single scalar value
    pub fn recv_scalar<T: MdiData>(&self) -> Result<T> {
        let mut value = std::mem::MaybeUninit::<T>::uninit();
        let ret = unsafe {
            MDI_Recv(
                value.as_mut_ptr() as *mut libc::c_void,
                1,
                T::mdi_datatype(),
                self.handle,
            )
        };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(unsafe { value.assume_init() })
    }

    /// Send an array of values
    pub fn send_array<T: MdiData>(&self, data: &[T]) -> Result<()> {
        let ret = unsafe {
            MDI_Send(
                data.as_ptr() as *const libc::c_void,
                data.len() as i32,
                T::mdi_datatype(),
                self.handle,
            )
        };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(())
    }

    /// Receive an array of values
    pub fn recv_array<T: MdiData>(&self, count: usize) -> Result<Vec<T>> {
        let mut data = Vec::with_capacity(count);
        let ret = unsafe {
            MDI_Recv(
                data.as_mut_ptr() as *mut libc::c_void,
                count as i32,
                T::mdi_datatype(),
                self.handle,
            )
        };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        unsafe { data.set_len(count) };
        Ok(data)
    }

    /// Receive data into an existing buffer
    pub fn recv_into<T: MdiData>(&self, buffer: &mut [T]) -> Result<()> {
        let ret = unsafe {
            MDI_Recv(
                buffer.as_mut_ptr() as *mut libc::c_void,
                buffer.len() as i32,
                T::mdi_datatype(),
                self.handle,
            )
        };
        if ret != 0 {
            return Err(Error::CommunicationFailed);
        }
        Ok(())
    }

    // =========================================================================
    // Node/Command/Callback Queries
    // =========================================================================

    /// Check if a node exists on the connected code
    pub fn node_exists(&self, node_name: &str) -> Result<bool> {
        let c_name = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let mut flag: i32 = 0;
        let ret = unsafe { MDI_Check_node_exists(c_name.as_ptr(), self.handle, &mut flag) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(flag != 0)
    }

    /// Get the number of nodes supported by the connected code
    pub fn get_num_nodes(&self) -> Result<i32> {
        let mut nnodes: i32 = 0;
        let ret = unsafe { MDI_Get_nnodes(self.handle, &mut nnodes) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(nnodes)
    }

    /// Get a node name by index
    pub fn get_node(&self, index: i32) -> Result<String> {
        let mut buffer = vec![0u8; constants::COMMAND_LENGTH];
        let ret =
            unsafe { MDI_Get_node(index, self.handle, buffer.as_mut_ptr() as *mut i8) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        let c_str = unsafe { CStr::from_ptr(buffer.as_ptr() as *const i8) };
        Ok(c_str.to_string_lossy().trim_end().to_string())
    }

    /// Get all node names
    pub fn get_nodes(&self) -> Result<Vec<String>> {
        let num_nodes = self.get_num_nodes()?;
        let mut nodes = Vec::with_capacity(num_nodes as usize);
        for i in 0..num_nodes {
            nodes.push(self.get_node(i)?);
        }
        Ok(nodes)
    }

    /// Check if a command exists for a node
    pub fn command_exists(&self, node_name: &str, command_name: &str) -> Result<bool> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let c_cmd = CString::new(command_name).map_err(|_| Error::InvalidString)?;
        let mut flag: i32 = 0;
        let ret = unsafe {
            MDI_Check_command_exists(c_node.as_ptr(), c_cmd.as_ptr(), self.handle, &mut flag)
        };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(flag != 0)
    }

    /// Get the number of commands for a node
    pub fn get_num_commands(&self, node_name: &str) -> Result<i32> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let mut ncommands: i32 = 0;
        let ret = unsafe { MDI_Get_ncommands(c_node.as_ptr(), self.handle, &mut ncommands) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(ncommands)
    }

    /// Get a command name by index
    pub fn get_command(&self, node_name: &str, index: i32) -> Result<String> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let mut buffer = vec![0u8; constants::COMMAND_LENGTH];
        let ret = unsafe {
            MDI_Get_command(
                c_node.as_ptr(),
                index,
                self.handle,
                buffer.as_mut_ptr() as *mut i8,
            )
        };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        let c_str = unsafe { CStr::from_ptr(buffer.as_ptr() as *const i8) };
        Ok(c_str.to_string_lossy().trim_end().to_string())
    }

    /// Get all commands for a node
    pub fn get_commands(&self, node_name: &str) -> Result<Vec<String>> {
        let num_commands = self.get_num_commands(node_name)?;
        let mut commands = Vec::with_capacity(num_commands as usize);
        for i in 0..num_commands {
            commands.push(self.get_command(node_name, i)?);
        }
        Ok(commands)
    }

    /// Check if a callback exists for a node
    pub fn callback_exists(&self, node_name: &str, callback_name: &str) -> Result<bool> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let c_cb = CString::new(callback_name).map_err(|_| Error::InvalidString)?;
        let mut flag: i32 = 0;
        let ret = unsafe {
            MDI_Check_callback_exists(c_node.as_ptr(), c_cb.as_ptr(), self.handle, &mut flag)
        };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(flag != 0)
    }

    /// Get the number of callbacks for a node
    pub fn get_num_callbacks(&self, node_name: &str) -> Result<i32> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let mut ncallbacks: i32 = 0;
        let ret = unsafe { MDI_Get_ncallbacks(c_node.as_ptr(), self.handle, &mut ncallbacks) };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        Ok(ncallbacks)
    }

    /// Get a callback name by index
    pub fn get_callback(&self, node_name: &str, index: i32) -> Result<String> {
        let c_node = CString::new(node_name).map_err(|_| Error::InvalidString)?;
        let mut buffer = vec![0u8; constants::COMMAND_LENGTH];
        let ret = unsafe {
            MDI_Get_callback(
                c_node.as_ptr(),
                index,
                self.handle,
                buffer.as_mut_ptr() as *mut i8,
            )
        };
        if ret != 0 {
            return Err(Error::Unknown(ret));
        }
        let c_str = unsafe { CStr::from_ptr(buffer.as_ptr() as *const i8) };
        Ok(c_str.to_string_lossy().trim_end().to_string())
    }

    /// Get all callbacks for a node
    pub fn get_callbacks(&self, node_name: &str) -> Result<Vec<String>> {
        let num_callbacks = self.get_num_callbacks(node_name)?;
        let mut callbacks = Vec::with_capacity(num_callbacks as usize);
        for i in 0..num_callbacks {
            callbacks.push(self.get_callback(node_name, i)?);
        }
        Ok(callbacks)
    }
}
