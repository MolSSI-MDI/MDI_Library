//! MDI Test Driver in Rust
//!
//! This driver matches the behavior of driver_cxx for testing purposes.
//! Expected output: " Engine name: MM\n"

use libc::{c_char, c_int, c_void};
use mdi_sys::*;
use std::env;
use std::ffi::{CStr, CString};

fn main() {
    // Parse command line arguments to find -mdi option
    let args: Vec<String> = env::args().collect();
    let mut mdi_options: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        if args[i] == "-mdi" && i + 1 < args.len() {
            mdi_options = Some(args[i + 1].clone());
            i += 2;
        } else {
            i += 1;
        }
    }

    let options = mdi_options.expect("Must provide -mdi option");
    let c_options = CString::new(options).expect("Invalid options string");

    // Initialize MDI - must call MDI_Init_code() first
    let ret = unsafe { MDI_Init_code() };
    if ret != 0 {
        panic!("MDI_Init_code failed");
    }

    let ret = unsafe { MDI_Init_with_options(c_options.as_ptr()) };
    if ret != 0 {
        panic!("MDI_Init_with_options failed");
    }

    // Verify MDI initialized
    let mut initialized: c_int = 0;
    let ret = unsafe { MDI_Initialized(&mut initialized) };
    if ret != 0 || initialized == 0 {
        panic!("MDI not initialized");
    }

    // Confirm we're running as a driver
    let mut role: c_int = 0;
    unsafe { MDI_Get_role(&mut role) };
    if role != MDI_DRIVER {
        panic!("Must run driver_rust as a DRIVER");
    }

    // Wait for a communicator to become available
    let mut new_comm_flag: c_int = 0;
    while new_comm_flag != 1 {
        unsafe { MDI_Check_for_communicator(&mut new_comm_flag) };
    }

    // Accept the communicator
    let mut comm: MDI_Comm = MDI_COMM_NULL;
    unsafe { MDI_Accept_communicator(&mut comm) };

    // Check that there are no additional communicators
    unsafe { MDI_Check_for_communicator(&mut new_comm_flag) };
    if new_comm_flag != 0 {
        panic!("After accepting, there is still a communicator");
    }

    // Confirm engine has @DEFAULT node
    let default_node = CString::new("@DEFAULT").unwrap();
    let mut exists: c_int = 0;
    unsafe { MDI_Check_node_exists(default_node.as_ptr(), comm, &mut exists) };
    if exists != 1 {
        panic!("Engine does not have @DEFAULT node");
    }

    // Confirm engine supports EXIT command
    let exit_cmd = CString::new("EXIT").unwrap();
    unsafe { MDI_Check_command_exists(default_node.as_ptr(), exit_cmd.as_ptr(), comm, &mut exists) };
    if exists != 1 {
        panic!("Engine does not support EXIT command");
    }

    // Get the engine name
    let name_cmd = CString::new("<NAME").unwrap();
    unsafe { MDI_Send_command(name_cmd.as_ptr(), comm) };

    let mut engine_name = vec![0u8; MDI_NAME_LENGTH as usize];
    unsafe {
        MDI_Recv(
            engine_name.as_mut_ptr() as *mut c_void,
            MDI_NAME_LENGTH,
            MDI_CHAR,
            comm,
        )
    };

    // Print the engine name (matching C++ output format exactly)
    let name_cstr = unsafe { CStr::from_ptr(engine_name.as_ptr() as *const c_char) };
    let name = name_cstr.to_str().unwrap().trim_end_matches('\0');
    println!(" Engine name: {}", name);

    // Send EXIT command
    unsafe { MDI_Send_command(exit_cmd.as_ptr(), comm) };
}
