//! MDI Test Engine in Rust
//!
//! This engine matches the behavior of engine_cxx for testing purposes.
//! Supports commands: EXIT, <NATOMS, <COORDS, >COORDS, <FORCES, <FORCES_B

use libc::{c_char, c_int, c_void};
use mdi_sys::*;
use std::env;
use std::ffi::{CStr, CString};

static mut EXIT_SIGNAL: bool = false;

fn execute_command(command: &str, comm: MDI_Comm) {
    // Set dummy molecular information (matching engine_cxx)
    let natoms: i64 = 10;
    let mut coords: Vec<f64> = (0..3 * natoms).map(|i| 0.1 * i as f64).collect();
    let forces: Vec<f64> = (0..3 * natoms).map(|i| 0.01 * i as f64).collect();

    match command {
        "EXIT" => {
            unsafe { EXIT_SIGNAL = true };
        }
        "<NATOMS" => unsafe {
            MDI_Send(&natoms as *const i64 as *const c_void, 1, MDI_INT64_T, comm);
        },
        "<COORDS" => unsafe {
            MDI_Send(
                coords.as_ptr() as *const c_void,
                (3 * natoms) as c_int,
                MDI_DOUBLE,
                comm,
            );
        },
        ">COORDS" => unsafe {
            MDI_Recv(
                coords.as_mut_ptr() as *mut c_void,
                (3 * natoms) as c_int,
                MDI_DOUBLE,
                comm,
            );
        },
        "<FORCES" => unsafe {
            MDI_Send(
                forces.as_ptr() as *const c_void,
                (3 * natoms) as c_int,
                MDI_DOUBLE,
                comm,
            );
        },
        "<FORCES_B" => unsafe {
            MDI_Send(
                forces.as_ptr() as *const c_void,
                (3 * natoms * 8) as c_int, // 8 bytes per double
                MDI_BYTE,
                comm,
            );
        },
        _ => {
            panic!("Unrecognized command: {}", command);
        }
    }
}

fn initialize_mdi(comm: &mut MDI_Comm) {
    // Confirm we're running as an engine
    let mut role: c_int = 0;
    unsafe { MDI_Get_role(&mut role) };
    if role != MDI_ENGINE {
        panic!("Must run engine_rust as an ENGINE");
    }

    // Register nodes and commands (matching engine_cxx)
    let default_node = CString::new("@DEFAULT").unwrap();
    let forces_node = CString::new("@FORCES").unwrap();

    unsafe {
        MDI_Register_node(default_node.as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new("EXIT").unwrap().as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new("<NATOMS").unwrap().as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new("<COORDS").unwrap().as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new(">COORDS").unwrap().as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new("<FORCES").unwrap().as_ptr());
        MDI_Register_command(default_node.as_ptr(), CString::new("<FORCES_B").unwrap().as_ptr());

        MDI_Register_node(forces_node.as_ptr());
        MDI_Register_command(forces_node.as_ptr(), CString::new("EXIT").unwrap().as_ptr());
        MDI_Register_command(forces_node.as_ptr(), CString::new("<FORCES").unwrap().as_ptr());
        MDI_Register_command(forces_node.as_ptr(), CString::new(">FORCES").unwrap().as_ptr());
        MDI_Register_callback(forces_node.as_ptr(), CString::new(">FORCES").unwrap().as_ptr());
    }

    // Accept connection from driver
    unsafe { MDI_Accept_communicator(comm) };
}

fn respond_to_commands(comm: MDI_Comm) {
    let mut command_buf = vec![0u8; MDI_COMMAND_LENGTH as usize];

    while unsafe { !EXIT_SIGNAL } {
        unsafe {
            MDI_Recv_command(command_buf.as_mut_ptr() as *mut c_char, comm);
        }

        let command_cstr = unsafe { CStr::from_ptr(command_buf.as_ptr() as *const c_char) };
        let command = command_cstr.to_str().unwrap().trim_end_matches('\0');

        execute_command(command, comm);
    }
}

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

    // Initialize MDI connection
    let mut comm: MDI_Comm = MDI_COMM_NULL;
    initialize_mdi(&mut comm);

    // Respond to driver commands
    respond_to_commands(comm);
}
