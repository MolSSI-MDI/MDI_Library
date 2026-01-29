//! Example MDI driver in Rust
//!
//! Run with:
//! ```
//! cargo run --example driver -- -role DRIVER -name driver -method TCP -port 8021
//! ```

use mdi::{Mdi, Role, Result};
use std::env;

fn main() -> Result<()> {
    // Get command-line arguments and build options string
    let args: Vec<String> = env::args().skip(1).collect();
    let options = if args.is_empty() {
        "-role DRIVER -name driver -method TEST".to_string()
    } else {
        args.join(" ")
    };

    println!("Initializing MDI driver with options: {}", options);

    // Initialize MDI
    Mdi::init_with_options(&options)?;

    // Verify initialization
    if !Mdi::is_initialized()? {
        eprintln!("MDI failed to initialize");
        return Ok(());
    }

    // Verify we are a driver
    let role = Mdi::get_role()?;
    assert_eq!(role, Role::Driver, "Expected DRIVER role");
    println!("Role: Driver");

    // Print version info
    println!(
        "MDI version: {}.{}.{}",
        mdi::version::MAJOR,
        mdi::version::MINOR,
        mdi::version::PATCH
    );

    // Test unit conversion
    let angstrom_to_bohr = Mdi::conversion_factor("angstrom", "bohr")?;
    println!("1 angstrom = {} bohr", angstrom_to_bohr);

    // Test element lookup
    let carbon_z = Mdi::element_to_atomic_number("C")?;
    println!("Atomic number of C: {}", carbon_z);

    // Accept a communicator from an engine
    println!("Waiting for engine connection...");
    let comm = Mdi::accept_communicator()?;
    println!("Engine connected!");

    // Query supported nodes
    if let Ok(nodes) = comm.get_nodes() {
        println!("Supported nodes: {:?}", nodes);
    }

    // Send commands and receive data
    comm.send_command("<NAME")?;
    let name = comm.recv_command()?;
    println!("Engine name: {}", name);

    // Send exit command
    comm.send_command("EXIT")?;

    println!("Driver finished successfully");
    Ok(())
}
