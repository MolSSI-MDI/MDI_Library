//! Example MDI engine in Rust
//!
//! Run with:
//! ```
//! cargo run --example engine -- -role ENGINE -name engine -method TCP -hostname localhost -port 8021
//! ```

use mdi::{Mdi, Role, Result};
use std::env;

fn main() -> Result<()> {
    // Get command-line arguments and build options string
    let args: Vec<String> = env::args().skip(1).collect();
    let options = if args.is_empty() {
        "-role ENGINE -name my_engine -method TEST".to_string()
    } else {
        args.join(" ")
    };

    println!("Initializing MDI engine with options: {}", options);

    // Initialize MDI
    Mdi::init_with_options(&options)?;

    // Verify we are an engine
    let role = Mdi::get_role()?;
    assert_eq!(role, Role::Engine, "Expected ENGINE role");
    println!("Role: Engine");

    // Register our supported nodes and commands
    Mdi::register_node("@DEFAULT")?;
    Mdi::register_command("@DEFAULT", "<NAME")?;
    Mdi::register_command("@DEFAULT", "<ENERGY")?;
    Mdi::register_command("@DEFAULT", ">COORDS")?;
    Mdi::register_command("@DEFAULT", "EXIT")?;

    println!("Registered nodes and commands");

    // Accept connection from driver
    println!("Waiting for driver connection...");
    let comm = Mdi::accept_communicator()?;
    println!("Driver connected!");

    // Main command loop
    loop {
        let command = comm.recv_command()?;
        println!("Received command: {}", command);

        match command.as_str() {
            "<NAME" => {
                // Send back our name
                comm.send_command("my_engine")?;
            }
            "<ENERGY" => {
                // Send back a mock energy value
                let energy: f64 = -123.456;
                comm.send_scalar(energy)?;
            }
            ">COORDS" => {
                // Receive coordinates (3 atoms * 3 coords = 9 values)
                let coords: Vec<f64> = comm.recv_array(9)?;
                println!("Received coordinates: {:?}", coords);
            }
            "EXIT" => {
                println!("Received EXIT command, shutting down");
                break;
            }
            _ => {
                eprintln!("Unknown command: {}", command);
            }
        }
    }

    println!("Engine finished successfully");
    Ok(())
}
