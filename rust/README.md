# MDI Rust Bindings

Rust bindings for the MolSSI Driver Interface (MDI) library.

## Crates

This workspace contains two crates:

- **`mdi-sys`**: Raw, unsafe FFI bindings to the MDI C library
- **`mdi`**: Safe, idiomatic Rust wrapper around `mdi-sys`

## Building

```bash
cd rust
cargo build
```

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
mdi = { path = "path/to/mdi" }
```

Or for raw bindings only:

```toml
[dependencies]
mdi-sys = { path = "path/to/mdi-sys" }
```

## Examples

### Driver Example

```rust
use mdi::{Mdi, Role, Result};

fn main() -> Result<()> {
    // Initialize MDI as a driver
    Mdi::init_with_options("-role DRIVER -name driver -method TCP -port 8021")?;

    // Accept a connection from an engine
    let comm = Mdi::accept_communicator()?;

    // Query supported nodes
    let nodes = comm.get_nodes()?;
    println!("Engine supports nodes: {:?}", nodes);

    // Send a command and receive data
    comm.send_command("<ENERGY")?;
    let energy: f64 = comm.recv_scalar()?;
    println!("Energy: {}", energy);

    // Send exit command
    comm.send_command("EXIT")?;

    Ok(())
}
```

### Engine Example

```rust
use mdi::{Mdi, Role, Result};

fn main() -> Result<()> {
    // Initialize MDI as an engine
    Mdi::init_with_options("-role ENGINE -name engine -method TCP -hostname localhost -port 8021")?;

    // Register supported nodes and commands
    Mdi::register_node("@DEFAULT")?;
    Mdi::register_command("@DEFAULT", "<ENERGY")?;
    Mdi::register_command("@DEFAULT", "EXIT")?;

    // Accept connection from driver
    let comm = Mdi::accept_communicator()?;

    // Command loop
    loop {
        let command = comm.recv_command()?;
        match command.as_str() {
            "<ENERGY" => {
                let energy = -123.456f64;
                comm.send_scalar(energy)?;
            }
            "EXIT" => break,
            _ => {}
        }
    }

    Ok(())
}
```

## API Overview

### Main Types

- `Mdi` - Static methods for initialization and global operations
- `Communicator` - Handle for communication with another code
- `Role` - Either `Driver` or `Engine`
- `Method` - Communication method (`Tcp`, `Mpi`, `Link`, `Test`)
- `DataType` - MDI data types for send/receive operations
- `MdiData` - Trait for types that can be sent/received

### Key Functions

```rust
// Initialization
Mdi::init_with_options(options: &str) -> Result<()>
Mdi::is_initialized() -> Result<bool>
Mdi::get_role() -> Result<Role>

// Communicator management
Mdi::accept_communicator() -> Result<Communicator>
Mdi::check_for_communicator() -> Result<bool>

// Node/command registration (for engines)
Mdi::register_node(name: &str) -> Result<()>
Mdi::register_command(node: &str, command: &str) -> Result<()>

// Utilities
Mdi::conversion_factor(from: &str, to: &str) -> Result<f64>
Mdi::element_to_atomic_number(symbol: &str) -> Result<i32>
```

### Communicator Methods

```rust
// Commands
comm.send_command(command: &str) -> Result<()>
comm.recv_command() -> Result<String>

// Scalar data
comm.send_scalar<T: MdiData>(value: T) -> Result<()>
comm.recv_scalar<T: MdiData>() -> Result<T>

// Array data
comm.send_array<T: MdiData>(data: &[T]) -> Result<()>
comm.recv_array<T: MdiData>(count: usize) -> Result<Vec<T>>

// Node/command queries
comm.node_exists(name: &str) -> Result<bool>
comm.get_nodes() -> Result<Vec<String>>
comm.command_exists(node: &str, command: &str) -> Result<bool>
comm.get_commands(node: &str) -> Result<Vec<String>>
```

## Supported Data Types

The following Rust types implement `MdiData` and can be used with send/recv:

| Rust Type | MDI Type |
|-----------|----------|
| `i8` | `MDI_INT8_T` |
| `i16` | `MDI_INT16_T` |
| `i32` | `MDI_INT32_T` |
| `i64` | `MDI_INT64_T` |
| `u8` | `MDI_UINT8_T` |
| `u16` | `MDI_UINT16_T` |
| `u32` | `MDI_UINT32_T` |
| `u64` | `MDI_UINT64_T` |
| `f32` | `MDI_FLOAT` |
| `f64` | `MDI_DOUBLE` |

## License

BSD-3-Clause (same as MDI library)
