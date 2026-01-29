# Build the Rust driver using Cargo

# Find cargo
find_program(CARGO cargo REQUIRED)

# Set output directory
set(RUST_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/target/release)
set(DRIVER_RUST_EXECUTABLE ${CMAKE_BINARY_DIR}/driver_rust${CMAKE_EXECUTABLE_SUFFIX})

# Build command
add_custom_command(
    OUTPUT ${RUST_TARGET_DIR}/driver_rust${CMAKE_EXECUTABLE_SUFFIX}
    COMMAND ${CARGO} build --release
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building driver_rust with Cargo"
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/main.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/Cargo.toml
)

# Copy to build directory
add_custom_command(
    OUTPUT ${DRIVER_RUST_EXECUTABLE}
    COMMAND ${CMAKE_COMMAND} -E copy
            ${RUST_TARGET_DIR}/driver_rust${CMAKE_EXECUTABLE_SUFFIX}
            ${DRIVER_RUST_EXECUTABLE}
    DEPENDS ${RUST_TARGET_DIR}/driver_rust${CMAKE_EXECUTABLE_SUFFIX}
    COMMENT "Copying driver_rust to build directory"
)

add_custom_target(driver_rust ALL DEPENDS ${DRIVER_RUST_EXECUTABLE})
