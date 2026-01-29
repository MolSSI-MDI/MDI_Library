# Build the MDI Rust bindings using Cargo
# CARGO is found by the parent CMakeLists.txt

message(STATUS "Building MDI Rust bindings")

# Set output directories
set(RUST_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/target/release)
set(MDI_RUST_LIB_DIR ${CMAKE_BINARY_DIR}/lib/mdi/rust)

# Build both mdi-sys and mdi crates
add_custom_command(
    OUTPUT ${RUST_TARGET_DIR}/libmdi.rlib
           ${RUST_TARGET_DIR}/libmdi_sys.rlib
    COMMAND ${CARGO} build --release --target-dir ${CMAKE_CURRENT_BINARY_DIR}/target
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building MDI Rust bindings with Cargo"
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/mdi-sys/src/lib.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi-sys/Cargo.toml
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi-sys/build.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi/src/lib.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi/src/comm.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi/src/datatype.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi/src/error.rs
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi/Cargo.toml
            ${CMAKE_CURRENT_SOURCE_DIR}/Cargo.toml
)

# Create output directory and copy artifacts
add_custom_command(
    OUTPUT ${MDI_RUST_LIB_DIR}/mdi-sys
           ${MDI_RUST_LIB_DIR}/mdi
    COMMAND ${CMAKE_COMMAND} -E make_directory ${MDI_RUST_LIB_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi-sys
            ${MDI_RUST_LIB_DIR}/mdi-sys
    COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_CURRENT_SOURCE_DIR}/mdi
            ${MDI_RUST_LIB_DIR}/mdi
    COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/Cargo.toml
            ${MDI_RUST_LIB_DIR}/Cargo.toml
    COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_CURRENT_SOURCE_DIR}/README.md
            ${MDI_RUST_LIB_DIR}/README.md
    DEPENDS ${RUST_TARGET_DIR}/libmdi.rlib
            ${RUST_TARGET_DIR}/libmdi_sys.rlib
    COMMENT "Copying MDI Rust bindings to lib directory"
)

add_custom_target(mdi_rust ALL
    DEPENDS ${MDI_RUST_LIB_DIR}/mdi-sys
            ${MDI_RUST_LIB_DIR}/mdi
)

# Install the Rust bindings
install(DIRECTORY ${MDI_RUST_LIB_DIR}/mdi-sys
        DESTINATION share/mdi/rust)
install(DIRECTORY ${MDI_RUST_LIB_DIR}/mdi
        DESTINATION share/mdi/rust)
install(FILES ${MDI_RUST_LIB_DIR}/Cargo.toml
        DESTINATION share/mdi/rust)
install(FILES ${MDI_RUST_LIB_DIR}/README.md
        DESTINATION share/mdi/rust)
