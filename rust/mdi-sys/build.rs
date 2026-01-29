//! Build script for mdi-sys
//!
//! This script compiles the MDI C library and links it with Rust.

use std::env;
use std::path::PathBuf;

fn main() {
    let mdi_lib_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("MDI_Library");

    // Compile the MDI library
    let mut build = cc::Build::new();

    build
        .file(mdi_lib_dir.join("mdi.c"))
        .file(mdi_lib_dir.join("mdi_general.c"))
        .file(mdi_lib_dir.join("mdi_global.c"))
        .file(mdi_lib_dir.join("mdi_lib.c"))
        .file(mdi_lib_dir.join("mdi_tcp.c"))
        .file(mdi_lib_dir.join("mdi_mpi.c"))
        .file(mdi_lib_dir.join("mdi_test.c"))
        .include(&mdi_lib_dir)
        .include(mdi_lib_dir.join("STUBS_MPI"))
        .define("_MDI_PLUGIN_SUPPORT", "0")  // Disable plugin support for simplicity
        .warnings(false);

    // Platform-specific settings
    #[cfg(target_os = "windows")]
    {
        build.define("_WIN32", None);
        println!("cargo:rustc-link-lib=ws2_32");
        println!("cargo:rustc-link-lib=wsock32");
    }

    #[cfg(unix)]
    {
        println!("cargo:rustc-link-lib=dl");
    }

    build.compile("mdi");

    // Tell cargo to invalidate the built crate whenever the C sources change
    println!("cargo:rerun-if-changed={}", mdi_lib_dir.display());
}
