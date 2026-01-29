//! Error types for the MDI library

use thiserror::Error;

/// Result type for MDI operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error type for MDI operations
#[derive(Error, Debug)]
pub enum Error {
    /// MDI initialization failed
    #[error("MDI initialization failed")]
    InitializationFailed,

    /// Communication operation failed
    #[error("MDI communication failed")]
    CommunicationFailed,

    /// Invalid string (contains null bytes)
    #[error("Invalid string: contains null bytes")]
    InvalidString,

    /// Unit conversion failed
    #[error("Unit conversion failed")]
    ConversionFailed,

    /// Node not found
    #[error("Node not found: {0}")]
    NodeNotFound(String),

    /// Command not found
    #[error("Command not found: {0}")]
    CommandNotFound(String),

    /// Buffer too small
    #[error("Buffer too small: need {needed} bytes, got {available}")]
    BufferTooSmall { needed: usize, available: usize },

    /// Type mismatch
    #[error("Type mismatch in MDI operation")]
    TypeMismatch,

    /// Unknown error code from MDI
    #[error("MDI error code: {0}")]
    Unknown(i32),
}
