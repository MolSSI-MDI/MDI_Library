//! Data type handling for MDI communication

use mdi_sys::*;

/// MDI data types for send/receive operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    /// Platform-dependent integer
    Int,
    /// 8-bit signed integer
    Int8,
    /// 16-bit signed integer
    Int16,
    /// 32-bit signed integer
    Int32,
    /// 64-bit signed integer
    Int64,
    /// 8-bit unsigned integer
    UInt8,
    /// 16-bit unsigned integer
    UInt16,
    /// 32-bit unsigned integer
    UInt32,
    /// 64-bit unsigned integer
    UInt64,
    /// Double-precision floating point
    Double,
    /// Single-precision floating point
    Float,
    /// Character
    Char,
    /// Raw byte
    Byte,
}

impl DataType {
    /// Convert to the raw MDI_Datatype value
    pub fn to_raw(self) -> MDI_Datatype {
        match self {
            DataType::Int => MDI_INT,
            DataType::Int8 => MDI_INT8_T,
            DataType::Int16 => MDI_INT16_T,
            DataType::Int32 => MDI_INT32_T,
            DataType::Int64 => MDI_INT64_T,
            DataType::UInt8 => MDI_UINT8_T,
            DataType::UInt16 => MDI_UINT16_T,
            DataType::UInt32 => MDI_UINT32_T,
            DataType::UInt64 => MDI_UINT64_T,
            DataType::Double => MDI_DOUBLE,
            DataType::Float => MDI_FLOAT,
            DataType::Char => MDI_CHAR,
            DataType::Byte => MDI_BYTE,
        }
    }

    /// Get the size in bytes of this data type
    pub fn size(self) -> usize {
        match self {
            DataType::Int => std::mem::size_of::<libc::c_int>(),
            DataType::Int8 => 1,
            DataType::Int16 => 2,
            DataType::Int32 => 4,
            DataType::Int64 => 8,
            DataType::UInt8 => 1,
            DataType::UInt16 => 2,
            DataType::UInt32 => 4,
            DataType::UInt64 => 8,
            DataType::Double => 8,
            DataType::Float => 4,
            DataType::Char => 1,
            DataType::Byte => 1,
        }
    }
}

/// Trait for types that can be sent/received through MDI
///
/// # Safety
/// This trait is unsafe because implementations must ensure that the type
/// is compatible with the specified MDI_Datatype.
pub unsafe trait MdiData: Sized {
    /// The MDI data type for this Rust type
    const MDI_TYPE: DataType;

    /// Get the raw MDI_Datatype value
    fn mdi_datatype() -> MDI_Datatype {
        Self::MDI_TYPE.to_raw()
    }
}

// Implement MdiData for primitive types
unsafe impl MdiData for i8 {
    const MDI_TYPE: DataType = DataType::Int8;
}

unsafe impl MdiData for i16 {
    const MDI_TYPE: DataType = DataType::Int16;
}

unsafe impl MdiData for i32 {
    const MDI_TYPE: DataType = DataType::Int32;
}

unsafe impl MdiData for i64 {
    const MDI_TYPE: DataType = DataType::Int64;
}

unsafe impl MdiData for u8 {
    const MDI_TYPE: DataType = DataType::UInt8;
}

unsafe impl MdiData for u16 {
    const MDI_TYPE: DataType = DataType::UInt16;
}

unsafe impl MdiData for u32 {
    const MDI_TYPE: DataType = DataType::UInt32;
}

unsafe impl MdiData for u64 {
    const MDI_TYPE: DataType = DataType::UInt64;
}

unsafe impl MdiData for f32 {
    const MDI_TYPE: DataType = DataType::Float;
}

unsafe impl MdiData for f64 {
    const MDI_TYPE: DataType = DataType::Double;
}
