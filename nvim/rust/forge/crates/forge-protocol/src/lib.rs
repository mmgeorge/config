//! Shared wire records and bounded transport primitives for the Forge host.
//!
//! Framing bounds apply before JSON deserialization. A framing failure poisons
//! the connection so a discarded prefix can never become a second request.

pub mod credit;
pub mod frame;
pub mod input;
pub mod message;
pub mod outbound;
pub mod snapshot;
pub mod transfer;

pub const WIRE_VERSION: u32 = 4;

pub const MAX_FRAME_BYTES: usize = 512 * 1024;
pub const MAX_ACTIVE_REQUESTS: usize = 64;
pub const MAX_QUEUED_BYTES: usize = 8 * 1024 * 1024;
pub const MAX_PENDING_FRAMES: usize = 128;
pub const RESERVED_CONTROL_RECORDS: usize = 32;
pub const MAX_SNAPSHOT_PART_BYTES: usize = 256 * 1024;
pub const MAX_SNAPSHOT_BYTES: usize = 16 * 1024 * 1024;
