//! Shared wire records and bounded transport primitives for the Forge host.
//!
//! Outbound frame limits govern individual deliveries, not complete request content.
//! Bounded reads retain each complete JSONL request before decoding.

pub mod contract;
pub mod credit;
pub mod frame;
pub mod input;
pub mod message;
pub mod method_policy;
pub mod outbound;
pub mod snapshot;
pub mod transfer;

pub const WIRE_VERSION: u32 = 39;

pub const MAX_FRAME_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_ACTIVE_REQUESTS: usize = 256;
pub const MAX_PENDING_FRAMES: usize = 4096;
pub const RESERVED_CONTROL_RECORDS: usize = 32;
pub const MAX_SNAPSHOT_PART_BYTES: usize = 256 * 1024;
