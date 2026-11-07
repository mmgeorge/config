//! Bound for test waits that must finish but should only fail on a genuine hang.
//!
//! Nextest terminates any test that runs for five seconds (see `.config/nextest.toml`). Guards
//! fire just before that so a stuck wait reports which step hung instead of a bare termination.
//! Tests that exercise a timeout path drive it deterministically rather than through this guard.

/// Upper bound on any awaited step a test expects to complete.
pub(crate) const HANG_GUARD: std::time::Duration = std::time::Duration::from_secs(4);
