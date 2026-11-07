//! Harness-authored provider input that delivers child-agent outcomes to a parent.
//!
//! These messages reach the provider without a host submission, so a provider that echoes its
//! input as a native user item cannot correlate the echo by submission. The shared preambles
//! let the exchange recognize those echoes as evidence of input the parent's rows already show.

/// Opens each child-result report attached to a parent's input.
pub const CHILD_RESULT_PREAMBLE: &str =
    "Harness child result. This is a report from a child agent, not a new user request.";

/// Opens the parent continuation that synthesizes settled child results.
pub const CHILD_CONTINUATION_PREAMBLE: &str =
    "Continue this same unfinished request using the child messages below.";

/// Report whether native user text is an echo of Harness-authored child-result input.
pub fn is_child_report_echo(text: &str) -> bool {
    let text = text.trim_start();
    text.starts_with(CHILD_RESULT_PREAMBLE) || text.starts_with(CHILD_CONTINUATION_PREAMBLE)
}
