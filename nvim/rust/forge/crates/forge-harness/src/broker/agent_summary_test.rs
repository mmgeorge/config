use super::*;
use anyhow::{Context, Result};

/// The agents picker reads a child's duration and tool tally from the child's own exchange, the
/// clock and counter its status row, transcript row, and heading read. The picker once measured
/// the agent record from admission to its last update and summed calls without the denied and
/// successful split, so one child read `Ran 10s`, `Cancelled 10s`, and `Completed in 14s`.
#[test]
fn agent_summary_reads_the_child_exchange_clock_and_tool_tally() -> Result<()> {
    let agent = Agent::pending("session", "reviewer", "review", 0);
    let mut running = crate::storage::test::running_exchange("running");
    running.agent_id = agent.id.clone();
    running.tool_counter.total = 3;
    running.tool_counter.completed = 2;
    running.tool_counter.denied = 1;
    let summary = agent_summaries(std::slice::from_ref(&agent), std::slice::from_ref(&running));
    let entry = summary
        .get(&agent.id)
        .context("running child has no summary")?;
    assert_eq!(entry["elapsed_ms"], json!(20));
    assert_eq!(entry["running_since_ms"], json!(30));
    assert_eq!(entry["tool_label"], json!("2/3 tools · (1 denied)"));

    let mut settled = running.clone();
    settled.finish(crate::exchange::ExchangeOutcome::Cancelled, 10_017)?;
    let summary = agent_summaries(std::slice::from_ref(&agent), std::slice::from_ref(&settled));
    let entry = summary
        .get(&agent.id)
        .context("settled child has no summary")?;
    assert_eq!(entry["state"], json!("cancelled"));
    assert_eq!(entry["elapsed_ms"], json!(10_007));
    assert_eq!(entry["running_since_ms"], Value::Null);
    Ok(())
}
