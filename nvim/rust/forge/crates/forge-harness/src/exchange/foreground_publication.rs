//! Forward committed foreground publication without owning transitions.

use crate::broker::ForegroundSignal;
use anyhow::Result;
use forge_protocol::outbound::MessageSender;

use super::publication::SessionPublications;

/// Commit complete notifications and report retention evidence to the exchange owner.
pub(super) async fn forward_signal(
    signal: ForegroundSignal,
    publications: &SessionPublications,
    _message_sink: Option<&MessageSender>,
) -> ForegroundSignal {
    match signal {
        crate::broker::ForegroundSignal::ObservationPublicationNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::ObservationPublicationDelivered(result)
        }
        crate::broker::ForegroundSignal::CommandEventsNeedForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(())
            }
            .await;
            crate::broker::ForegroundSignal::CommandEventsForwarded(result)
        }
        crate::broker::ForegroundSignal::CommandNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::CommandPublished(result)
        }
        crate::broker::ForegroundSignal::PermissionNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::PermissionPublished(result)
        }
        crate::broker::ForegroundSignal::AdmissionEventsNeedForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(())
            }
            .await;
            crate::broker::ForegroundSignal::AdmissionEventsForwarded(result)
        }
        crate::broker::ForegroundSignal::FailureAdmissionEventsNeedForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(())
            }
            .await;
            crate::broker::ForegroundSignal::FailureAdmissionEventsForwarded(result)
        }
        crate::broker::ForegroundSignal::FailurePublicationNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::FailurePublicationCompleted(result)
        }
        crate::broker::ForegroundSignal::AutoCompactionNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::AutoCompactionPublished(result)
        }
        crate::broker::ForegroundSignal::CheckRepairNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::CheckRepairPublished(result)
        }
        crate::broker::ForegroundSignal::ClarificationNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::ClarificationPublished(result)
        }
        crate::broker::ForegroundSignal::FinalResponseNeedsForward { events } => {
            let result = async {
                publications.publish_batch(events)?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::FinalResponsePublished(result)
        }
        crate::broker::ForegroundSignal::PlanTerminalNeedsForward { events, receipts } => {
            let result = async {
                publications.publish_batch(events)?;
                receipts.acknowledge()?;
                Ok(Vec::new())
            }
            .await;
            crate::broker::ForegroundSignal::PlanTerminalPublished(result)
        }
        signal => signal,
    }
}

/// Retain a publication result even when an owned effect panics.
pub(super) async fn run_foreground_job(
    job: crate::broker::ForegroundJob,
    publication: bool,
) -> (bool, Result<crate::broker::ForegroundSignal>) {
    use futures_util::FutureExt;
    let result = std::panic::AssertUnwindSafe(job.run()).catch_unwind().await;
    (
        publication,
        result.unwrap_or_else(|_| Err(anyhow::anyhow!("foreground effect task panicked"))),
    )
}
