//! Read the cancellation projection committed with the owner's lifecycle journal.

use super::*;

impl SqliteStore {
    /// Get the latest cancellation operation, including its terminal retirement state.
    pub(crate) fn cancellation_progress(
        &self,
        session: &str,
    ) -> Result<Option<crate::exchange::CancellationProgress>> {
        let encoded: Option<String> = self
            .connection()
            .query_row(
                "SELECT record FROM owner_cancellation WHERE session_id=?1",
                [session],
                |row| row.get(0),
            )
            .optional()?;
        encoded
            .map(|encoded| serde_json::from_str(&encoded).map_err(Into::into))
            .transpose()
    }
}
