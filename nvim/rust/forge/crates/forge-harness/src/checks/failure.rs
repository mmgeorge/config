use super::CheckState;

#[derive(Debug)]
/// Retains a validated execution category across contextual error propagation.
pub(super) struct CheckFailure {
    pub state: CheckState,
    detail: anyhow::Error,
}

impl CheckFailure {
    /// Preserves the original diagnostic while assigning its terminal check state.
    pub fn classified(state: CheckState, detail: impl Into<anyhow::Error>) -> anyhow::Error {
        Self {
            state,
            detail: detail.into(),
        }
        .into()
    }

    /// Separates an unavailable executable from failures in process ownership or operating-system I/O.
    pub fn launch(error: std::io::Error) -> anyhow::Error {
        let state = if error.kind() == std::io::ErrorKind::NotFound {
            CheckState::Blocked
        } else {
            CheckState::ExecutionError
        };
        Self::classified(state, error)
    }
}

impl std::fmt::Display for CheckFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:#}", self.detail)
    }
}

impl std::error::Error for CheckFailure {}

pub(super) fn classify(error: &anyhow::Error, fallback: CheckState) -> CheckState {
    if let Some(failure) = error.downcast_ref::<CheckFailure>() {
        failure.state
    } else if error.is::<std::io::Error>() {
        CheckState::ExecutionError
    } else {
        fallback
    }
}

#[cfg(test)]
mod tests {}
