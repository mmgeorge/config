use serde::{Deserialize, Serialize};

/// Select the minimum diagnostic severity, independently of execution behavior.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum LogLevel {
    /// Disable diagnostic recording.
    None,
    /// Record errors only.
    Error,
    /// Record warnings and errors.
    Warn,
    /// Record coarse operational events, warnings, and errors.
    #[default]
    Info,
    /// Include verbose instrumentation and dependency diagnostics.
    Trace,
}

impl LogLevel {
    /// Decide whether a tracing event meets the configured threshold.
    pub(super) fn allows(self, level: &tracing::Level) -> bool {
        match self {
            Self::None => false,
            Self::Error => *level <= tracing::Level::ERROR,
            Self::Warn => *level <= tracing::Level::WARN,
            Self::Info => *level <= tracing::Level::INFO,
            Self::Trace => true,
        }
    }

    /// Encode the private atomic cache without changing the persistent representation.
    pub(super) const fn encode(self) -> u8 {
        match self {
            Self::None => 0,
            Self::Error => 1,
            Self::Warn => 2,
            Self::Info => 3,
            Self::Trace => 4,
        }
    }

    /// Decode values written exclusively by the logging owner.
    pub(super) fn decode(value: u8) -> Self {
        match value {
            0 => Self::None,
            1 => Self::Error,
            2 => Self::Warn,
            3 => Self::Info,
            4 => Self::Trace,
            _ => unreachable!("logging level cache contains only encoded LogLevel values"),
        }
    }
}
