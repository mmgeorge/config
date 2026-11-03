use super::ControlToolArgumentError;
use std::error::Error;
use std::fmt;

/// The typed result returned by Harness tool handlers.
pub type ToolResult<T> = Result<T, ToolError>;

/// Preserve expected tool rejections and lower-level causes until response rendering.
#[derive(Debug)]
pub enum ToolError {
    /// The request does not satisfy the advertised input schema.
    InvalidArguments(Box<dyn Error + Send + Sync>),
    /// The requested plan differs from the active plan.
    PlanIdMismatch { requested: String, active: String },
    /// The current interaction lacks state required by the operation.
    MissingState(String),
    /// A valid request is rejected by the operation's state or policy.
    Rejected(String),
    /// An operation failed while decoding or encoding JSON.
    Json(serde_json::Error),
    /// An operation failed during filesystem or stream access.
    Io(std::io::Error),
    /// Retain an existing domain error without flattening its source chain.
    Operation(Box<dyn Error + Send + Sync>),
    /// Explain which operation failed while retaining its typed cause.
    Context {
        message: String,
        source: Box<ToolError>,
    },
}

impl ToolError {
    /// Test the complete source chain without discarding a converted domain error.
    pub fn is<E: Error + 'static>(&self) -> bool {
        let mut source: Option<&(dyn Error + 'static)> = Some(self);
        while let Some(error) = source {
            if error.is::<E>() {
                return true;
            }
            source = error.source();
        }
        false
    }
}

impl fmt::Display for ToolError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidArguments(error) => fmt::Display::fmt(error, formatter),
            Self::PlanIdMismatch { requested, active } => write!(
                formatter,
                "requested plan {requested} does not match active plan {active}"
            ),
            Self::MissingState(message) | Self::Rejected(message) => formatter.write_str(message),
            Self::Json(error) => fmt::Display::fmt(error, formatter),
            Self::Io(error) => fmt::Display::fmt(error, formatter),
            Self::Operation(error) => fmt::Display::fmt(error, formatter),
            Self::Context { message, .. } => formatter.write_str(message),
        }
    }
}

impl Error for ToolError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InvalidArguments(error) => Some(error.as_ref()),
            Self::Json(error) => Some(error),
            Self::Io(error) => Some(error),
            Self::Operation(error) => Some(error.as_ref()),
            Self::Context { source, .. } => Some(source.as_ref()),
            Self::PlanIdMismatch { .. } | Self::MissingState(_) | Self::Rejected(_) => None,
        }
    }
}

impl From<ControlToolArgumentError> for ToolError {
    fn from(error: ControlToolArgumentError) -> Self {
        Self::InvalidArguments(Box::new(error))
    }
}

impl From<serde_json::Error> for ToolError {
    fn from(error: serde_json::Error) -> Self {
        Self::Json(error)
    }
}

impl From<std::io::Error> for ToolError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<std::fmt::Error> for ToolError {
    fn from(error: std::fmt::Error) -> Self {
        Self::Operation(Box::new(error))
    }
}

impl From<anyhow::Error> for ToolError {
    fn from(error: anyhow::Error) -> Self {
        // Preserve downcasting to the domain error used by response classification.
        Self::Operation(error.reallocate_into_boxed_dyn_error_without_backtrace())
    }
}

impl From<crate::checkpoint::CheckpointGateError> for ToolError {
    fn from(error: crate::checkpoint::CheckpointGateError) -> Self {
        Self::Operation(Box::new(error))
    }
}

impl From<crate::broker::TurnCancelled> for ToolError {
    fn from(error: crate::broker::TurnCancelled) -> Self {
        Self::Operation(Box::new(error))
    }
}

/// Add operation context while preserving typed tool errors and domain causes.
pub(in crate::tools) trait ToolContext<T> {
    fn context(self, message: impl Into<String>) -> ToolResult<T>;
    fn with_context(self, message: impl FnOnce() -> String) -> ToolResult<T>;
}

impl<T> ToolContext<T> for Option<T> {
    fn context(self, message: impl Into<String>) -> ToolResult<T> {
        self.ok_or_else(|| ToolError::MissingState(message.into()))
    }
    fn with_context(self, message: impl FnOnce() -> String) -> ToolResult<T> {
        self.ok_or_else(|| ToolError::MissingState(message()))
    }
}

impl<T, E: Into<ToolError>> ToolContext<T> for Result<T, E> {
    fn context(self, message: impl Into<String>) -> ToolResult<T> {
        self.map_err(|error| ToolError::Context {
            message: message.into(),
            source: Box::new(error.into()),
        })
    }
    fn with_context(self, message: impl FnOnce() -> String) -> ToolResult<T> {
        self.map_err(|error| ToolError::Context {
            message: message(),
            source: Box::new(error.into()),
        })
    }
}

macro_rules! rejected {
    ($($argument:tt)*) => { $crate::tools::ToolError::Rejected(format!($($argument)*)) };
}

macro_rules! bail {
    ($($argument:tt)*) => { return Err($crate::tools::error::rejected!($($argument)*).into()) };
}

macro_rules! ensure {
    ($condition:expr, $($argument:tt)*) => {
        if !$condition { $crate::tools::error::bail!($($argument)*); }
    };
}

pub(in crate::tools) use {bail, ensure, rejected};
