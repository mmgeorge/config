use super::AcpConnection;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{
    FailureDisposition, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    ProviderPhase, RecoveryToken,
};
use serde_json::Value;
use std::sync::Arc;

/// Select the ACP v1 operation whose response provides delivery evidence.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AcpSubmission {
    Prompt,
    Steer,
}

impl AcpSubmission {
    /// Get the semantic operation for every receipt and native response belonging to this send.
    fn operation(self) -> ProviderOperation {
        match self {
            Self::Prompt => ProviderOperation::TurnStart,
            Self::Steer => ProviderOperation::TurnSteer,
        }
    }

    fn method(self) -> &'static str {
        match self {
            Self::Prompt | Self::Steer => "session/prompt",
        }
    }
}

impl AcpConnection {
    /// Commit ACP's provider-owned delivery evidence around one native send.
    pub(super) async fn submit(
        &self,
        kind: AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn InputReceipt>>,
        dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
        cancellation: Option<&super::connection::PromptCancellation>,
    ) -> Result<super::response::NativePromptEnd, ProviderError> {
        let method = kind.method();
        let operation = kind.operation();
        let session_id = parameter["sessionId"]
            .as_str()
            .map(str::to_owned)
            .ok_or_else(|| failure(operation, ProviderFailureKind::Rejected))?;
        let sending_checkpoint = receipt
            .as_ref()
            .map(|receipt| checkpoint(false, method, &session_id, receipt.as_ref(), operation))
            .transpose()?;
        let accepted_checkpoint = receipt
            .as_ref()
            .map(|receipt| checkpoint(true, method, &session_id, receipt.as_ref(), operation))
            .transpose()?;
        if let Some(receipt) = &receipt {
            receipt
                .clone()
                .record_sending(sending_checkpoint)
                .await
                .map_err(|error| error.for_operation(operation))?;
        }
        let response = match self
            .request_prompt(method, parameter, &session_id, dispatch, cancellation)
            .await
        {
            Ok(Some(native_end)) => Ok(native_end),
            Ok(None) => {
                if kind == AcpSubmission::Prompt {
                    return Err(match receipt {
                        Some(receipt) => {
                            reject_before_dispatch(&receipt, ProviderError::Cancelled).await
                        }
                        None => ProviderError::Cancelled,
                    });
                }
                if let Some(receipt) = receipt {
                    receipt.record_rejected(ProviderError::NotConsumed).await?;
                }
                return Err(ProviderError::NotConsumed);
            }
            Err(error) => Err(crate::error::acp(error, operation)),
        };
        let result: Result<super::response::NativePromptEnd, ProviderError> = async {
            let native_end = response?;
            if let Some(receipt) = &receipt {
                self.retain_prompt_end(
                    &forge_provider_api::CoveredInput::new(
                        receipt.id().clone(),
                        receipt.attempt_id()?,
                    ),
                    native_end,
                )?;
            }
            if let Some(receipt) = &receipt {
                receipt
                    .clone()
                    .record_accepted(accepted_checkpoint)
                    .await
                    .map_err(|error| match error {
                        ProviderError::Failure(failure) => {
                            ProviderError::Failure(failure.for_operation(operation).at_phase(
                                ProviderPhase::Persistence,
                                FailureDisposition::Uncertain,
                            ))
                        }
                        error => ProviderError::Combined {
                            primary: Box::new(
                                ProviderFailure::new(operation, ProviderFailureKind::Unknown)
                                    .at_phase(
                                        ProviderPhase::Persistence,
                                        FailureDisposition::Uncertain,
                                    )
                                    .into(),
                            ),
                            secondary: vec![error],
                        },
                    })?;
            }
            Ok(native_end)
        }
        .await;
        if let (Err(error), Some(receipt)) = (&result, receipt) {
            let record_result = receipt
                .record_failure(error.clone().for_operation(operation))
                .await;
            if let Err(record_error) = record_result {
                return Err(ProviderError::Combined {
                    primary: Box::new(error.clone()),
                    secondary: vec![record_error],
                });
            }
        }
        result
    }
}

/// Record positive local no-send evidence while retaining an independent journal failure.
pub(super) async fn reject_before_dispatch(
    receipt: &Arc<dyn InputReceipt>,
    cause: ProviderError,
) -> ProviderError {
    match receipt.clone().record_rejected(cause.clone()).await {
        Ok(()) => ProviderError::PreparationFailed(Box::new(cause)),
        Err(cleanup) => ProviderError::PreparationCleanup {
            cause: Box::new(cause),
            cleanup: Box::new(cleanup),
        },
    }
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Scope opaque evidence to the exact logical input and immutable delivery attempt.
fn checkpoint(
    accepted: bool,
    method: &str,
    session_id: &str,
    receipt: &dyn InputReceipt,
    operation: ProviderOperation,
) -> Result<RecoveryToken, ProviderError> {
    let attempt = receipt.attempt_id()?;
    let bytes = serde_json::to_vec(&serde_json::json!({
        "version":2,"accepted":accepted,"method":method,"session":session_id,
        "submission":receipt.id().as_str(),"attempt":attempt.as_str(),
    }))
    .map_err(|_| failure(operation, ProviderFailureKind::Protocol))?;
    RecoveryToken::new(bytes).map_err(Into::into)
}
