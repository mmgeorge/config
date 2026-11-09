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
}

impl AcpSubmission {
    /// Get the semantic operation for every receipt and native response belonging to this send.
    fn operation(self) -> ProviderOperation {
        match self {
            Self::Prompt => ProviderOperation::TurnStart,
        }
    }

    fn method(self) -> &'static str {
        match self {
            Self::Prompt => "session/prompt",
        }
    }
}

impl AcpConnection {
    /// Commit ACP's provider-owned delivery evidence around one native send.
    pub async fn submit(
        &self,
        kind: AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn InputReceipt>>,
        dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
        cancellation: Option<&tokio_util::sync::CancellationToken>,
    ) -> Result<Value, ProviderError> {
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
        let mut rejected_without_consumption = false;
        let result = async {
            let response = self
                .request_tracked(method, parameter, dispatch, cancellation)
                .await
                .map_err(|error| crate::error::acp(error, operation))?;
            if kind == AcpSubmission::Prompt {
                // ACP v1 acknowledges a prompt only after the native turn has ended.
                let native_end = super::response::NativePromptEnd::parse(
                    &response,
                    super::children::timestamp(),
                )
                .map_err(|_| {
                    ProviderFailure::new(operation, ProviderFailureKind::Protocol)
                        .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain)
                })?;
                if let Some(receipt) = &receipt {
                    self.retain_prompt_end(
                        &forge_provider_api::CoveredInput::new(
                            receipt.id().clone(),
                            receipt.attempt_id()?,
                        ),
                        native_end,
                    )?;
                }
            } else {
                match response["outcome"].as_str() {
                    Some("injected" | "startedNewTurn") => {}
                    Some("promptRequired")
                        if response["reason"].as_str() == Some("noRunningTurn") =>
                    {
                        rejected_without_consumption = true;
                        let cause = ProviderError::PreparationFailed(Box::new(
                            ProviderFailure::new(operation, ProviderFailureKind::Rejected)
                                .at_phase(ProviderPhase::Observation, FailureDisposition::Unchanged)
                                .into(),
                        ));
                        if let Some(receipt) = &receipt
                            && let Err(record_error) =
                                receipt.clone().record_rejected(cause.clone()).await
                        {
                            return Err(ProviderError::Combined {
                                primary: Box::new(cause),
                                secondary: vec![record_error],
                            });
                        }
                        return Err(cause);
                    }
                    _ => {
                        return Err(
                            ProviderFailure::new(operation, ProviderFailureKind::Protocol)
                                .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain)
                                .into(),
                        );
                    }
                }
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
            Ok(response)
        }
        .await;
        if let (Err(error), Some(receipt)) = (&result, receipt)
            && !rejected_without_consumption
        {
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
