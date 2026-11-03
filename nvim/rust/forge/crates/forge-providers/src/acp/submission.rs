use super::AcpConnection;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation, RecoveryToken,
};
use serde_json::Value;
use std::sync::Arc;
use uuid::Uuid;

/// Select the ACP v1 operation whose response provides delivery evidence.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AcpSubmission {
    Prompt,
    Steering,
}

/// Distinguish steering accepted for the active turn from a new unobservable turn.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SteeringAcceptance {
    Injected,
    UnobservableExecution,
}

impl AcpSubmission {
    fn method(self) -> &'static str {
        match self {
            Self::Prompt => "session/prompt",
            Self::Steering => "_session/steering",
        }
    }
}

impl AcpConnection {
    /// Decode a steering response after its accepted delivery receipt is committed.
    pub async fn submit_steering(
        &self,
        parameter: Value,
        receipt: Option<Arc<dyn InputReceipt>>,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let response = self
            .submit(AcpSubmission::Steering, parameter, receipt)
            .await?;
        match response["outcome"].as_str() {
            Some("injected") => Ok(SteeringAcceptance::Injected),
            Some("startedNewTurn") => Ok(SteeringAcceptance::UnobservableExecution),
            _ => Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Protocol,
            )),
        }
    }

    /// Commit ACP's provider-owned delivery evidence around one native send.
    pub async fn submit(
        &self,
        kind: AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn InputReceipt>>,
    ) -> Result<Value, ProviderError> {
        let method = kind.method();
        let session_id = parameter["sessionId"]
            .as_str()
            .map(str::to_owned)
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        let checkpoint = RecoveryToken::new(format!("v1\n{method}\n{session_id}").into_bytes())
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))?;
        if let Some(receipt) = &receipt {
            receipt.clone().record_sending(Some(checkpoint)).await?;
        }
        let mut rejected_without_consumption = false;
        let result = async {
            let response = self.request(method, parameter).await?;
            if kind == AcpSubmission::Prompt {
                // ACP v1 acknowledges a prompt only after the native turn has ended.
                super::prompt_outcome(&response).map_err(|_| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                })?;
            } else {
                match response["outcome"].as_str() {
                    Some("injected" | "startedNewTurn") => {}
                    Some("promptRequired")
                        if response["reason"].as_str() == Some("noRunningTurn") =>
                    {
                        rejected_without_consumption = true;
                        let continuation = forge_provider_api::session::ContinuationToken::new(
                            Uuid::new_v4().as_bytes().to_vec(),
                        )
                        .map_err(|_| {
                            failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                        })?;
                        if let Some(receipt) = &receipt {
                            receipt
                                .clone()
                                .record_rejected(failure(
                                    ProviderOperation::AdmitInput,
                                    ProviderFailureKind::Rejected,
                                ))
                                .await?;
                        }
                        return Err(ProviderError::NotConsumed { continuation });
                    }
                    _ => {
                        return Err(failure(
                            ProviderOperation::AdmitInput,
                            ProviderFailureKind::Protocol,
                        ));
                    }
                }
            }
            if let Some(receipt) = &receipt {
                let accepted =
                    RecoveryToken::new(format!("accepted\n{method}\n{session_id}").into_bytes())
                        .map_err(|_| {
                            failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                        })?;
                receipt.clone().record_accepted(Some(accepted)).await?;
            }
            Ok(response)
        }
        .await;
        if let (Err(error), Some(receipt)) = (&result, receipt)
            && !rejected_without_consumption
        {
            let record_result = receipt
                .record_failure(ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::AdmitInput,
                    match error {
                        ProviderError::Failure(failure) => failure.kind(),
                        ProviderError::Cancelled => ProviderFailureKind::Runtime,
                        ProviderError::DeliveryUncertain => ProviderFailureKind::Transport,
                        _ => ProviderFailureKind::Unknown,
                    },
                )))
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
