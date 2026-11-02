use super::AcpConnection;
use forge_provider_api::{
    ProviderError,
    submission::{InputReceipt, SubmissionTarget},
};
use serde_json::Value;
use std::sync::Arc;

/// Select the ACP operation whose response can establish delivery evidence.
#[derive(Clone, Copy)]
pub enum AcpSubmission {
    Prompt,
    Steering,
}

/// Distinguish confirmed injection from accepted work without an observable terminal signal.
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

impl TryFrom<&str> for AcpSubmission {
    type Error = ProviderError;
    fn try_from(method: &str) -> Result<Self, Self::Error> {
        match method {
            "session/prompt" => Ok(Self::Prompt),
            "_session/steering" => Ok(Self::Steering),
            _ => Err(ProviderError::Provider {
                code: None,
                message: format!("ACP submission method is unsupported: {method}"),
            }),
        }
    }
}

impl std::str::FromStr for AcpSubmission {
    type Err = ProviderError;

    fn from_str(method: &str) -> Result<Self, Self::Err> {
        Self::try_from(method)
    }
}

impl AcpConnection {
    /// Decode steering outcomes only after recording native acceptance evidence.
    pub async fn submit_steering(
        &self,
        parameter: Value,
        submission: Option<Arc<dyn InputReceipt>>,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let response = self
            .submit(AcpSubmission::Steering, parameter, submission)
            .await?;
        match response["outcome"].as_str() {
            Some("injected") => Ok(SteeringAcceptance::Injected),
            Some("startedNewTurn") => Ok(SteeringAcceptance::UnobservableExecution),
            _ => Err(ProviderError::Protocol {
                message: "ACP accepted an unsupported steering outcome".into(),
            }),
        }
    }

    /// Commits delivery evidence around an ACP prompt or advertised steering request.
    /// Cancellation after dispatch leaves the input uncertain. Only a valid native
    /// response permits acceptance or the documented idle-steering fallback.
    pub async fn submit(
        &self,
        kind: AcpSubmission,
        parameter: Value,
        submission: Option<Arc<dyn InputReceipt>>,
    ) -> Result<Value, ProviderError> {
        let method = kind.method();
        let result: Result<Value, ProviderError> = async {
            let session_id = parameter["sessionId"]
                .as_str()
                .ok_or_else(|| ProviderError::Provider {
                    code: None,
                    message: "ACP submission has no session ID".into(),
                })?
                .to_owned();
            if let Some(submission) = submission.as_ref() {
                let target = SubmissionTarget::native(
                    "acp",
                    session_id.clone(),
                    serde_json::json!({"method":method}),
                );
                submission.clone().record_sending(target).await?;
            }
            let response = self.request(method, parameter).await?;
            if method == "session/prompt" {
                // ACP v1 responds when the turn ends, not when input is accepted.
                // Preserve uncertainty on disconnect. The v2 draft returns an acceptance
                // messageId, enabling earlier receipts once both adapters support it:
                // https://agentclientprotocol.com/protocol/v2/prompt-lifecycle
                super::validate_prompt_response(response.clone()).map_err(|error| {
                    ProviderError::Protocol {
                        message: format!(
                            "ACP prompt response does not confirm a completed turn: {error}"
                        ),
                    }
                })?;
            } else {
                match response["outcome"].as_str() {
                    Some("injected" | "startedNewTurn") => {}
                    Some("promptRequired")
                        if response["reason"].as_str() == Some("noRunningTurn") =>
                    {
                        let error =
                            "Active work finished before steering. Queue this prompt instead";
                        // Claude's opted-in idle response guarantees no input was queued.
                        // Other errors and unknown outcomes never authorize another send.
                        if let Some(submission) = submission.as_ref() {
                            submission.clone().record_rejected(error.into()).await?;
                        }
                        return Err(ProviderError::NotConsumed {
                            message: error.into(),
                        });
                    }
                    _ => {
                        return Err(ProviderError::Protocol {
                            message: format!("ACP steering acceptance is unknown: {response}"),
                        });
                    }
                }
            }
            if let Some(submission) = submission.as_ref() {
                let receipt = format!("{method}:{session_id}");
                submission.clone().record_accepted(receipt).await?;
            }
            Ok(response)
        }
        .await;
        if let Err(error) = &result
            && let Some(submission) = submission
        {
            let failure = format!("{error:#}");
            submission.clone().record_failure(failure).await?;
        }
        result
    }
}
