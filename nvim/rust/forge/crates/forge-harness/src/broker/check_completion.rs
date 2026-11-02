//! Admit explicit completion checks and their automatic repair exchanges.

use super::*;

impl HarnessBroker {
    /// Start explicit checks with their own publication lifetime.
    pub(crate) fn begin_check_completion(
        &mut self,
        sink: publication::SessionUpdateSink,
    ) -> Result<(
        Option<check_admission::AutomaticCheckTransaction>,
        AutomaticCheckAction,
    )> {
        self.require_restore_recovered()?;
        self.with_publication_sink(&sink, |broker| {
            let result = (serde_json::to_value(broker.snapshot()?)?, Vec::new());
            broker.begin_automatic_checks(result, PromptMode::Chat)
        })
    }

    /// Admit a later automatic repair as a new exchange under the original change owner.
    pub(crate) fn begin_check_repair(
        &mut self,
        request_id: u64,
        prompt: String,
        change_owner: String,
        sink: publication::SessionUpdateSink,
    ) -> Result<ForegroundDispatch> {
        let scoped = sink.clone();
        self.with_publication_sink(&scoped, |broker| {
            let mut dispatch = broker.begin_foreground_dispatch(
                Request {
                    id: request_id,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":prompt}),
                },
                sink,
            )?;
            let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
                unreachable!("new foreground dispatch is admitting");
            };
            let mut exchange_admission = ExchangeAdmission::chat(String::new());
            exchange_admission.check_owner = Some(change_owner.clone());
            exchange_admission.automatic_owner = Some(change_owner.clone());
            admission.staged_interaction = Some(broker.stage_foreground_interaction(
                &prompt,
                PromptMode::Chat,
                Some(&exchange_admission),
                ProviderInput::from_text(prompt.clone()),
            )?);
            admission.automatic_check_owner = Some(change_owner);
            Ok(dispatch)
        })
    }

    /// Release check publication ownership and preserve the completed response or typed failure.
    pub(crate) fn finish_check_completion(
        &mut self,
        request_id: u64,
        result: Result<DispatchResult>,
    ) -> DispatchResult {
        match result {
            Ok(result) => result,
            Err(error) => DispatchResult {
                response: Response::failure(
                    request_id,
                    if error.is::<TurnCancelled>() {
                        "turn_cancelled"
                    } else {
                        "request_failed"
                    },
                    crate::storage::error_message(&error),
                ),
                event: Vec::new(),
            },
        }
    }
}
