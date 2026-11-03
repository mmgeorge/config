use forge_provider_api::input::{QuestionReply, QuestionReplyResponse};
use serde_json::json;

/// Render a validated structured question response as Codex conversation input.
pub(super) fn render(reply: &QuestionReply) -> String {
    match &reply.response {
        QuestionReplyResponse::Answer {
            answer,
            required_questions_remain,
        } => {
            let question_end = reply
                .question
                .floor_char_boundary(reply.question.len().min(512));
            let replies = json!([{
                "answer": answer,
                "question": reply.question[..question_end].replace(['\n', '\r'], " "),
                "questionItemId": reply.question_id,
            }]);
            let wait = if *required_questions_remain {
                "\nOther required questions remain unanswered. Record this answer without resuming the original request. Wait for the remaining answers."
            } else {
                ""
            };
            format!(
                "<send_user_message_question_reply>\n{replies}\n</send_user_message_question_reply>{wait}"
            )
        }
        QuestionReplyResponse::Correction {
            answer,
            required_questions_remain,
        } => {
            let question_end = reply
                .question
                .floor_char_boundary(reply.question.len().min(512));
            let replies = json!([{
                "answer": answer,
                "question": reply.question[..question_end].replace(['\n', '\r'], " "),
                "questionItemId": reply.question_id,
            }]);
            let wait = if *required_questions_remain {
                "\nOther required questions remain unanswered. Record this correction without resuming the original request. Wait for the remaining answers."
            } else {
                ""
            };
            format!(
                "<send_user_message_question_reply>\n{replies}\n</send_user_message_question_reply>{wait}"
            )
        }
        QuestionReplyResponse::Clarification {
            options,
            text,
            reply_to,
        } => {
            let target = json!({
                "question_id": reply.question_id,
                "revision": reply.revision,
                "message_id": reply.message_id,
                "reply_to": reply_to,
            });
            format!(
                "Harness question clarification\n{}\nChoices: {}\nUser clarification: {}\nReply target: {}\nThis is a read-only question discussion. Explain the choices and provide illustrative examples when requested, without selecting or recording an answer. Call question_reply with these exact target fields and your explanation in text. Copy the complete opaque question_id, including its colon and index suffix. Leave the question unanswered. Honor any explicit constraint in the user's text. Do not implement or resume the original task; Harness owns its continuation.",
                reply.question,
                serde_json::to_string(options).expect("question options serialize as JSON"),
                text,
                target,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::render;
    use forge_provider_api::input::{QuestionReply, QuestionReplyOption, QuestionReplyResponse};

    #[test]
    fn answer_uses_codex_reply_envelope_and_waits_for_other_required_questions() {
        let reply = QuestionReply {
            question_id: "question:3".into(),
            revision: 2,
            message_id: "host-message".into(),
            question: "Pick a mode\ncarefully".into(),
            response: QuestionReplyResponse::Answer {
                answer: "Careful mode".into(),
                required_questions_remain: true,
            },
        };

        let rendered = render(&reply);

        assert!(rendered.starts_with("<send_user_message_question_reply>\n"));
        assert!(rendered.contains("\"questionItemId\":\"question:3\""));
        assert!(rendered.contains("Record this answer without resuming"));
    }

    #[test]
    fn clarification_keeps_the_opaque_target_and_never_resolves_the_question() {
        let reply = QuestionReply {
            question_id: "question:opaque:4".into(),
            revision: 7,
            message_id: "clarification-message".into(),
            question: "Which option?".into(),
            response: QuestionReplyResponse::Clarification {
                options: vec![
                    QuestionReplyOption {
                        id: "one".into(),
                        label: "One".into(),
                        description: "First option".into(),
                        recommended: true,
                    },
                    QuestionReplyOption {
                        id: "two".into(),
                        label: "Two".into(),
                        description: "Second option".into(),
                        recommended: false,
                    },
                ],
                text: "Explain the tradeoff".into(),
                reply_to: Some("prior-clarification".into()),
            },
        };

        let rendered = render(&reply);

        assert!(rendered.contains("question:opaque:4"));
        assert!(rendered.contains("prior-clarification"));
        assert!(rendered.contains("First option"));
        assert!(rendered.contains("recommended"));
        assert!(rendered.contains("read-only question discussion"));
        assert!(!rendered.contains("<send_user_message_question_reply>"));
    }

    #[test]
    fn correction_waits_when_required_questions_remain() {
        let reply = QuestionReply {
            question_id: "question:3".into(),
            revision: 3,
            message_id: "correction-message".into(),
            question: "Pick a mode".into(),
            response: QuestionReplyResponse::Correction {
                answer: "Careful mode".into(),
                required_questions_remain: true,
            },
        };

        let rendered = render(&reply);

        assert!(rendered.starts_with("<send_user_message_question_reply>\n"));
        assert!(rendered.contains("Record this correction without resuming"));
    }
}
