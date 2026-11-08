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
