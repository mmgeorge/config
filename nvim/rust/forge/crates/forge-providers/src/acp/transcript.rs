use serde::{Deserialize, de::IgnoredAny};
use std::io::{ErrorKind, SeekFrom};
use std::path::{Path, PathBuf};
use tokio::io::{AsyncBufReadExt, AsyncSeekExt, BufReader};

const REPLY_SEPARATOR: &str = "\n\n";

/// Read the CLI's native transcript without reloading its session or reconnecting tools.
pub(super) struct TranscriptCursor {
    path: PathBuf,
    offset: u64,
}

#[derive(Deserialize)]
struct TranscriptEntry<'line> {
    #[serde(rename = "type")]
    kind: &'line str,
    status: &'line str,
    #[serde(default)]
    content: serde_json::Value,
    tool_calls: Option<Vec<IgnoredAny>>,
}

/// Consecutive completed replies separated by user input, with no intervening tool activity.
#[derive(Default)]
pub(super) struct TranscriptReplies {
    replies: Vec<String>,
    awaiting_reply: bool,
    trailing_error: bool,
    incomplete_record: bool,
}

/// The completed prefix and the remaining replies from one anonymous wire item.
pub(super) struct TranscriptSplit {
    pub(super) first: String,
    pub(super) following: Vec<String>,
    pub(super) last_completed: bool,
}

/// Locate the full transcript beside the abbreviated log reported to Antigravity hooks.
pub(super) fn path(profile: &Path, session_id: &str) -> Option<PathBuf> {
    let id = uuid::Uuid::parse_str(session_id).ok()?;
    Some(
        profile
            .join("antigravity-cli/brain")
            .join(id.hyphenated().to_string())
            .join(".system_generated/logs/transcript_full.jsonl"),
    )
}

impl TranscriptCursor {
    /// Mark the native history visible before this invocation can publish its first input.
    pub(super) async fn capture(path: &Path) -> Option<Self> {
        let offset = match tokio::fs::metadata(path).await {
            Ok(metadata) if metadata.is_file() => metadata.len(),
            Err(error) if error.kind() == ErrorKind::NotFound => 0,
            _ => return None,
        };
        Some(Self {
            path: path.into(),
            offset,
        })
    }

    /// Read complete appended records and record whether the final line is still being written.
    pub(super) async fn replies(self) -> Option<TranscriptReplies> {
        let mut file = tokio::fs::File::open(self.path).await.ok()?;
        if !file.metadata().await.ok()?.is_file() {
            return None;
        }
        file.seek(SeekFrom::Start(self.offset)).await.ok()?;
        let mut reader = BufReader::new(file);
        let mut line = Vec::new();
        let mut history = TranscriptReplies::default();
        let mut started = false;
        while reader.read_until(b'\n', &mut line).await.ok()? != 0 {
            if !line.ends_with(b"\n") {
                history.incomplete_record = true;
                break;
            }
            let entry: TranscriptEntry<'_> = serde_json::from_slice(&line).ok()?;
            if !started
                && !matches!(
                    entry,
                    TranscriptEntry {
                        kind: "USER_INPUT",
                        status: "DONE",
                        content: serde_json::Value::String(_),
                        ..
                    }
                )
            {
                return None;
            }
            started = true;
            history.record(entry);
            line.clear();
        }
        started.then_some(history)
    }
}

impl TranscriptReplies {
    /// Match complete reply bytes and retain any pending steer response as interrupted text.
    pub(super) fn split(self, streamed: &str, interrupted: bool) -> Option<TranscriptSplit> {
        if !interrupted && (self.awaiting_reply || self.trailing_error) {
            return None;
        }
        if self.incomplete_record && !(interrupted && self.awaiting_reply) {
            return None;
        }
        let (index, partial) = match self.exact_suffix(streamed) {
            Some(index) => (index, ""),
            None if self.awaiting_reply => self.interrupted_suffix(streamed)?,
            None => return None,
        };
        let last_completed = !interrupted || (self.awaiting_reply && partial.is_empty());
        let mut matched = self.replies.into_iter().skip(index);
        Some(TranscriptSplit {
            first: matched.next()?,
            following: matched
                .chain((!partial.is_empty()).then(|| partial.to_owned()))
                .collect(),
            last_completed,
        })
    }

    /// Identify a suffix of the recorded replies that accounts for the complete wire item.
    fn exact_suffix(&self, streamed: &str) -> Option<usize> {
        // Bundled ACP puts two line feeds between adjacent native replies in its live stream.
        // Walk backward so an already-separate earlier reply is left alone.
        let mut remainder = streamed;
        for (index, reply) in self.replies.iter().enumerate().rev() {
            remainder = remainder.strip_suffix(reply)?;
            if remainder.is_empty() {
                return (index + 1 < self.replies.len() || self.awaiting_reply).then_some(index);
            }
            remainder = remainder.strip_suffix(REPLY_SEPARATOR)?;
        }
        None
    }

    /// Find a unique completed prefix before the pending user's interrupted response.
    fn interrupted_suffix<'wire>(&self, streamed: &'wire str) -> Option<(usize, &'wire str)> {
        let mut candidates = self.replies.iter().enumerate().filter_map(|(index, _)| {
            let partial = self.replies[index..]
                .iter()
                .try_fold(streamed, |wire, reply| {
                    wire.strip_prefix(reply)?.strip_prefix(REPLY_SEPARATOR)
                })?;
            Some((index, partial))
        });
        let candidate = candidates.next()?;
        // An earlier wire boundary may already have separated replies with identical prefixes.
        candidates.next().is_none().then_some(candidate)
    }

    /// Keep only the latest reply sequence whose input boundaries remain known.
    fn record(&mut self, entry: TranscriptEntry<'_>) {
        if self.trailing_error {
            *self = Self::default();
        }
        let has_tool_calls = entry.tool_calls.is_some_and(|calls| !calls.is_empty());
        match (entry.kind, entry.status, entry.content, has_tool_calls) {
            ("USER_INPUT", "DONE", serde_json::Value::String(_), false) => {
                self.awaiting_reply = true;
            }
            ("PLANNER_RESPONSE", "DONE", serde_json::Value::String(reply), false) => {
                if reply.is_empty() {
                    return;
                }
                if !self.awaiting_reply {
                    self.replies.clear();
                }
                self.replies.push(reply);
                self.awaiting_reply = false;
            }
            // The CLI appends a diagnostic after the interrupted response in the transcript.
            // Only the invocation's native interruption can explain this trailing record.
            ("ERROR_MESSAGE", "DONE", _, _) => self.trailing_error = true,
            // Resume can write an internal system reminder between the input and the reply.
            ("SYSTEM_MESSAGE", "DONE", _, false)
            | ("PLANNER_RESPONSE", "DONE", serde_json::Value::Null, false) => {}
            _ => *self = Self::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// A cursor excludes prior invocations and preserves a trailing prefix without a known boundary.
    #[tokio::test]
    async fn transcript_uses_only_complete_records_after_the_cursor() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("transcript.jsonl");
        std::fs::write(
            &path,
            "{\"type\":\"PLANNER_RESPONSE\",\"status\":\"DONE\",\"content\":\"stale\"}\n",
        )
        .unwrap();
        let cursor = TranscriptCursor::capture(&path).await.unwrap();
        let mut file = std::fs::OpenOptions::new()
            .append(true)
            .open(&path)
            .unwrap();
        for (kind, content) in [("USER_INPUT", "new"), ("PLANNER_RESPONSE", "reply")] {
            writeln!(
                file,
                "{}",
                serde_json::json!({"type":kind,"status":"DONE","content":content})
            )
            .unwrap();
        }
        assert!(
            cursor
                .replies()
                .await
                .unwrap()
                .split("stale\n\nreply", false)
                .is_none()
        );

        let cursor = TranscriptCursor::capture(&path).await.unwrap();
        write!(file, "{{\"type\":\"USER_INPUT\"").unwrap();
        assert!(cursor.replies().await.is_none());
    }

    /// A torn final line can establish no boundary, even when it ends inside a UTF-8 character.
    #[tokio::test]
    async fn interrupted_split_keeps_the_completed_prefix_before_torn_utf8() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("transcript_full.jsonl");
        let completed_cursor = TranscriptCursor::capture(&path).await.unwrap();
        let interrupted_cursor = TranscriptCursor::capture(&path).await.unwrap();
        let mut file = std::fs::File::create(&path).unwrap();
        for (kind, content) in [
            ("USER_INPUT", "original"),
            ("PLANNER_RESPONSE", "完了"),
            ("USER_INPUT", "steer"),
        ] {
            write!(
                file,
                "{}\r\n",
                serde_json::json!({"type":kind,"status":"DONE","content":content})
            )
            .unwrap();
        }
        file.write_all(
            b"{\"type\":\"PLANNER_RESPONSE\",\"status\":\"DONE\",\"content\":\"\xe7\xb6",
        )
        .unwrap();
        let streamed = "完了\n\n継続\n\n…";
        assert!(
            completed_cursor
                .replies()
                .await
                .unwrap()
                .split(streamed, false)
                .is_none()
        );
        let split = interrupted_cursor
            .replies()
            .await
            .unwrap()
            .split(streamed, true)
            .unwrap();
        assert_eq!(split.first, "完了");
        assert_eq!(split.following, ["継続\n\n…"]);
        assert!(!split.last_completed);
    }

    /// Several alternating steers can split the current wire item even if an earlier item is separate.
    #[test]
    fn exact_split_keeps_each_reply_in_a_multi_steer_suffix() {
        let replies = TranscriptReplies {
            replies: ["separate", "first", "second", "third"]
                .map(String::from)
                .to_vec(),
            ..Default::default()
        };
        let split = replies.split("first\n\nsecond\n\nthird", false).unwrap();
        assert_eq!(split.first, "first");
        assert_eq!(split.following, ["second", "third"]);
        assert!(split.last_completed);
    }

    /// Repeated reply text cannot identify where a wire item's unrecorded suffix begins.
    #[test]
    fn interrupted_split_leaves_an_ambiguous_prefix_intact() {
        let replies = TranscriptReplies {
            replies: vec!["same".into(), "same".into()],
            awaiting_reply: true,
            ..Default::default()
        };
        assert!(replies.split("same\n\nsame\n\npartial", true).is_none());
    }
}
