use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TaskStart {
    pub kind: crate::task::TaskKind,
    pub title: String,
}

impl TaskStart {
    pub fn heading(&self) -> String {
        let label = match self.kind {
            crate::task::TaskKind::Goal => "Goal",
            crate::task::TaskKind::Plan => "Plan",
            crate::task::TaskKind::Execute => "Execution",
        };
        format!("{label} started: {}", self.title)
    }
}

/// Identifies who requested an exchange without retaining task ownership.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum ExchangeOrigin {
    #[default]
    User,
    TaskStarted {
        start: TaskStart,
    },
    TaskContinuation {
        reason: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task_start_origin_retains_presentation_without_task_authority() {
        let origin = ExchangeOrigin::TaskStarted {
            start: TaskStart {
                kind: crate::task::TaskKind::Goal,
                title: "Reduce startup time".into(),
            },
        };
        let encoded = serde_json::to_value(&origin).unwrap();
        assert_eq!(
            encoded,
            serde_json::json!({"kind":"task_started","start":{"kind":"goal","title":"Reduce startup time"}})
        );
        assert_eq!(
            serde_json::from_value::<ExchangeOrigin>(encoded).unwrap(),
            origin
        );
        assert!(serde_json::from_value::<ExchangeOrigin>(serde_json::json!({
            "kind":"task_started","start":{"kind":"goal","title":"Reduce startup time","revision":"stale"}
        })).is_err());
    }

    #[test]
    fn continuation_origin_contains_no_task_reference() {
        let origin = ExchangeOrigin::task_continuation("Validation remains");
        let encoded = serde_json::to_value(&origin).unwrap();
        assert_eq!(
            encoded,
            serde_json::json!({"kind":"task_continuation","reason":"Validation remains"})
        );
        assert_eq!(
            serde_json::from_value::<ExchangeOrigin>(encoded).unwrap(),
            origin
        );
        assert!(
            serde_json::from_value::<ExchangeOrigin>(
                serde_json::json!({"kind":"task_continuation","reason":"test","task_id":"old"})
            )
            .is_err()
        );
    }
}

impl ExchangeOrigin {
    pub fn task_continuation(reason: impl Into<String>) -> Self {
        Self::TaskContinuation {
            reason: reason.into(),
        }
    }
}
