use super::{
    super::{
        AdapterError, TestItemId,
        events::{CaseStatus, TestEvent, TestFailure},
    },
    workspace::relative,
};
use serde::Deserialize;
use std::{
    collections::{BTreeMap, BTreeSet},
    path::Path,
};

/// Map native wire names to validated cases without allowing duplicate terminal outcomes.
pub(super) struct PlanIndex {
    pub names: BTreeMap<String, TestItemId>,
    pub ignored: BTreeSet<String>,
    finished: BTreeSet<TestItemId>,
}

#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum Record {
    Suite {
        event: SuiteEvent,
    },
    Test {
        name: String,
        #[serde(flatten)]
        event: TestOutcome,
    },
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum SuiteEvent {
    Started,
    Ok,
    Failed,
}

#[derive(Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
enum TestOutcome {
    Started,
    Ok {
        exec_time: Option<f64>,
        #[serde(default)]
        stdout: String,
    },
    Failed {
        exec_time: Option<f64>,
        #[serde(default)]
        stdout: String,
        reason: Option<String>,
    },
    Ignored,
}

impl PlanIndex {
    /// Create the execution index from selected cases and known ignored wire names.
    pub(super) fn new(names: BTreeMap<String, TestItemId>, ignored: BTreeSet<String>) -> Self {
        Self {
            names,
            ignored,
            finished: BTreeSet::new(),
        }
    }

    /// Translate one complete native JSON record into category events.
    pub(super) fn translate(
        &mut self,
        line: &str,
        workspace: &Path,
        repository: &Path,
    ) -> Result<Vec<TestEvent>, AdapterError> {
        let record: Record = serde_json::from_str(line)
            .map_err(|error| AdapterError::Protocol(format!("nextest event: {error}")))?;
        let Record::Test { name, event } = record else {
            let Record::Suite { event } = record else {
                unreachable!()
            };
            match event {
                SuiteEvent::Started | SuiteEvent::Ok | SuiteEvent::Failed => {}
            }
            return Ok(Vec::new());
        };
        let Some(id) = self.names.get(&name) else {
            if self.ignored.contains(&name)
                && matches!(event, TestOutcome::Started | TestOutcome::Ignored)
            {
                return Ok(Vec::new());
            }
            return Err(AdapterError::Protocol(format!(
                "nextest reported unplanned test {name:?}"
            )));
        };
        if matches!(event, TestOutcome::Started) {
            return Ok(vec![TestEvent::Started { id: id.clone() }]);
        }
        let (status, seconds, output, reason) = match event {
            TestOutcome::Started => unreachable!(),
            TestOutcome::Ok { exec_time, stdout } => (CaseStatus::Passed, exec_time, stdout, None),
            TestOutcome::Failed {
                exec_time,
                stdout,
                reason,
            } => {
                let status = if reason.as_deref() == Some("time limit exceeded") {
                    CaseStatus::TimedOut
                } else {
                    CaseStatus::Failed
                };
                (status, exec_time, stdout, reason)
            }
            TestOutcome::Ignored => (CaseStatus::Skipped, None, String::new(), None),
        };
        let duration_ms = seconds
            .map(|seconds| {
                if !seconds.is_finite() || seconds < 0.0 {
                    return Err(AdapterError::Protocol(
                        "invalid nextest test duration".into(),
                    ));
                }
                let duration = std::time::Duration::try_from_secs_f64(seconds)
                    .map_err(|_| AdapterError::Protocol("nextest test duration overflow".into()))?;
                u64::try_from(duration.as_millis())
                    .map_err(|_| AdapterError::Protocol("nextest test duration overflow".into()))
            })
            .transpose()?;
        if !self.finished.insert(id.clone()) {
            return Err(AdapterError::Protocol(format!(
                "duplicate nextest outcome for {name:?}"
            )));
        }
        let mut failures = panic_locations(&output, workspace, repository);
        if matches!(status, CaseStatus::Failed | CaseStatus::TimedOut) && failures.is_empty() {
            failures.push(TestFailure {
                message: reason.unwrap_or_else(|| "test failed; inspect retained output".into()),
                path: None,
                line: None,
            });
        }
        let mut events = Vec::new();
        if !output.is_empty() {
            events.push(TestEvent::Output {
                id: Some(id.clone()),
                text: output,
            });
        }
        events.push(TestEvent::Finished {
            id: id.clone(),
            status,
            duration_ms,
            failures,
        });
        Ok(events)
    }

    /// Prove every selected case received a terminal outcome before emitting completion.
    pub(super) fn complete(&self) -> bool {
        self.finished.len() == self.names.len()
    }
}

/// Extract modern panic locations while retaining external paths as unnavigable text.
fn panic_locations(output: &str, workspace: &Path, repository: &Path) -> Vec<TestFailure> {
    let pattern = regex::Regex::new(r"panicked at ([^\r\n]+):(\d+):(\d+):")
        .expect("constant panic location pattern");
    pattern
        .captures_iter(output)
        .map(|capture| {
            let normalized = capture[1].replace('\\', "/");
            let raw = Path::new(&normalized);
            let absolute = if raw.is_absolute() {
                raw.to_owned()
            } else {
                workspace.join(raw)
            };
            let path = relative(&absolute, repository)
                .filter(|path| super::super::contract::validate_path(path).is_ok());
            let line = path
                .as_ref()
                .and_then(|_| capture[2].parse::<u32>().ok())
                .filter(|line| *line > 0);
            TestFailure {
                message: capture[0].to_owned(),
                path: path.filter(|_| line.is_some()),
                line,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Translate recorded native events using the same eligible inventory as exact discovery.
    #[test]
    fn recorded_list_and_run_agree_on_eligible_cases_and_outcomes() {
        let list =
            nextest_metadata::TestListSummary::parse_json(include_str!("fixtures/list.stdout"))
                .unwrap();
        let suite = list.rust_suites.values().next().unwrap();
        assert_eq!(suite.test_cases.len(), 5);
        let mut names = BTreeMap::new();
        let mut ignored = BTreeSet::new();
        for (name, case) in &suite.test_cases {
            let wire = format!(
                "{}::{}${}",
                suite.package_name,
                suite.binary.binary_name,
                name.as_str()
            );
            if case.filter_match.is_match() {
                names.insert(
                    wire,
                    TestItemId::try_from(name.as_str().to_owned()).unwrap(),
                );
            } else if case.ignored {
                ignored.insert(wire);
            } else {
                assert_eq!(name.as_str(), "excluded");
            }
        }
        assert_eq!(names.len(), 3);
        assert_eq!(ignored.len(), 1);
        let mut plan = PlanIndex::new(names, ignored);
        let mut outcomes = BTreeMap::new();
        let mut failure_location = None;
        let mut failure_output = false;
        for line in include_str!("fixtures/run.stdout").lines() {
            for event in plan
                .translate(line, Path::new("/repo"), Path::new("/repo"))
                .unwrap()
            {
                match event {
                    TestEvent::Finished {
                        id,
                        status,
                        duration_ms,
                        failures,
                    } => {
                        assert!(duration_ms.is_some_and(|duration| duration > 0));
                        if id.as_str() == "fails" {
                            failure_location = failures
                                .first()
                                .map(|failure| (failure.path.clone(), failure.line));
                        }
                        assert!(outcomes.insert(id.as_str().to_owned(), status).is_none());
                    }
                    TestEvent::Output { text, .. } => {
                        failure_output |= text.contains("fixture failure")
                    }
                    TestEvent::Started { .. } => {}
                    event => panic!("unexpected translated event: {event:?}"),
                }
            }
        }
        assert!(plan.complete());
        assert!(failure_output);
        assert_eq!(failure_location, Some((Some("src/lib.rs".into()), Some(5))));
        assert_eq!(
            outcomes,
            BTreeMap::from([
                ("fails".into(), CaseStatus::Failed),
                ("passes".into(), CaseStatus::Passed),
                ("times_out".into(), CaseStatus::TimedOut),
            ])
        );
        assert!(!include_str!("fixtures/run.stderr").trim().is_empty());
    }

    /// Distinguish timeouts and preserve output and source location without accepting retries.
    #[test]
    fn failures_preserve_identity_output_and_timeout() {
        let id: TestItemId = "pkg tests::fails".to_owned().try_into().unwrap();
        let mut plan = PlanIndex::new(
            BTreeMap::from([("pkg::pkg$tests::fails".into(), id.clone())]),
            BTreeSet::new(),
        );
        let output = r#"{"type":"test","event":"failed","name":"pkg::pkg$tests::fails","exec_time":0.25,"stdout":"thread panicked at src/lib.rs:12:3:\nassertion failed"}"#;
        let events = plan
            .translate(output, Path::new("/repo"), Path::new("/repo"))
            .unwrap();
        assert!(
            matches!(&events[1], TestEvent::Finished { status: CaseStatus::Failed, duration_ms: Some(250), failures, .. } if failures[0].path.as_deref() == Some("src/lib.rs") && failures[0].line == Some(12))
        );
        assert!(plan.complete());
        assert!(
            plan.translate(output, Path::new("/repo"), Path::new("/repo"))
                .is_err()
        );
        let mut plan = PlanIndex::new(
            BTreeMap::from([("pkg::pkg$tests::fails".into(), id)]),
            BTreeSet::new(),
        );
        let events = plan.translate(r#"{"type":"test","event":"failed","name":"pkg::pkg$tests::fails","reason":"time limit exceeded"}"#, Path::new("/repo"), Path::new("/repo")).unwrap();
        assert!(matches!(
            events[0],
            TestEvent::Finished {
                status: CaseStatus::TimedOut,
                ..
            }
        ));
    }
}
