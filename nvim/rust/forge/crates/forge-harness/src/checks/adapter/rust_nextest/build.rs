use super::super::{
    AdapterError,
    events::{TestEvent, TestEventSink, TestPhase},
};
use super::workspace::WorkspaceModel;
use serde::Deserialize;

/// Identify Cargo records without interpreting nextest's inventory or test events.
#[derive(Deserialize)]
struct Envelope<'source> {
    /// Cargo discriminator, absent from nextest inventory records.
    reason: Option<&'source str>,
    /// Nextest discriminator, including failed tests that also carry a reason.
    #[serde(rename = "type")]
    record_type: Option<&'source str>,
}

/// Decode the Cargo build records that affect live progress.
#[derive(Deserialize)]
#[serde(tag = "reason", rename_all = "kebab-case")]
enum BuildRecord {
    /// An artifact is available, either newly compiled or reused from the cache.
    CompilerArtifact {
        /// Cargo package identity used to resolve the authored package name.
        package_id: String,
        /// Artifact target, used when metadata excludes a dependency package.
        target: BuildTarget,
        /// Whether Cargo reused an existing artifact without compiling it.
        fresh: bool,
    },
    /// Records without artifact progress remain available in the native log.
    #[serde(other)]
    Other,
}

/// Preserve the target name without exposing Cargo's record shape to the UI.
#[derive(Deserialize)]
struct BuildTarget {
    /// Cargo's native target name.
    name: String,
}

/// Consume Cargo JSON before nextest inventory or test-result decoding.
pub(super) fn consume(
    line: &str,
    workspace: &WorkspaceModel,
    events: Option<&mut TestEventSink>,
) -> Result<bool, AdapterError> {
    let envelope: Envelope<'_> = serde_json::from_str(line)
        .map_err(|error| AdapterError::Protocol(format!("nextest output: {error}")))?;
    if envelope.reason.is_none() || envelope.record_type.is_some() {
        return Ok(false);
    }
    let record: BuildRecord = serde_json::from_str(line)
        .map_err(|error| AdapterError::Protocol(format!("Cargo build event: {error}")))?;
    if let BuildRecord::CompilerArtifact {
        package_id,
        target,
        fresh: false,
    } = record
        && let Some(events) = events
    {
        let package = workspace
            .packages
            .iter()
            .find(|package| package.id == package_id)
            .map(|package| package.name.as_str())
            .or_else(|| {
                package_id
                    .rsplit_once('#')?
                    .1
                    .split_once('@')
                    .map(|(name, _)| name)
            })
            .unwrap_or(&target.name);
        events.accept(TestEvent::Phase {
            name: TestPhase::Build,
            message: Some(format!("Built {package}")),
        })?;
    }
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::adapter::{TestSelection, rust_nextest::targets::BuildArguments};
    use std::path::Path;

    /// Publish actual compilations without letting cached records replace their progress.
    #[test]
    fn cargo_artifacts_publish_completed_packages_and_clear_for_tests() {
        let workspace = WorkspaceModel::parse(
            r#"{"workspace_root":"/repo","workspace_members":["path+file:///repo#forge-diff@0.1.0"],"packages":[{"id":"path+file:///repo#forge-diff@0.1.0","name":"forge-diff","manifest_path":"/repo/Cargo.toml","targets":[]}]}"#,
            &BuildArguments::parse(&[]).unwrap(),
            &|_| unreachable!(),
        )
        .unwrap();
        let mut events =
            TestEventSink::new(TestSelection::default(), Path::new("unused-output")).unwrap();
        let progress = events.progress_subscribe();
        assert!(consume(
            r#"{"reason":"compiler-artifact","package_id":"path+file:///repo#forge-diff@0.1.0","target":{"name":"forge_diff"},"fresh":false}"#,
            &workspace,
            Some(&mut events),
        )
        .unwrap());
        assert_eq!(progress.borrow().phase, Some(TestPhase::Build));
        assert_eq!(
            progress.borrow().message.as_deref(),
            Some("Built forge-diff")
        );
        for line in [
            r#"{"reason":"compiler-artifact","package_id":"registry+https://github.com/rust-lang/crates.io-index#serde@1.0.0","target":{"name":"serde"},"fresh":true}"#,
            r#"{"reason":"build-script-executed","package_id":"dependency"}"#,
            r#"{"reason":"build-finished","success":true}"#,
        ] {
            assert!(consume(line, &workspace, Some(&mut events)).unwrap());
            assert_eq!(
                progress.borrow().message.as_deref(),
                Some("Built forge-diff")
            );
        }
        assert!(consume(
            r#"{"reason":"compiler-artifact","package_id":"registry+https://github.com/rust-lang/crates.io-index#tree-sitter@0.27.0","target":{"name":"tree_sitter"},"fresh":false}"#,
            &workspace,
            Some(&mut events),
        )
        .unwrap());
        assert_eq!(
            progress.borrow().message.as_deref(),
            Some("Built tree-sitter")
        );
        assert!(!consume(r#"{"rust-suites":{}}"#, &workspace, None).unwrap());
        assert!(!consume(r#"{"type":"suite","event":"started"}"#, &workspace, None).unwrap());
        assert!(
            !consume(
                r#"{"type":"test","event":"failed","reason":"time limit exceeded"}"#,
                &workspace,
                None
            )
            .unwrap()
        );
        assert!(consume(r#"{"reason":"compiler-artifact"}"#, &workspace, None).is_err());
        events
            .accept(TestEvent::Phase {
                name: TestPhase::Run,
                message: None,
            })
            .unwrap();
        assert_eq!(progress.borrow().phase, Some(TestPhase::Run));
        assert_eq!(progress.borrow().message, None);
    }
}
