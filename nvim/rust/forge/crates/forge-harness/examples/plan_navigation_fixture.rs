//! Seeds a disposable mock Harness session with real captured language fixtures.

use anyhow::{Context, Result};
use forge_diff::syntax::{DeclarationCallKind, DeclarationOverview};
use forge_harness::plan::{
    CallKind, CallPosition, CallSite, DeclarationDesign, DeclarationFile, FunctionBody,
    PlanFileStore,
};
use forge_harness::storage::SqliteStore;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::PathBuf;

fn main() -> Result<()> {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();
    anyhow::ensure!(
        arguments.len() == 5,
        "expected data directory, workspace, session, plan, language"
    );
    let data = PathBuf::from(&arguments[0]);
    let workspace = PathBuf::from(&arguments[1]);
    let session = &arguments[2];
    let plan = &arguments[3];
    let paths: &[&str] = match arguments[4].as_str() {
        "rust" => &["Cargo.toml", "src/lib.rs", "src/model.rs"],
        "typescript" => &["src/main.ts", "src/model.ts"],
        "lua" => &["main.lua", "model.lua"],
        "behavior" | "tests" | "flows" => &["src/change.rs"],
        _ => anyhow::bail!("unsupported fixture language"),
    };
    let files = PlanFileStore::new(&data, &workspace)?;
    let mut document = files.read_working_document(session, plan)?;
    let mut design = DeclarationDesign::default();
    design.document = document
        .design
        .as_ref()
        .context("mock plan is not a declaration plan")?
        .document
        .clone();
    // Build both sides completely before storing them, because each stored proposal records the
    // baseline it was derived from.
    let mut baseline = BTreeMap::<String, (String, String)>::new();
    let mut proposed = BTreeMap::<String, String>::new();
    let mut proposed_calls = BTreeMap::<String, Vec<FunctionBody>>::new();
    for path in paths {
        let source = std::fs::read_to_string(workspace.join(path))?;
        let (text, callables) = DeclarationOverview::extract_with_calls(path, &source)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let bodies = callables
            .into_iter()
            .map(|function| FunctionBody {
                owner: function.owner,
                behavior: None,
                evidence: None,
                call: Some(
                    function
                        .call
                        .into_iter()
                        .map(|call| CallSite {
                            name: call.name,
                            unresolved: call.unresolved,
                            kind: match call.kind {
                                DeclarationCallKind::Call => CallKind::Call,
                                DeclarationCallKind::Callback => CallKind::Callback,
                                DeclarationCallKind::Property => CallKind::Property,
                                DeclarationCallKind::Value => CallKind::Value,
                                DeclarationCallKind::Construct => CallKind::Construct,
                            },
                            source: Some(CallPosition {
                                line: call.line,
                                column: call.column,
                                expression: call.expression,
                            }),
                        })
                        .collect(),
                ),
            })
            .collect::<Vec<_>>();
        baseline.insert(
            (*path).into(),
            (
                text.clone(),
                format!("{:x}", Sha256::digest(source.as_bytes())),
            ),
        );
        proposed.insert((*path).into(), text);
        design.baseline_calls.insert((*path).into(), bodies.clone());
        let mut changed = bodies;
        for body in &mut changed {
            body.behavior = Some(forge_harness::plan::FunctionBehavior::Change(
                "Preserve behavior while reviewing definition navigation.".into(),
            ));
        }
        proposed_calls.insert((*path).into(), changed);
    }
    if arguments[4] == "behavior" {
        proposed.get_mut("src/change.rs").unwrap().push_str(
            "\n/// Holds the optional probe result.\npub struct ProbeResult {\n  /// Value observed by the probe.\n  pub value: Option<u32>\n}\n/// Preserves the optional value.\npub fn planned_probe(\n  value: Option<u32>,\n  abilities: &mut PlayerAbilities\n) -> ProbeResult;\n",
        );
        proposed_calls.get_mut("src/change.rs").unwrap().push(FunctionBody {
            owner: "planned_probe".into(),
            behavior: Some(forge_harness::plan::FunctionBehavior::Algorithm(
                "result ← value\nrecord(ProbeResult.value)\ndirection ← movement_direction(0)\nabilities.start_dash(direction)\nabilities.cooldown ← 0\ndecrease_cooldowns(cooldown_remaining, weapon.cooldown, elapsed_seconds, minimum_remaining = 0, preserve_order = stored_firing_order)\n# Preserve firing order while decreasing cooldowns\nif result ≠ none:\n  return ProbeResult(value = result)\nreturn ProbeResult(value = none)".into(),
            )),
            evidence: None,
            call: Some(vec![
                CallSite { name: "movement_direction".into(), kind: CallKind::Call, source: None, unresolved: false },
                CallSite { name: "PlayerAbilities::start_dash".into(), kind: CallKind::Call, source: None, unresolved: false },
                CallSite { name: "Other::start_dash".into(), kind: CallKind::Call, source: None, unresolved: false },
                CallSite { name: "PlayerAbilities::cooldown".into(), kind: CallKind::Property, source: None, unresolved: false },
                CallSite { name: "ProbeResult::value".into(), kind: CallKind::Property, source: None, unresolved: false },
                CallSite {
                name: "ProbeResult".into(),
                unresolved: false,
                kind: CallKind::Construct,
                source: None,
            }]),
        });
    }
    if arguments[4] == "tests" {
        proposed.get_mut("src/change.rs").unwrap().push_str("\n#[cfg(test)]\nmod tests {\n  #[test]\n  fn new_case();\n  #[test]\n  fn changed_case();\n  #[test]\n  fn reused_case();\n}\n");
        baseline
            .get_mut("src/change.rs")
            .unwrap()
            .0
            .push_str("\n#[cfg(test)]\nmod history {\n  #[test]\n  fn removed_case();\n}\n");
        proposed.get_mut("src/change.rs").unwrap().push_str("\n/// Exercises the new test-only helpers.\n#[cfg(test)]\nmod added_tests {\n  #[test]\n  fn added_case();\n}\n");
        proposed_calls
            .get_mut("src/change.rs")
            .unwrap()
            .push(FunctionBody {
                owner: "added_tests::added_case".into(),
                evidence: None,
                behavior: Some(forge_harness::plan::FunctionBehavior::Algorithm(
                    "return".into(),
                )),
                call: Some(Vec::new()),
            });
        proposed.insert(
            "tests/new.rs".into(),
            "// Planned integration test file.\n".into(),
        );
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"src/change.rs","cases":[
                {"name":"combat::tests::new_case","change":"new","description":"Adds a scenario."},
                {"name":"combat::tests::changed_case","change":"modified","description":"Updates a long scenario whose full expected result remains readable when the diff wraps across multiple screen rows in a narrow window."},
                {"name":"history::removed_case","change":"removed","description":"Removes obsolete coverage."},
                {"name":"combat::tests::reused_case","change":"reused","description":"Preserves current coverage."}]},
            {"file":"tests/new.rs","cases":[{"name":"new_file_case","change":"new","description":"Covers a new test file."}]},
            {"file":"tests/reused.rs","cases":[{"name":"reused_file_case","change":"reused","description":"Keeps a separate existing suite."}]}
        ]))?;
    }
    if arguments[4] == "flows" {
        design.document.flows = "## Restart input\n\nRuns on a fresh R input and restores the base and combat state.\n\n```mermaid\nflowchart TD\n    input([Fresh R input]) --> restart[restart_round]\n    restart --> reset[reset_combat]\n    reset --> old{old combat entities exist?}\n    old -->|yes| remove[remove old entities]\n```\n".into();
    }
    for (path, (text, source_digest)) in baseline {
        let calls = design
            .baseline_calls
            .get(&path)
            .cloned()
            .unwrap_or_default();
        let file = DeclarationFile::new(&path, text, source_digest, &calls)?;
        design.baseline.insert(path, file);
    }
    for (path, declarations) in proposed {
        let calls = proposed_calls.remove(&path).unwrap_or_default();
        design.parsed_insert(&path, declarations, calls)?;
    }
    document.design = Some(design);
    document.version += 1;
    files.create_working_document(session, plan, &document)?;
    let mut store = SqliteStore::open(&data)?;
    let mut record = store
        .load_plan(plan)?
        .context("mock plan record is missing")?;
    record.model_revision += 1;
    let (document, _, digest) =
        files.submit_document_revision(session, plan, record.model_revision, document.version)?;
    record.document_version = document.version;
    record.submitted_version = Some(document.version);
    record.review_digest = Some(digest);
    store.save_plan(&record)?;
    println!("{}", serde_json::to_string(&record)?);
    Ok(())
}
