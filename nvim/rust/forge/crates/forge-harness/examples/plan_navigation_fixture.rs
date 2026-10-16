//! Seeds a disposable mock Harness session with real captured language fixtures.

use anyhow::{Context, Result};
use forge_diff::syntax::{DeclarationCallKind, DeclarationOverview};
use forge_harness::plan::{
    CallKind, CallPosition, CallSite, DeclarationDesign, DeclarationFile, FunctionBody,
    PlanFileStore,
};
use forge_harness::storage::SqliteStore;
use sha2::{Digest, Sha256};
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
        _ => anyhow::bail!("unsupported fixture language"),
    };
    let files = PlanFileStore::new(&data, &workspace);
    let mut document = files.read_working_document(session, plan)?;
    let mut design = DeclarationDesign::default();
    design.document = document
        .design
        .as_ref()
        .context("mock plan is not a declaration plan")?
        .document
        .clone();
    for path in paths {
        let source = std::fs::read_to_string(workspace.join(path))?;
        let (text, callables) = DeclarationOverview::extract_with_calls(path, &source)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let bodies = callables
            .into_iter()
            .map(|function| FunctionBody {
                owner: function.owner,
                change: None,
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
        design.baseline.insert(
            (*path).into(),
            DeclarationFile {
                text: text.clone(),
                source_digest: format!("{:x}", Sha256::digest(source.as_bytes())),
            },
        );
        design.proposed.insert((*path).into(), text);
        design.baseline_calls.insert((*path).into(), bodies.clone());
        let mut proposed = bodies;
        for body in &mut proposed {
            body.change = Some("Preserve behavior while reviewing definition navigation.".into());
        }
        design.proposed_calls.insert((*path).into(), proposed);
    }
    document.design = Some(design);
    document.version += 1;
    files.write_working_document(session, plan, &document)?;
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
