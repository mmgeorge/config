use super::{CheckConfig, CheckRun, CheckState, CommandCheck};
use anyhow::{Context, Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::io::{Read, Seek};

#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DetailCursor {
    pub command_index: usize,
    pub offset: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub result: Option<ResultPageCursor>,
}

/// Bind structured pagination to the immutable command and collection it traverses.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ResultPageCursor {
    run_id: String,
    command_id: String,
    collection: ResultCollection,
}

/// Identify a typed result collection without interpreting adapter-owned identifiers.
#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum ResultCollection {
    Cases,
    NotFound,
    Diagnostics,
    Targets,
    Artifacts,
    Files,
}

impl ResultCollection {
    /// Get the ordered collections belonging to this command's category contract.
    fn collections(result: &super::adapter::AdapterResult) -> &'static [Self] {
        match result {
            super::adapter::AdapterResult::Tests(_) => &[Self::Cases, Self::NotFound],
            super::adapter::AdapterResult::Diagnostics(_) => {
                &[Self::Diagnostics, Self::Targets, Self::Artifacts]
            }
            super::adapter::AdapterResult::Format(_) => &[Self::Files],
        }
    }

    /// Get a collection's length, rejecting a cursor from another result contract.
    fn count(self, result: &super::adapter::AdapterResult) -> Result<usize> {
        use super::adapter::AdapterResult;
        match (self, result) {
            (Self::Cases, AdapterResult::Tests(result)) => Ok(result.cases.len()),
            (Self::NotFound, AdapterResult::Tests(result)) => Ok(result.not_found.len()),
            (Self::Diagnostics, AdapterResult::Diagnostics(result)) => Ok(result.diagnostics.len()),
            (Self::Targets, AdapterResult::Diagnostics(result)) => Ok(result.targets.len()),
            (Self::Artifacts, AdapterResult::Diagnostics(result)) => Ok(result.artifacts.len()),
            (Self::Files, AdapterResult::Format(result)) => Ok(result.files.len()),
            (
                Self::Cases | Self::NotFound,
                AdapterResult::Diagnostics(_) | AdapterResult::Format(_),
            )
            | (
                Self::Diagnostics | Self::Targets | Self::Artifacts,
                AdapterResult::Tests(_) | AdapterResult::Format(_),
            )
            | (Self::Files, AdapterResult::Tests(_) | AdapterResult::Diagnostics(_)) => {
                anyhow::bail!("result cursor collection does not belong to this command")
            }
        }
    }

    /// Serialize only the selected page instead of copying every retained result.
    fn page(
        self,
        result: &super::adapter::AdapterResult,
        start: usize,
        end: usize,
    ) -> Result<Value> {
        use super::adapter::AdapterResult;
        match (self, result) {
            (Self::Cases, AdapterResult::Tests(result)) => {
                Ok(serde_json::to_value(&result.cases[start..end])?)
            }
            (Self::NotFound, AdapterResult::Tests(result)) => {
                Ok(serde_json::to_value(&result.not_found[start..end])?)
            }
            (Self::Diagnostics, AdapterResult::Diagnostics(result)) => {
                Ok(serde_json::to_value(&result.diagnostics[start..end])?)
            }
            (Self::Targets, AdapterResult::Diagnostics(result)) => {
                Ok(serde_json::to_value(&result.targets[start..end])?)
            }
            (Self::Artifacts, AdapterResult::Diagnostics(result)) => {
                Ok(serde_json::to_value(&result.artifacts[start..end])?)
            }
            (Self::Files, AdapterResult::Format(result)) => {
                Ok(serde_json::to_value(&result.files[start..end])?)
            }
            (
                Self::Cases | Self::NotFound,
                AdapterResult::Diagnostics(_) | AdapterResult::Format(_),
            )
            | (
                Self::Diagnostics | Self::Targets | Self::Artifacts,
                AdapterResult::Tests(_) | AdapterResult::Format(_),
            )
            | (Self::Files, AdapterResult::Tests(_) | AdapterResult::Diagnostics(_)) => {
                anyhow::bail!("result cursor collection does not belong to this command")
            }
        }
    }

    /// Get the response field used for entries in this collection.
    fn field(self) -> &'static str {
        match self {
            Self::Cases | Self::Diagnostics | Self::Files => "results",
            Self::NotFound => "not_found",
            Self::Targets => "targets",
            Self::Artifacts => "artifacts",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum OutputStream {
    Stdout,
    Stderr,
    #[default]
    Combined,
}

#[derive(Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub(crate) struct DetailRequest {
    pub run_id: String,
    #[serde(default)]
    pub cursor: DetailCursor,
    #[serde(default)]
    pub stream: OutputStream,
    #[serde(default)]
    pub results: bool,
    #[serde(default)]
    pub case: Option<super::adapter::TestItemId>,
}

pub(super) fn definitions(workspace: &std::path::Path) -> Result<Value> {
    let document = super::config::read_document(workspace)?;
    let revision = crate::plan::digest(&serde_json::to_vec(&document)?);
    let Some(source) = document.get("checks") else {
        return Ok(
            json!({"configured":false,"revision":revision,"definitions":[],"adapters":super::adapter::catalog(),"error":null}),
        );
    };
    let config = match serde_json::from_value::<CheckConfig>(source.clone()) {
        Ok(config) => config,
        Err(error) => {
            return Ok(json!({
                "configured":true,"revision":revision,"definitions":[],"adapters":super::adapter::catalog(),"error":error.to_string()
            }));
        }
    };
    let mut ordered: Vec<_> = config.definitions.iter().collect();
    ordered.sort_by_key(|definition| (definition.category.priority(), &definition.id));
    let definitions: Vec<_> = ordered
        .into_iter()
        .map(|definition| {
            json!({
                "check_id":definition.id,
                "label":definition.name,
                "category":definition.category,
                "auto":definition.auto,
                "enabled":definition.enabled,
                "inputs":definition.runner.command().and_then(|command| command.inputs.as_ref()).cloned().unwrap_or_default()
            })
        })
        .collect();
    Ok(
        json!({"configured":true,"revision":revision,"definitions":definitions,"adapters":super::adapter::catalog(),"error":null}),
    )
}

pub(super) fn definition(workspace: &std::path::Path, id: &str) -> Result<Value> {
    let document = super::config::read_document(workspace)?;
    let source = document.get("checks").context("checks_not_configured")?;
    // Inspection must remain available when legacy or invalid definitions need repair.
    let source = source
        .as_object()
        .context("check definitions must be an object")?;
    let definition = source
        .get(id)
        .with_context(|| format!("unknown check definition: {id}"))?;
    Ok(json!({
        "revision":crate::plan::digest(&serde_json::to_vec(&document)?),
        "check_id":id,"definition":definition
    }))
}

pub(super) fn summary(run: &CheckRun) -> Value {
    let selected: Vec<_> = run
        .commands
        .iter()
        .filter(|command| command.selected)
        .collect();
    let mut definition = std::collections::BTreeMap::new();
    for command in &selected {
        let id = command
            .context
            .as_ref()
            .map_or(command.id.as_str(), |context| &context.definition_id);
        let terminal = !matches!(command.state, CheckState::Pending | CheckState::Running);
        definition
            .entry(id)
            .and_modify(|complete| *complete &= terminal)
            .or_insert(terminal);
    }
    let latest = selected
        .iter()
        .rev()
        .filter(|command| {
            (command.started_at_ms.is_some() || command.reused_from.is_some())
                && !command.preview.trim().is_empty()
        })
        .max_by_key(|command| {
            (
                command.state == CheckState::Running,
                command.completed_at_ms.or(command.started_at_ms),
            )
        });
    let latest_output = latest.and_then(|command| {
        String::from_utf8_lossy(&strip_ansi_escapes::strip(command.preview.as_bytes()))
            .rsplit(['\r', '\n'])
            .map(str::trim)
            .find(|line| !line.is_empty())
            .map(str::to_owned)
    });
    let error = selected
        .iter()
        .filter(|command| !matches!(command.state, CheckState::Passed | CheckState::Skipped))
        .find_map(|command| command.error.as_deref())
        .map(str::to_owned);
    json!({
        "run_id":run.id,
        "state":run.state,
        "completed":definition.values().filter(|complete| **complete).count(),
        "total":definition.len(),
        "elapsed_ms":run.completed_at_ms.unwrap_or_else(super::supervisor::now_ms)
            .saturating_sub(run.started_at_ms).max(0),
        "exit_code":if selected.len() == 1 { selected[0].exit_code } else { None },
        "latest_output":latest_output,
        "progress":selected.iter().find(|command| command.state == CheckState::Running).or_else(|| (selected.len() == 1).then(|| &selected[0])).and_then(|command| command.adapter_progress.as_ref()),
        "error":error,
        "executed":selected.iter().filter(|command| command.started_at_ms.is_some() && command.reused_from.is_none()).count(),
        "reused":selected.iter().filter(|command| command.reused_from.is_some()).count()
    })
}

fn invocation(run: &CheckRun, command: &CommandCheck) -> Result<Value> {
    let context = command.context.as_ref();
    if let Some(id) = context.map(|context| &context.definition_id)
        && let Some(definition) = run.context.as_ref().and_then(|context| {
            context
                .snapshot
                .config
                .definitions
                .iter()
                .find(|definition| &definition.id == id)
        })
        && let super::CheckRunner::Adapter(super::adapter::AdapterConfig::Script(options)) =
            &definition.runner
    {
        return Ok(json!({"program":options.command[0],"args":options.command[1..]}));
    }
    let operation = context.and_then(|context| context.operation.as_ref());
    if let Some(invocation) = context.and_then(|context| context.invocation.as_ref())
        && operation.is_some_and(|operation| operation.args.is_some())
    {
        return Ok(json!({"program":invocation.program,"args":invocation.args}));
    }
    let Some(executable) = run.executable.as_ref() else {
        return Ok(Value::Null);
    };
    let process = run.shell.command(executable, &command.command, false)?;
    let process = process.as_std();
    Ok(json!({
        "program":process.get_program().to_string_lossy(),
        "args":process.get_args().map(|argument| argument.to_string_lossy()).collect::<Vec<_>>()
    }))
}

pub(super) fn detail(run: &CheckRun, request: &DetailRequest) -> Result<Value> {
    ensure!(
        request.run_id == run.id,
        "detail request does not match this run"
    );
    ensure!(
        !(request.results && request.case.is_some()),
        "choose results or case output, not both"
    );
    ensure!(
        request.results || request.cursor.result.is_none(),
        "result cursor requires results:true"
    );
    if matches!(run.state, CheckState::Pending | CheckState::Running) {
        return Ok(json!({
            "run_id":run.id,"state":run.state,
            "message":"Still running. Use check_status to wait."
        }));
    }
    let selected: Vec<_> = run
        .commands
        .iter()
        .filter(|command| command.selected)
        .collect();
    if selected.is_empty() {
        ensure!(
            request.cursor.command_index == 0
                && request.cursor.offset == 0
                && request.cursor.result.is_none(),
            "invalid detail cursor"
        );
        return Ok(json!({"run_id":run.id,"state":run.state,"command":null,"next_cursor":null}));
    }
    if request.results {
        return result_detail(run, &selected, &request.cursor);
    }
    let command = selected
        .get(request.cursor.command_index)
        .context("invalid detail command index")?;
    let case_output = request
        .case
        .as_ref()
        .map(|id| {
            command
                .result
                .as_ref()
                .and_then(super::adapter::AdapterResult::tests)
                .and_then(|results| results.cases.iter().find(|case| &case.id == id))
                .context("unknown test case")
        })
        .transpose()?;
    let path = match case_output {
        Some(case) => case.output.as_ref(),
        None => Some(match request.stream {
            OutputStream::Stdout => &command.stdout,
            OutputStream::Stderr => &command.stderr,
            OutputStream::Combined => &command.output,
        }),
    };
    let (text, offset, more) = match path.map(std::fs::File::open).transpose() {
        Ok(None) => {
            ensure!(request.cursor.offset == 0, "case has no output");
            (String::new(), 0, false)
        }
        Ok(Some(mut file)) => {
            let length = file.metadata()?.len();
            ensure!(
                request.cursor.offset <= length,
                "detail cursor exceeds log length"
            );
            file.seek(std::io::SeekFrom::Start(request.cursor.offset))?;
            let mut bytes = Vec::new();
            (&mut file).take(16_384).read_to_end(&mut bytes)?;
            while let Err(error) = std::str::from_utf8(&bytes) {
                ensure!(error.error_len().is_none(), "check log is not UTF-8");
                let mut next = [0];
                ensure!(
                    file.read(&mut next)? == 1,
                    "check log ends in incomplete UTF-8"
                );
                bytes.push(next[0]);
            }
            let offset = request.cursor.offset + bytes.len() as u64;
            (
                String::from_utf8(bytes).context("decode check log")?,
                offset,
                offset < length,
            )
        }
        Err(error)
            if error.kind() == std::io::ErrorKind::NotFound
                && command.started_at_ms.is_none()
                && command.reused_from.is_none() =>
        {
            ensure!(
                request.cursor.offset == 0,
                "invalid detail cursor for unstarted command"
            );
            (String::new(), 0, false)
        }
        Err(error) => {
            return Err(error).with_context(|| {
                format!(
                    "read check log {}",
                    path.expect("file error has a path").display()
                )
            });
        }
    };
    let next = if more {
        Some(DetailCursor {
            command_index: request.cursor.command_index,
            offset,
            result: None,
        })
    } else if request.case.is_none() && request.cursor.command_index + 1 < selected.len() {
        Some(DetailCursor {
            command_index: request.cursor.command_index + 1,
            offset: 0,
            result: None,
        })
    } else {
        None
    };
    let context = command.context.as_ref();
    Ok(json!({
        "run_id":run.id,"state":run.state,
        "command":{
            "command_id":command.id,
            "check_id":context.map(|context| &context.definition_id),
            "invocation":invocation(run, command)?,
            "directory":std::path::Path::new(&run.workspace).join(context.map_or(".", |context| context.directory.as_str())),
            "inputs":context.and_then(|context| context.invocation.as_ref()).map(|invocation| &invocation.inputs),
            "state":command.state,"exit_code":command.exit_code,"error":command.error,
            "elapsed_ms":command.elapsed_ms(super::supervisor::now_ms()),
            "reused_from":command.reused_from
        },
        "stream":request.stream,"offset":request.cursor.offset,"text":text,"next_cursor":next
    }))
}

/// Page one contract collection and bind continuation to its immutable run and command.
fn result_detail(
    run: &CheckRun,
    selected: &[&CommandCheck],
    cursor: &DetailCursor,
) -> Result<Value> {
    let (command_index, collection) = if let Some(identity) = &cursor.result {
        ensure!(
            identity.run_id == run.id,
            "result cursor belongs to another run"
        );
        let command = selected
            .get(cursor.command_index)
            .context("invalid detail command index")?;
        ensure!(
            identity.command_id == command.id,
            "result cursor belongs to another command"
        );
        (cursor.command_index, identity.collection)
    } else {
        ensure!(
            cursor.command_index == 0 && cursor.offset == 0,
            "result continuation requires the returned cursor"
        );
        let Some(index) = selected.iter().position(|command| command.result.is_some()) else {
            return Ok(
                json!({"run_id":run.id,"state":run.state,"results":[],"counts":{},"next_cursor":null}),
            );
        };
        let result = selected[index]
            .result
            .as_ref()
            .expect("selected structured result exists");
        (index, ResultCollection::collections(result)[0])
    };
    let command = selected[command_index];
    let result = command
        .result
        .as_ref()
        .context("this command has no structured adapter results")?;
    let count = collection.count(result)?;
    let start = usize::try_from(cursor.offset).context("result cursor is too large")?;
    ensure!(start <= count, "result cursor exceeds result count");
    let end = start.saturating_add(100).min(count);

    // Advance across collections and commands without duplicating metadata collections.
    let continuation = if end < count {
        Some((command_index, collection, end))
    } else {
        let collections = ResultCollection::collections(result);
        let index = collections
            .iter()
            .position(|candidate| *candidate == collection)
            .expect("validated collection belongs to result");
        let next_collection = collections[index + 1..]
            .iter()
            .copied()
            .find(|candidate| candidate.count(result).is_ok_and(|count| count > 0));
        next_collection
            .map(|collection| (command_index, collection, 0))
            .or_else(|| {
                selected
                    .iter()
                    .enumerate()
                    .skip(command_index + 1)
                    .find_map(|(index, command)| {
                        command
                            .result
                            .as_ref()
                            .map(|result| (index, ResultCollection::collections(result)[0], 0))
                    })
            })
    };
    let next = continuation.map(|(index, collection, offset)| DetailCursor {
        command_index: index,
        offset: u64::try_from(offset).expect("result offset fits u64"),
        result: Some(ResultPageCursor {
            run_id: run.id.clone(),
            command_id: selected[index].id.clone(),
            collection,
        }),
    });
    let mut details = match result {
        super::adapter::AdapterResult::Tests(result) => json!({
            "category":"tests","widened":result.widened,"error":result.error,"protocol_error":result.protocol_error,
            "output_truncated_bytes":result.output_truncated_bytes,
            "counts":{"cases":result.cases.len(),"not_found":result.not_found.len()}
        }),
        super::adapter::AdapterResult::Diagnostics(result) => json!({
            "category":"diagnostics","widened":result.widened,"error":result.error,"protocol_error":result.protocol_error,
            "truncated":result.truncated,"truncated_bytes":result.truncated_bytes,
            "counts":{"diagnostics":result.diagnostics.len() + result.truncated.values().sum::<usize>(),"retained_diagnostics":result.diagnostics.len(),"targets":result.targets.len(),"artifacts":result.artifacts.len()}
        }),
        super::adapter::AdapterResult::Format(result) => json!({
            "category":"format","operation":result.operation,"widened":result.widened,"error":result.error,"protocol_error":result.protocol_error,
            "truncated_bytes":result.truncated_bytes,"counts":{"files":result.files.len()}
        }),
    };
    details[collection.field()] = collection.page(result, start, end)?;
    details["collection"] = json!(collection);
    details["run_id"] = json!(run.id);
    details["state"] = json!(run.state);
    details["command_state"] = json!(command.state);
    details["command_id"] = json!(command.id);
    details["offset"] = json!(start);
    details["next_cursor"] = json!(next);

    // Limit human text at the response boundary without altering retained results or identity.
    let mut preview = TextPreview {
        remaining: 64 * 1024,
        omitted: 0,
    };
    preview.bound(&mut details);
    details["preview_omitted_bytes"] = json!(preview.omitted);
    Ok(details)
}

/// Bound free-form result text while keeping identifiers and pagination coordinates intact.
struct TextPreview {
    remaining: usize,
    omitted: usize,
}

impl TextPreview {
    /// Trim text fields at UTF-8 boundaries and count bytes omitted from this response only.
    fn bound(&mut self, value: &mut Value) {
        match value {
            Value::Object(fields) => {
                for (key, value) in fields {
                    if matches!(
                        key.as_str(),
                        "message"
                            | "rendered"
                            | "diff"
                            | "replacement"
                            | "widened"
                            | "error"
                            | "protocol_error"
                            | "label"
                            | "name"
                            | "code"
                            | "source"
                    ) && let Value::String(text) = value
                    {
                        self.text(text);
                    } else if key == "artifacts"
                        && let Value::Array(artifacts) = value
                    {
                        for artifact in artifacts {
                            if let Some(Value::String(kind)) = artifact.get_mut(1) {
                                self.text(kind);
                            }
                        }
                    } else {
                        self.bound(value);
                    }
                }
            }
            Value::Array(values) => {
                for value in values {
                    self.bound(value);
                }
            }
            Value::String(_) | Value::Number(_) | Value::Bool(_) | Value::Null => {}
        }
    }

    /// Preserve UTF-8 and account for omitted bytes independently of stored truncation.
    fn text(&mut self, text: &mut String) {
        let mut end = text.len().min(self.remaining);
        while !text.is_char_boundary(end) {
            end -= 1;
        }
        self.omitted += text.len() - end;
        self.remaining -= end;
        text.truncate(end);
    }
}

#[cfg(test)]
mod definition_tests {
    use super::*;

    /// Preserve invalid authored definitions for revision-checked repair without accepting them for execution.
    #[test]
    fn invalid_configuration_can_be_inspected_without_modification() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let path = workspace.path().join(".forge.json");
        let authored = r#"{"checks":{"legacy":{"trigger":{},"action":{"category":"check","command":["old-tool"]}},"broken":{"action":{"category":"unknown"}}},"other":{"keep":true}}"#;
        std::fs::write(&path, authored)?;
        let listing = definitions(workspace.path())?;
        assert!(listing["error"].is_string());
        let legacy = definition(workspace.path(), "legacy")?;
        assert_eq!(legacy["revision"], listing["revision"]);
        assert_eq!(legacy["definition"]["action"]["category"], "check");
        assert_eq!(
            legacy["definition"]["action"]["command"],
            json!(["old-tool"])
        );
        assert_eq!(
            definition(workspace.path(), "broken")?["definition"]["action"]["category"],
            "unknown"
        );
        assert!(definition(workspace.path(), "missing").is_err());
        assert_eq!(std::fs::read_to_string(path)?, authored);
        Ok(())
    }
}

#[cfg(test)]
mod result_tests {
    use super::*;

    /// Construct saved commands through the same serialization boundary as restored history.
    fn run(results: Vec<Value>) -> Result<CheckRun> {
        let commands = results.into_iter().enumerate().map(|(index, result)| json!({
            "id":format!("command-{index}"),"command":"fixture","state":"failed","started_at_ms":1,"completed_at_ms":2,
            "exit_code":1,"error":null,"stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
            "context":null,"result":result,"selected":true
        })).collect::<Vec<_>>();
        Ok(serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":2,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":commands
        }))?)
    }

    /// Traverse every collection once while limiting entries and preserving large stored messages.
    #[test]
    fn collections_and_large_text_are_paged_without_repetition() -> Result<()> {
        let message = "界".repeat(100_000);
        let run = run(vec![json!({
            "category":"diagnostics","selection":{"paths":[]},"widened":null,
            "targets":(0..205).map(|index| json!({"id":format!("target-{index}"),"status":"passed","duration_ms":null})).collect::<Vec<_>>(),
            "diagnostics":[{"severity":"error","message":message,"path":"src/main.rs"}],
            "artifacts":(0..105).map(|index| json!([format!("target/{index}"),"binary"])).collect::<Vec<_>>(),
            "truncated":{"error":2},"truncated_bytes":0,"error":null,"protocol_error":null,"exit":1,"cancelled":false,"unsupported":false
        })])?;
        let mut request: DetailRequest =
            serde_json::from_value(json!({"run_id":"run","results":true}))?;
        let mut totals = std::collections::BTreeMap::<String, usize>::new();
        let mut targets = std::collections::BTreeSet::new();
        let mut pages = 0;
        loop {
            let page = detail(&run, &request)?;
            pages += 1;
            let collection = page["collection"].as_str().unwrap();
            let field = match collection {
                "targets" => "targets",
                "artifacts" => "artifacts",
                _ => "results",
            };
            let entries = page[field].as_array().unwrap();
            assert!(entries.len() <= 100);
            *totals.entry(collection.to_owned()).or_default() += entries.len();
            assert_eq!(page["counts"]["targets"], 205);
            assert_eq!(page["counts"]["artifacts"], 105);
            assert_eq!(page["counts"]["diagnostics"], 3);
            assert_eq!(page["counts"]["retained_diagnostics"], 1);
            if collection == "diagnostics" {
                assert_eq!(
                    page["results"][0]["message"].as_str().unwrap().len(),
                    65_535
                );
                assert_eq!(page["preview_omitted_bytes"], message.len() - 65_535);
                assert!(page.get("targets").is_none());
                assert!(page.get("artifacts").is_none());
            }
            for entry in entries.iter().filter(|_| collection == "targets") {
                assert!(targets.insert(entry["id"].as_str().unwrap().to_owned()));
            }
            if page["next_cursor"].is_null() {
                break;
            }
            request.cursor = serde_json::from_value(page["next_cursor"].clone())?;
        }
        assert_eq!(pages, 6);
        assert_eq!(totals["diagnostics"], 1);
        assert_eq!(totals["targets"], 205);
        assert_eq!(totals["artifacts"], 105);
        let super::super::adapter::AdapterResult::Diagnostics(result) =
            run.commands[0].result.as_ref().unwrap()
        else {
            panic!("wrong fixture")
        };
        assert_eq!(result.diagnostics[0].message, message);
        Ok(())
    }

    /// Bind continuations to their run, command, collection, and output mode.
    #[test]
    fn result_cursors_reject_rebinding_and_skip_command_only_results() -> Result<()> {
        let format = json!({
            "category":"format","operation":"check","selection":{"paths":[]},"widened":null,
            "files":[],"truncated_bytes":0,"error":null,"protocol_error":null,"exit":0,"cancelled":false,"unsupported":false
        });
        let run = run(vec![Value::Null, format.clone(), Value::Null, format])?;
        let request: DetailRequest =
            serde_json::from_value(json!({"run_id":"run","results":true}))?;
        let page = detail(&run, &request)?;
        assert_eq!(page["command_id"], "command-1");
        assert_eq!(page["results"], json!([]));
        let cursor = page["next_cursor"].clone();
        let valid: DetailRequest =
            serde_json::from_value(json!({"run_id":"run","results":true,"cursor":cursor}))?;
        assert_eq!(detail(&run, &valid)?["command_id"], "command-3");
        for invalid in [
            json!({"run_id":"different","results":true,"cursor":cursor}),
            json!({"run_id":"run","results":false,"cursor":cursor}),
            json!({"run_id":"run","results":true,"cursor":{"command_index":1,"offset":0,"result":cursor["result"]}}),
            json!({"run_id":"run","results":true,"cursor":{"command_index":3,"offset":1,"result":cursor["result"]}}),
            json!({"run_id":"run","results":true,"cursor":{"command_index":3,"offset":0,"result":{"run_id":"run","command_id":"command-3","collection":"targets"}}}),
            json!({"run_id":"run","results":true,"cursor":{"command_index":3,"offset":0}}),
        ] {
            assert!(
                detail(&run, &serde_json::from_value(invalid.clone())?).is_err(),
                "accepted {invalid}"
            );
        }
        Ok(())
    }

    /// Keep explicit missing IDs inspectable without placing all of them on every case page.
    #[test]
    fn missing_test_ids_are_a_separate_collection() -> Result<()> {
        let run = run(vec![json!({
            "category":"tests","selection":{"ids":[],"affected":[]},"items":[],"widened":null,"cases":[],
            "not_found":(0..101).map(|index| format!("case-{index}")).collect::<Vec<_>>(),
            "error":null,"protocol_error":null,"exit":1,"cancelled":false,"unsupported":false,"output_truncated_bytes":0
        })])?;
        let mut request: DetailRequest =
            serde_json::from_value(json!({"run_id":"run","results":true}))?;
        let first = detail(&run, &request)?;
        assert_eq!(first["collection"], "cases");
        assert!(first.get("not_found").is_none());
        request.cursor = serde_json::from_value(first["next_cursor"].clone())?;
        let second = detail(&run, &request)?;
        assert_eq!(second["collection"], "not_found");
        assert_eq!(second["not_found"].as_array().unwrap().len(), 100);
        request.cursor = serde_json::from_value(second["next_cursor"].clone())?;
        let third = detail(&run, &request)?;
        assert_eq!(third["not_found"], json!(["case-100"]));
        assert!(third["next_cursor"].is_null());
        Ok(())
    }
}
