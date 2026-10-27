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
}

pub(super) fn definitions(workspace: &std::path::Path) -> Result<Value> {
    let document = super::config::read_document(workspace)?;
    let revision = crate::plan::digest(&serde_json::to_vec(&document)?);
    let Some(source) = document.get("checks") else {
        return Ok(json!({"configured":false,"revision":revision,"definitions":[],"error":null}));
    };
    let config = match serde_json::from_value::<CheckConfig>(source.clone()) {
        Ok(config) => config,
        Err(error) => {
            return Ok(json!({
                "configured":true,"revision":revision,"definitions":[],"error":error.to_string()
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
                "inputs":definition.all.inputs.as_ref().cloned().unwrap_or_default()
            })
        })
        .collect();
    Ok(json!({"configured":true,"revision":revision,"definitions":definitions,"error":null}))
}

pub(super) fn definition(workspace: &std::path::Path, id: &str) -> Result<Value> {
    let document = super::config::read_document(workspace)?;
    let source = document.get("checks").context("checks_not_configured")?;
    let config: CheckConfig = serde_json::from_value(source.clone())?;
    config.validate()?;
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
        "error":error,
        "executed":selected.iter().filter(|command| command.started_at_ms.is_some() && command.reused_from.is_none()).count(),
        "reused":selected.iter().filter(|command| command.reused_from.is_some()).count()
    })
}

fn invocation(run: &CheckRun, command: &CommandCheck) -> Result<Value> {
    let context = command.context.as_ref();
    let operation = context.and_then(|context| context.operation.as_ref());
    if let Some(invocation) = context.and_then(|context| context.invocation.as_ref())
        && operation.is_some_and(|operation| operation.args.is_some())
    {
        return Ok(json!({"program":invocation.program,"args":invocation.args}));
    }
    let Some(executable) = run.executable.as_ref() else {
        return Ok(Value::Null);
    };
    let input = operation.is_some_and(|operation| operation.input);
    let source = if input && run.shell == crate::session::CheckShell::Nushell {
        format!("$in | {}", command.command)
    } else {
        command.command.clone()
    };
    let process = run.shell.command(executable, &source, input)?;
    let process = process.as_std();
    Ok(json!({
        "program":process.get_program().to_string_lossy(),
        "args":process.get_args().map(|argument| argument.to_string_lossy()).collect::<Vec<_>>()
    }))
}

pub(super) fn detail(run: &CheckRun, request: &DetailRequest) -> Result<Value> {
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
            request.cursor.command_index == 0 && request.cursor.offset == 0,
            "invalid detail cursor"
        );
        return Ok(json!({"run_id":run.id,"state":run.state,"command":null,"next_cursor":null}));
    }
    let command = selected
        .get(request.cursor.command_index)
        .context("invalid detail command index")?;
    let path = match request.stream {
        OutputStream::Stdout => &command.stdout,
        OutputStream::Stderr => &command.stderr,
        OutputStream::Combined => &command.output,
    };
    let (text, offset, more) = match std::fs::File::open(path) {
        Ok(mut file) => {
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
            return Err(error).with_context(|| format!("read check log {}", path.display()));
        }
    };
    let next = if more {
        Some(DetailCursor {
            command_index: request.cursor.command_index,
            offset,
        })
    } else if request.cursor.command_index + 1 < selected.len() {
        Some(DetailCursor {
            command_index: request.cursor.command_index + 1,
            offset: 0,
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
