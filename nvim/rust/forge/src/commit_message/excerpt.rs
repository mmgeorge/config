use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use forge_diff::cache::AnalysisHandle;
use forge_diff::raw::RawDiff;
use forge_diff::source::SourceVersion;
use forge_diff::syntax::{SourceOutline, SyntaxEngine, SyntaxLanguage, SyntaxRequest};
use forge_diff::workers::WorkPriority;
use forge_harness::backend::TextGeneration;

/// Maximum combined UTF-8 size of constructed system instructions and prompt text.
pub(super) const MAX_INPUT_BYTES: usize = 500_000;
const BODY_CHANGED_LINES: usize = 20;
const CONTEXT_LINES: usize = 2;
const INSTRUCTIONS: &str = "Generate a conventional commit message for these changes.\n\n\
The diff is compacted. '+ ' marks added lines, '- ' removed lines, and '  ' unchanged lines. \
Signatures are retained. Omission markers include added and removed line counts. \
Import changes are excluded from excerpts. Files without excerpts appear only in the file list. \
Describe visible changes without guessing the contents of omitted lines.\n\nChanged files:\n";

/// A complete file comparison retained until inventory and excerpt selection finish.
pub(super) struct ChangedFile {
    /// Repository-relative display path.
    pub path: String,
    /// Non-content changes such as relocation or file mode.
    pub note: String,
    /// Exact text comparison, absent for binary or unsupported content.
    pub analysis: Option<AnalysisHandle>,
}

impl ChangedFile {
    fn heading(&self) -> String {
        let counts = self
            .analysis
            .as_ref()
            .map(|analysis| analysis.line_counts());
        let counts = counts.map_or_else(
            || "counts unavailable".into(),
            |counts| format!("+{} -{}", counts.added, counts.deleted),
        );
        let path = self.path.replace('\n', "\\n").replace('\r', "\\r");
        if self.note.is_empty() {
            format!("{path} ({counts})")
        } else {
            format!("{path} ({counts}, {})", self.note)
        }
    }
}

struct PromptWriter {
    text: String,
    limit: usize,
    stopped: bool,
}

impl PromptWriter {
    fn new(limit: usize) -> Self {
        Self {
            text: String::new(),
            limit,
            stopped: false,
        }
    }

    fn append(&mut self, text: &str) -> bool {
        if self.stopped || text.len() > self.limit.saturating_sub(self.text.len()) {
            self.stopped = true;
            return false;
        }
        self.text.push_str(text);
        true
    }
}

struct DiffRow<'source> {
    text: &'source str,
    marker: char,
    old: Option<usize>,
    new: Option<usize>,
}

impl DiffRow<'_> {
    fn changed(&self) -> bool {
        self.marker != ' '
    }
    fn render(&self) -> String {
        format!("{} {}\n", self.marker, self.text)
    }
}

#[derive(Default)]
struct ObjectRows {
    rows: Vec<usize>,
    signature: Vec<usize>,
    closing: Vec<usize>,
}

/// Construct one bounded prompt for every comparison and provider route.
pub(super) async fn prompt(files: &mut [ChangedFile], syntax: &Arc<SyntaxEngine>) -> String {
    render(
        files,
        syntax,
        MAX_INPUT_BYTES.saturating_sub(TextGeneration::CommitMessage.instructions().len()),
    )
    .await
}

async fn render(files: &mut [ChangedFile], syntax: &Arc<SyntaxEngine>, limit: usize) -> String {
    files.sort_by(|left, right| {
        test_path(&left.path)
            .cmp(&test_path(&right.path))
            .then(left.path.cmp(&right.path))
    });
    let mut writer = PromptWriter::new(limit);
    writer.append(INSTRUCTIONS);
    for file in files.iter() {
        if !writer.append(&format!("  {}\n", file.heading())) {
            return writer.text;
        }
    }
    let deadline = Instant::now() + Duration::from_secs(2);
    for file in files.iter() {
        if writer.stopped {
            break;
        }
        let Some(analysis) = file.analysis.as_ref() else {
            continue;
        };
        if analysis.hunks().is_empty() {
            continue;
        }
        let document = matches!(
            file.path
                .rsplit('.')
                .next()
                .map(str::to_ascii_lowercase)
                .as_deref(),
            Some("md" | "txt")
        );
        let (old, new) = if document {
            (SourceOutline::default(), SourceOutline::default())
        } else {
            let old = outline(&file.path, &analysis.source().old, syntax, deadline).await;
            let new = outline(&file.path, &analysis.source().new, syntax, deadline).await;
            (old, new)
        };
        emit_file(&mut writer, file, &old, &new, document);
    }
    writer.text
}

fn test_path(path: &str) -> bool {
    let path = path.to_ascii_lowercase();
    let name = path.rsplit(['/', '\\']).next().unwrap_or(&path);
    name.contains("test")
        || name.contains("spec")
        || path
            .split(['/', '\\'])
            .any(|part| matches!(part, "test" | "tests" | "spec" | "specs" | "__tests__"))
}

async fn outline(
    path: &str,
    source: &SourceVersion,
    syntax: &Arc<SyntaxEngine>,
    deadline: Instant,
) -> SourceOutline {
    let Some(language) = SyntaxLanguage::for_path(path) else {
        return SourceOutline::default();
    };
    if source.bytes().is_empty() || Instant::now() >= deadline {
        return SourceOutline::default();
    }
    match tokio::time::timeout_at(
        deadline.into(),
        syntax.analyze(SyntaxRequest {
            source: source.clone(),
            language,
            priority: WorkPriority::Speculative,
            deadline: Some(deadline),
        }),
    )
    .await
    {
        Ok(Ok(parsed)) => parsed.outline(),
        _ => SourceOutline::default(),
    }
}

fn rows(analysis: &RawDiff) -> Vec<DiffRow<'_>> {
    let old: Vec<_> = analysis.source().old.text().lines().collect();
    let new: Vec<_> = analysis.source().new.text().lines().collect();
    let mut rows = Vec::new();
    let mut old_position = 0;
    let mut new_position = 0;
    for hunk in analysis.hunks() {
        while new_position < hunk.new_lines.start {
            rows.push(DiffRow {
                text: new[new_position],
                marker: ' ',
                old: Some(old_position),
                new: Some(new_position),
            });
            old_position += 1;
            new_position += 1;
        }
        for position in hunk.old_lines.clone() {
            rows.push(DiffRow {
                text: old[position],
                marker: '-',
                old: Some(position),
                new: None,
            });
        }
        for position in hunk.new_lines.clone() {
            rows.push(DiffRow {
                text: new[position],
                marker: '+',
                old: None,
                new: Some(position),
            });
        }
        old_position = hunk.old_lines.end;
        new_position = hunk.new_lines.end;
    }
    while new_position < new.len() {
        rows.push(DiffRow {
            text: new[new_position],
            marker: ' ',
            old: Some(old_position),
            new: Some(new_position),
        });
        old_position += 1;
        new_position += 1;
    }
    rows
}

fn owner(outline: &SourceOutline, row: Option<usize>) -> Option<usize> {
    let row = row?;
    outline
        .scope
        .iter()
        .enumerate()
        .filter(|(_, scope)| scope.extent.contains(&row))
        .min_by_key(|(_, scope)| scope.extent.len())
        .map(|(index, _)| index)
}

fn imported(outline: &SourceOutline, row: Option<usize>) -> bool {
    row.is_some_and(|row| outline.import.iter().any(|range| range.contains(&row)))
}

fn objects(rows: &[DiffRow<'_>], old: &SourceOutline, new: &SourceOutline) -> Vec<ObjectRows> {
    let mut result: Vec<_> = (0..=new.scope.len())
        .map(|_| ObjectRows::default())
        .collect();
    let mut by_identity: HashMap<&str, Vec<usize>> = HashMap::new();
    for (index, scope) in new.scope.iter().enumerate().rev() {
        by_identity
            .entry(&scope.identity)
            .or_default()
            .push(index + 1);
    }
    let old_owner: Vec<_> = old
        .scope
        .iter()
        .map(|scope| {
            by_identity
                .get_mut(scope.identity.as_str())
                .and_then(Vec::pop)
                .unwrap_or_else(|| {
                    result.push(ObjectRows::default());
                    result.len() - 1
                })
        })
        .collect();
    for (index, row) in rows.iter().enumerate() {
        if imported(old, row.old) || imported(new, row.new) {
            continue;
        }
        let old_scope = owner(old, row.old);
        let new_scope = owner(new, row.new);
        let group = new_scope
            .map(|scope| scope + 1)
            .or_else(|| old_scope.map(|scope| old_owner[scope]))
            .unwrap_or(0);
        let object = &mut result[group];
        object.rows.push(index);
        let signature = old_scope
            .zip(row.old)
            .is_some_and(|(scope, line)| old.scope[scope].signature.contains(&line))
            || new_scope
                .zip(row.new)
                .is_some_and(|(scope, line)| new.scope[scope].signature.contains(&line));
        if signature {
            object.signature.push(index);
        }
        let closing = old_scope
            .zip(row.old)
            .is_some_and(|(scope, line)| old.scope[scope].closing == Some(line))
            || new_scope
                .zip(row.new)
                .is_some_and(|(scope, line)| new.scope[scope].closing == Some(line));
        if closing {
            object.closing.push(index);
        }
    }
    result.retain(|object| object.rows.iter().any(|index| rows[*index].changed()));
    result.sort_by_key(|object| object.rows.first().copied().unwrap_or(usize::MAX));
    result
}

fn emit_file(
    writer: &mut PromptWriter,
    file: &ChangedFile,
    old: &SourceOutline,
    new: &SourceOutline,
    document: bool,
) {
    let rows = rows(file.analysis.as_ref().unwrap());
    let groups = objects(&rows, old, new);
    let mut file_started = false;
    for object in groups {
        let mut selected = vec![false; rows.len()];
        for index in object.signature.iter().chain(&object.closing) {
            selected[*index] = true;
        }
        let mut changed = 0;
        for (position, index) in object.rows.iter().copied().enumerate() {
            if !rows[index].changed()
                || object.signature.contains(&index)
                || object.closing.contains(&index)
            {
                continue;
            }
            if !document && changed >= BODY_CHANGED_LINES {
                continue;
            }
            changed += 1;
            selected[index] = true;
            for backwards in [true, false] {
                let mut context = 0;
                let mut cursor = position;
                while context < CONTEXT_LINES {
                    cursor = if backwards {
                        let Some(previous) = cursor.checked_sub(1) else {
                            break;
                        };
                        previous
                    } else {
                        cursor + 1
                    };
                    let Some(neighbor) = object.rows.get(cursor).copied() else {
                        break;
                    };
                    if rows[neighbor].changed() {
                        if !selected[neighbor] {
                            break;
                        }
                    } else {
                        selected[neighbor] = true;
                        context += 1;
                    }
                }
            }
        }
        // Signatures are atomic at the byte boundary, even when spread over many rows.
        let mut prefix = if file_started {
            String::from("\n")
        } else {
            format!("\n{}\n\n", file.heading())
        };
        let first_body = object
            .rows
            .iter()
            .position(|index| !object.signature.contains(index))
            .unwrap_or(object.rows.len());
        for index in &object.rows[..first_body] {
            if selected[*index] {
                prefix.push_str(&rows[*index].render());
            }
        }
        let mut started = false;
        let mut gap = Vec::new();
        let mut position = first_body;
        while position < object.rows.len() {
            let index = object.rows[position];
            if selected[index] {
                if !gap.is_empty() {
                    prefix.push_str(&gap_text(&rows, &gap));
                    gap.clear();
                }
                let signature = object.signature.contains(&index);
                loop {
                    prefix.push_str(&rows[object.rows[position]].render());
                    position += 1;
                    if !signature
                        || position == object.rows.len()
                        || !object.signature.contains(&object.rows[position])
                    {
                        break;
                    }
                }
                if !writer.append(&prefix) {
                    return;
                }
                prefix.clear();
                started = true;
                file_started = true;
            } else {
                gap.push(index);
                position += 1;
            }
        }
        if !started && !prefix.is_empty() && !object.signature.is_empty() {
            if !writer.append(&prefix) {
                return;
            }
            started = true;
            file_started = true;
        }
        if started && !gap.is_empty() && !writer.append(&gap_text(&rows, &gap)) {
            return;
        }
    }
}

fn gap_text(rows: &[DiffRow<'_>], gap: &[usize]) -> String {
    let added = gap
        .iter()
        .filter(|index| rows[**index].marker == '+')
        .count();
    let removed = gap
        .iter()
        .filter(|index| rows[**index].marker == '-')
        .count();
    format!("  ... {} lines omitted (+{added} -{removed})\n", gap.len())
}

#[cfg(test)]
#[path = "excerpt_tests.rs"]
mod tests;
