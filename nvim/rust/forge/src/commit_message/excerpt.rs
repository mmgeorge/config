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
Added and removed functions include signatures only. \
Renamed files, license changes, and lockfile changes appear only in the file list. \
Standalone import changes are excluded from excerpts. Files without excerpts appear only in the file list. \
Describe visible changes without guessing the contents of omitted lines.\n\nChanged files:\n";

/// A complete file comparison retained until inventory and excerpt selection finish.
pub(super) struct ChangedFile {
    /// Repository-relative display path.
    pub path: String,
    /// Non-content changes such as relocation or file mode.
    pub note: String,
    /// Exact text comparison, absent for renames, binary files, or unsupported content.
    pub analysis: Option<AnalysisHandle>,
}

impl ChangedFile {
    fn heading(&self) -> String {
        let counts = self
            .analysis
            .as_ref()
            .map(|analysis| analysis.line_counts());
        let counts = counts.map(|counts| format!("+{} -{}", counts.added, counts.deleted));
        let path = self.path.replace('\n', "\\n").replace('\r', "\\r");
        let detail = match (counts, self.note.is_empty()) {
            (Some(counts), true) => counts,
            (Some(counts), false) => format!("{counts}, {}", self.note),
            (None, false) => self.note.clone(),
            (None, true) => "counts unavailable".into(),
        };
        format!("{path} ({detail})")
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
    changed: bool,
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
        if inventory_only(&file.path) {
            continue;
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

fn inventory_only(path: &str) -> bool {
    let filename = path
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or(path)
        .to_ascii_lowercase();
    let python_lock = filename
        .strip_prefix("pylock.")
        .and_then(|name| name.strip_suffix(".toml"))
        .is_some_and(|name| !name.is_empty() && !name.contains('.'));
    let julia_lock = filename
        .strip_prefix("manifest-v")
        .and_then(|name| name.strip_suffix(".toml"))
        .and_then(|version| version.split_once('.'))
        .is_some_and(|(major, minor)| {
            !major.is_empty()
                && !minor.is_empty()
                && major
                    .bytes()
                    .chain(minor.bytes())
                    .all(|byte| byte.is_ascii_digit())
        });
    filename.contains(".lock")
        || matches!(
            filename.as_str(),
            "license"
                | "license.md"
                | "license.txt"
                | "license.rst"
                | "package-lock.json"
                | ".package-lock.json"
                | "npm-shrinkwrap.json"
                | "pnpm-lock.yaml"
                | "package.resolved"
                | "pylock.toml"
                | "manifest.toml"
                | "juliamanifest.toml"
        )
        || (filename.contains("conda-lock")
            && (filename.ends_with(".yml") || filename.ends_with(".yaml")))
        || filename.ends_with(".project.freeze")
        || python_lock
        || julia_lock
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

fn signature_row_limit(
    outline: &SourceOutline,
    row: Option<usize>,
    signature_only: &[bool],
) -> Option<usize> {
    let row = row?;
    outline
        .scope
        .iter()
        .zip(signature_only)
        .filter(|(scope, only)| **only && scope.extent.contains(&row))
        .filter_map(|(scope, _)| {
            if !scope.signature.contains(&row) {
                Some(0)
            } else if row + 1 == scope.signature.end {
                Some(scope.signature_end_column)
            } else {
                None
            }
        })
        .min()
}

fn objects(
    rows: &[DiffRow<'_>],
    old: &SourceOutline,
    new: &SourceOutline,
) -> (Vec<ObjectRows>, Vec<bool>, Vec<bool>) {
    let mut result: Vec<_> = (0..=new.scope.len())
        .map(|_| ObjectRows::default())
        .collect();
    let owners: Vec<_> = rows
        .iter()
        .map(|row| (owner(old, row.old), owner(new, row.new)))
        .collect();
    let mut candidates = vec![None; old.scope.len()];
    let mut ambiguous = vec![false; old.scope.len()];
    for (row, (old_scope, new_scope)) in rows.iter().zip(&owners) {
        let (Some(old_scope), Some(new_scope), Some(old_line), Some(new_line)) =
            (*old_scope, *new_scope, row.old, row.new)
        else {
            continue;
        };
        if row.changed()
            || old.scope[old_scope].identity != new.scope[new_scope].identity
            || !old.scope[old_scope].signature.contains(&old_line)
            || !new.scope[new_scope].signature.contains(&new_line)
        {
            continue;
        }
        if candidates[old_scope].is_some_and(|candidate| candidate != new_scope) {
            ambiguous[old_scope] = true;
        }
        candidates[old_scope] = Some(new_scope);
    }
    let mut references = vec![0; new.scope.len()];
    for (candidate, ambiguous) in candidates.iter().zip(&ambiguous) {
        if let Some(candidate) = candidate.filter(|_| !ambiguous) {
            references[candidate] += 1;
        }
    }
    let mut used = vec![false; new.scope.len()];
    for (candidate, ambiguous) in candidates.iter_mut().zip(&ambiguous) {
        *candidate = candidate.filter(|candidate| !ambiguous && references[*candidate] == 1);
        if let Some(candidate) = candidate {
            used[*candidate] = true;
        }
    }
    let mut by_identity: HashMap<&str, Vec<usize>> = HashMap::new();
    for (index, scope) in new
        .scope
        .iter()
        .enumerate()
        .filter(|(index, _)| !used[*index])
    {
        by_identity
            .entry(&scope.identity)
            .or_default()
            .push(index + 1);
    }
    let mut remaining: HashMap<&str, usize> = HashMap::new();
    for (index, scope) in old.scope.iter().enumerate() {
        if candidates[index].is_none() {
            *remaining.entry(&scope.identity).or_default() += 1;
        }
    }
    let old_owner: Vec<_> = old
        .scope
        .iter()
        .enumerate()
        .map(|(index, scope)| {
            candidates[index]
                .map(|candidate| candidate + 1)
                .or_else(|| {
                    by_identity
                        .get_mut(scope.identity.as_str())
                        .filter(|available| {
                            available.len() == 1 && remaining[scope.identity.as_str()] == 1
                        })
                        .and_then(Vec::pop)
                })
                .unwrap_or_else(|| {
                    result.push(ObjectRows::default());
                    result.len() - 1
                })
        })
        .collect();
    let signature_only = old
        .scope
        .iter()
        .enumerate()
        .map(|(index, scope)| {
            scope.is_callable
                && old_owner[index] > new.scope.len()
                && !new
                    .scope
                    .iter()
                    .enumerate()
                    .any(|(candidate, declaration)| {
                        declaration.identity == scope.identity
                            && !old_owner.contains(&(candidate + 1))
                    })
                && rows.iter().any(|row| {
                    row.marker == '-' && row.old.is_some_and(|line| scope.signature.contains(&line))
                })
        })
        .collect();
    let new_signature_only = new
        .scope
        .iter()
        .enumerate()
        .map(|(index, scope)| {
            scope.is_callable
                && !old_owner.contains(&(index + 1))
                && !old
                    .scope
                    .iter()
                    .enumerate()
                    .any(|(candidate, declaration)| {
                        declaration.identity == scope.identity
                            && old_owner[candidate] > new.scope.len()
                    })
                && rows.iter().any(|row| {
                    row.marker == '+' && row.new.is_some_and(|line| scope.signature.contains(&line))
                })
        })
        .collect();
    for (index, (row, (old_scope, new_scope))) in rows.iter().zip(owners).enumerate() {
        if imported(old, row.old) || imported(new, row.new) {
            continue;
        }
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
        if row.changed() {
            result[group].changed = true;
            for (scope, declaration) in new.scope.iter().enumerate() {
                if row
                    .new
                    .is_some_and(|line| declaration.extent.contains(&line))
                {
                    result[scope + 1].changed = true;
                }
            }
            for (scope, declaration) in old.scope.iter().enumerate() {
                if row
                    .old
                    .is_some_and(|line| declaration.extent.contains(&line))
                {
                    result[old_owner[scope]].changed = true;
                }
            }
        }
    }
    result.retain(|object| object.changed);
    (result, signature_only, new_signature_only)
}

fn emit_file(
    writer: &mut PromptWriter,
    file: &ChangedFile,
    old: &SourceOutline,
    new: &SourceOutline,
    document: bool,
) {
    let mut rows = rows(file.analysis.as_ref().unwrap());
    let (groups, old_signature_only, new_signature_only) = objects(&rows, old, new);
    let mut selected = vec![false; rows.len()];
    let mut signature = vec![false; rows.len()];
    for object in &groups {
        for index in &object.signature {
            signature[*index] = true;
        }
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
                    let previous = if backwards { cursor + 1 } else { cursor - 1 };
                    if neighbor.abs_diff(object.rows[previous]) != 1 {
                        break;
                    }
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
    }
    for (index, row) in rows.iter_mut().enumerate() {
        let limit = if row.new.is_some() {
            signature_row_limit(new, row.new, &new_signature_only)
        } else {
            signature_row_limit(old, row.old, &old_signature_only)
        };
        if let Some(limit) = limit {
            if limit == 0 {
                selected[index] = false;
                signature[index] = false;
            } else if limit < row.text.len() {
                row.text = &row.text[..limit];
            }
        }
    }
    let mut signature_blocks = HashMap::new();
    let mut regrouped = vec![false; rows.len()];
    for object in &groups {
        let indices: Vec<_> = object
            .signature
            .iter()
            .copied()
            .filter(|index| selected[*index] && signature[*index])
            .collect();
        if !indices.iter().any(|index| rows[*index].marker == '-')
            || !indices.iter().any(|index| rows[*index].marker == '+')
            || indices.windows(2).all(|pair| pair[1] == pair[0] + 1)
        {
            continue;
        }
        let anchor = *indices
            .iter()
            .find(|index| rows[**index].new.is_some())
            .unwrap();
        for index in &indices {
            regrouped[*index] = true;
        }
        signature_blocks.insert(anchor, indices);
    }
    let mut prefix = format!("\n{}\n\n", file.heading());
    let mut started = false;
    let mut gap = Vec::new();
    let mut position = 0;
    while position < rows.len() {
        if let Some(indices) = signature_blocks.remove(&position) {
            if !gap.is_empty() {
                prefix.push_str(&gap_text(&rows, &gap));
                gap.clear();
            }
            for index in indices {
                prefix.push_str(&rows[index].render());
            }
            if !writer.append(&prefix) {
                return;
            }
            prefix.clear();
            started = true;
            position += 1;
            continue;
        }
        if regrouped[position]
            || imported(old, rows[position].old)
            || imported(new, rows[position].new)
        {
            position += 1;
            continue;
        }
        if !selected[position] {
            gap.push(position);
            position += 1;
            continue;
        }
        if !gap.is_empty() {
            prefix.push_str(&gap_text(&rows, &gap));
            gap.clear();
        }
        // Body rows keep raw diff order. A signature or paired signature block is atomic.
        let signature_chunk = signature[position];
        loop {
            prefix.push_str(&rows[position].render());
            position += 1;
            if !signature_chunk
                || position == rows.len()
                || !signature[position]
                || regrouped[position]
            {
                break;
            }
        }
        if !writer.append(&prefix) {
            return;
        }
        prefix.clear();
        started = true;
    }
    if started && !gap.is_empty() {
        writer.append(&gap_text(&rows, &gap));
    }
}

fn gap_text(rows: &[DiffRow<'_>], gap: &[usize]) -> String {
    if gap.iter().all(|index| rows[*index].text.trim().is_empty()) {
        return String::new();
    }
    if let [index] = gap {
        return rows[*index].render();
    }
    let added = gap
        .iter()
        .filter(|index| rows[**index].marker == '+')
        .count();
    let removed = gap
        .iter()
        .filter(|index| rows[**index].marker == '-')
        .count();
    format!("  ... {} lines (+{added} -{removed})\n", gap.len())
}

#[cfg(test)]
#[path = "excerpt_tests.rs"]
mod tests;
