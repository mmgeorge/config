//! Saved unified patches retain partial source coordinates without inventing complete files.

use std::borrow::Cow;
use std::fmt;
use std::iter::Peekable;
use std::ops::Range;
use std::str::SplitTerminator;

use crate::display::{RowCoordinates, RowKind};

#[derive(Debug, PartialEq, Eq)]
/// Rejects malformed input without returning a partial patch.
pub struct PatchParseError(pub &'static str);

impl fmt::Display for PatchParseError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.0)
    }
}

impl std::error::Error for PatchParseError {}

#[derive(Debug)]
/// Borrows patch bodies and owns decoded paths for display, never for applying edits.
pub struct UnifiedPatch<'source> {
    /// Files in their original patch order, including metadata-only changes.
    pub file: Vec<PatchFile<'source>>,
}

impl<'source> UnifiedPatch<'source> {
    /// Parses saved patches with exact hunk coordinates and borrowed source rows.
    ///
    /// Accepts Git extended patches and ordinary unified file headers. Invalid counts, paths,
    /// truncated hunks, and combined diffs return an error atomically.
    pub fn parse(source: &'source str) -> Result<Self, PatchParseError> {
        let mut result = Self { file: Vec::new() };
        let mut lines = source.split_terminator('\n').peekable();
        let mut source_header = false;
        while let Some(raw) = lines.next() {
            let line = raw.trim_end_matches('\r');
            if let Some(paths) = line.strip_prefix("diff --git ") {
                let (old, new) = git_paths(paths)?;
                let start = raw.as_ptr() as usize - source.as_ptr() as usize;
                if let Some(previous) = result.file.last_mut() {
                    previous.source_range.end = start;
                }
                result.file.push(PatchFile {
                    source_range: start..source.len(),
                    old_path: Some(old),
                    new_path: Some(new),
                    binary: false,
                    line_ending_changes: 0,
                    hunk: Vec::new(),
                });
                source_header = false;
            } else if let Some(path) = line.strip_prefix("--- ") {
                if source_header || result.file.is_empty() {
                    let start = raw.as_ptr() as usize - source.as_ptr() as usize;
                    if let Some(previous) = result.file.last_mut() {
                        previous.source_range.end = start;
                    }
                    result.file.push(PatchFile {
                        source_range: start..source.len(),
                        old_path: None,
                        new_path: None,
                        binary: false,
                        line_ending_changes: 0,
                        hunk: Vec::new(),
                    });
                }
                let file = result.file.last_mut().unwrap();
                file.old_path = source_path(path, "a/")?;
                let new = lines
                    .next()
                    .and_then(|line| line.trim_end_matches('\r').strip_prefix("+++ "))
                    .ok_or(PatchParseError(
                        "saved patch is missing its new-file header",
                    ))?;
                file.new_path = source_path(new, "b/")?;
                source_header = true;
            } else if line.starts_with("@@ ") {
                let file = result
                    .file
                    .last_mut()
                    .ok_or(PatchParseError("saved patch hunk has no file"))?;
                file.hunk.push(PatchHunk::parse(line, &mut lines)?);
            } else if line.starts_with("diff --cc ")
                || line.starts_with("diff --combined ")
                || line.starts_with("@@@")
            {
                return Err(PatchParseError("combined patches require a two-sided diff"));
            } else if let Some(file) = result.file.last_mut() {
                if line.starts_with("new file mode ") {
                    file.old_path = None;
                } else if line.starts_with("deleted file mode ") {
                    file.new_path = None;
                } else if let Some(path) = line
                    .strip_prefix("rename from ")
                    .or_else(|| line.strip_prefix("copy from "))
                {
                    file.old_path = Some(decode_path(path)?);
                } else if let Some(path) = line
                    .strip_prefix("rename to ")
                    .or_else(|| line.strip_prefix("copy to "))
                {
                    file.new_path = Some(decode_path(path)?);
                } else if line.starts_with("Binary files ") || line == "GIT binary patch" {
                    file.binary = true;
                } else if !file.binary
                    && !line.is_empty()
                    && !line.starts_with("index ")
                    && !line.starts_with("old mode ")
                    && !line.starts_with("new mode ")
                    && !line.starts_with("similarity index ")
                    && !line.starts_with("dissimilarity index ")
                {
                    return Err(PatchParseError(
                        "unexpected content outside a saved patch hunk",
                    ));
                }
            } else if !line.is_empty() {
                return Err(PatchParseError("saved patch is missing a file header"));
            }
        }
        if result
            .file
            .iter()
            .any(|file| file.old_path.is_none() && file.new_path.is_none())
        {
            return Err(PatchParseError("saved patch file has no source path"));
        }
        Ok(result)
    }

    /// Separate line-ending conversions from textual edits without changing the saved patch.
    pub fn display(mut self) -> Result<Self, crate::source::SourceError> {
        for file in &mut self.file {
            let mut display = Vec::new();
            for hunk in std::mem::take(&mut file.hunk) {
                let (hunks, line_ending_changes) = display_hunk(hunk)?;
                display.extend(hunks);
                file.line_ending_changes += line_ending_changes;
            }
            file.hunk = display;
        }
        Ok(self)
    }
}

#[derive(Debug)]
/// Owns file identity and borrows the available hunks of a saved change.
pub struct PatchFile<'source> {
    /// Exact file section in the original patch, including extended metadata and binary payloads.
    pub source_range: Range<usize>,
    /// Repository-relative old path, absent for file creation.
    pub old_path: Option<String>,
    /// Repository-relative new path, absent for file deletion.
    pub new_path: Option<String>,
    /// Binary content cannot be expanded as source rows.
    pub binary: bool,
    /// Matched text lines whose LF or CRLF terminator changed in the display comparison.
    pub line_ending_changes: usize,
    /// Hunks retain their absolute source positions and patch order.
    pub hunk: Vec<PatchHunk<'source>>,
}

impl PatchFile<'_> {
    /// Verify complete source coverage without reconstructing a file from sparse patch fragments.
    /// Both hunk bytes and all unchanged gaps must match. Memory use is constant in source length.
    pub fn matches_source(&self, source: &crate::source::SourcePair) -> bool {
        if self.binary
            || (self.old_path.is_none() && !source.old.bytes().is_empty())
            || (self.new_path.is_none() && !source.new.bytes().is_empty())
        {
            return false;
        }
        let mut old = source.old.text().split_inclusive('\n');
        let mut new = source.new.text().split_inclusive('\n');
        let mut old_position = 0;
        let mut new_position = 0;
        for hunk in &self.hunk {
            let Some(old_gap) = hunk.old_lines.start.checked_sub(old_position) else {
                return false;
            };
            let Some(new_gap) = hunk.new_lines.start.checked_sub(new_position) else {
                return false;
            };
            if old_gap != new_gap {
                return false;
            }
            for _ in 0..old_gap {
                match (old.next(), new.next()) {
                    (Some(old), Some(new)) if old == new => {}
                    _ => return false,
                }
            }
            old_position = hunk.old_lines.start;
            new_position = hunk.new_lines.start;
            for row in &hunk.row {
                let matches = |line: Option<&str>| {
                    line.is_some_and(|line| {
                        let text = line.strip_suffix('\n');
                        text.unwrap_or(line) == row.text && row.no_newline == text.is_none()
                    })
                };
                if let Some(position) = row.old_line() {
                    if position != old_position || !matches(old.next()) {
                        return false;
                    }
                    old_position += 1;
                }
                if let Some(position) = row.new_line() {
                    if position != new_position || !matches(new.next()) {
                        return false;
                    }
                    new_position += 1;
                }
            }
            if old_position != hunk.old_lines.end || new_position != hunk.new_lines.end {
                return false;
            }
        }
        old.eq(new)
    }
}

#[derive(Debug)]
/// Retains one validated hunk with zero-based source ranges.
pub struct PatchHunk<'source> {
    /// Absolute old-file line range, including unchanged context.
    pub old_lines: Range<usize>,
    /// Absolute new-file line range, including unchanged context.
    pub new_lines: Range<usize>,
    /// Original header, including any trailing function context.
    pub header: Cow<'source, str>,
    /// Source text excludes patch prefixes but preserves CRLF content bytes.
    pub row: Vec<PatchRow<'source>>,
}

impl<'source> PatchHunk<'source> {
    fn parse(
        header: &'source str,
        lines: &mut Peekable<SplitTerminator<'source, char>>,
    ) -> Result<Self, PatchParseError> {
        let (ranges, _) = header
            .strip_prefix("@@ ")
            .and_then(|value| value.split_once(" @@"))
            .ok_or(PatchParseError("invalid saved patch hunk header"))?;
        let (old, new) = ranges
            .split_once(' ')
            .ok_or(PatchParseError("invalid saved patch hunk ranges"))?;
        let old_lines = parse_range(old, '-')?;
        let new_lines = parse_range(new, '+')?;
        let mut old = old_lines.start;
        let mut new = new_lines.start;
        let mut row = Vec::new();
        while old < old_lines.end || new < new_lines.end {
            let line = lines
                .next()
                .ok_or(PatchParseError("truncated saved patch hunk"))?;
            let (kind, text) = match line.as_bytes().first() {
                Some(b' ') => (RowKind::Context, &line[1..]),
                Some(b'-') => (RowKind::Removed, &line[1..]),
                Some(b'+') => (RowKind::Added, &line[1..]),
                _ => return Err(PatchParseError("invalid saved patch body row")),
            };
            let old_line = (kind != RowKind::Added).then_some(old);
            let new_line = (kind != RowKind::Removed).then_some(new);
            let coordinates = match kind {
                RowKind::Added => RowCoordinates::Added { new_line: new },
                RowKind::Removed => RowCoordinates::Removed { old_line: old },
                RowKind::Context => RowCoordinates::Context {
                    old_line: old,
                    new_line: new,
                },
            };
            if old_line.is_some() && old == old_lines.end
                || new_line.is_some() && new == new_lines.end
            {
                return Err(PatchParseError("saved patch body exceeds its hunk range"));
            }
            old += usize::from(old_line.is_some());
            new += usize::from(new_line.is_some());
            let no_newline = lines
                .peek()
                .is_some_and(|line| line.trim_end_matches('\r') == "\\ No newline at end of file");
            if no_newline {
                lines.next();
            }
            row.push(PatchRow {
                text,
                coordinates,
                no_newline,
            });
        }
        if row.is_empty() {
            return Err(PatchParseError("saved patch hunk has no source rows"));
        }
        Ok(Self {
            old_lines,
            new_lines,
            header: Cow::Borrowed(header),
            row,
        })
    }
}

#[derive(Clone, Debug)]
/// Borrows one source row with its exact old and new file coordinates.
pub struct PatchRow<'source> {
    /// Original content without the unified-diff prefix or line feed.
    pub text: &'source str,
    coordinates: RowCoordinates,
    /// The patch explicitly marks this source row as lacking a final newline.
    pub no_newline: bool,
}

impl PatchRow<'_> {
    pub fn coordinates(&self) -> RowCoordinates {
        self.coordinates
    }

    pub fn kind(&self) -> RowKind {
        self.coordinates.kind()
    }
    pub fn old_line(&self) -> Option<usize> {
        self.coordinates.old_line()
    }
    pub fn new_line(&self) -> Option<usize> {
        self.coordinates.new_line()
    }
}

fn display_hunk(
    hunk: PatchHunk<'_>,
) -> Result<(Vec<PatchHunk<'_>>, usize), crate::source::SourceError> {
    if !hunk
        .row
        .iter()
        .any(|row| row.kind() != RowKind::Context && !row.no_newline && row.text.ends_with('\r'))
    {
        return Ok((vec![hunk], 0));
    }
    let old: Vec<_> = hunk
        .row
        .iter()
        .filter(|row| row.kind() != RowKind::Added)
        .collect();
    let new: Vec<_> = hunk
        .row
        .iter()
        .filter(|row| row.kind() != RowKind::Removed)
        .collect();
    let source = |rows: &[&PatchRow<'_>]| {
        let mut text = String::new();
        for row in rows {
            if row.no_newline {
                text.push_str(row.text);
            } else {
                text.push_str(row.text.strip_suffix('\r').unwrap_or(row.text));
                text.push('\n');
            }
        }
        crate::source::SourceVersion::new(
            text.into_bytes(),
            crate::source::Representation::DisplayOnly,
        )
    };
    let diff = crate::raw::compute_hunks(crate::source::SourcePair {
        old: source(&old)?,
        new: source(&new)?,
    });
    let mut old_position = 0;
    let mut new_position = 0;
    let mut line_ending_changes = 0;
    for change in diff.hunks() {
        while old_position < change.old_lines.start && new_position < change.new_lines.start {
            line_ending_changes += usize::from(old[old_position].text != new[new_position].text);
            old_position += 1;
            new_position += 1;
        }
        old_position = change.old_lines.end;
        new_position = change.new_lines.end;
    }
    while old_position < old.len() && new_position < new.len() {
        line_ending_changes += usize::from(old[old_position].text != new[new_position].text);
        old_position += 1;
        new_position += 1;
    }
    if line_ending_changes == 0 {
        return Ok((vec![hunk], 0));
    }
    let mut display = Vec::new();
    for group in crate::display::group_hunks(&diff, 3) {
        let old_lines = hunk.old_lines.start + group.old_lines.start
            ..hunk.old_lines.start + group.old_lines.end;
        let new_lines = hunk.new_lines.start + group.new_lines.start
            ..hunk.new_lines.start + group.new_lines.end;
        let range = |lines: &Range<usize>| {
            format!(
                "{},{}",
                lines.start + usize::from(!lines.is_empty()),
                lines.len()
            )
        };
        let mut rows = Vec::new();
        let mut old_position = group.old_lines.start;
        let mut new_position = group.new_lines.start;
        for change in &diff.hunks()[group.raw_range] {
            while old_position < change.old_lines.start {
                let mut row = new[new_position].clone();
                row.coordinates = RowCoordinates::Context {
                    old_line: hunk.old_lines.start + old_position,
                    new_line: hunk.new_lines.start + new_position,
                };
                rows.push(row);
                old_position += 1;
                new_position += 1;
            }
            for (index, row) in old[change.old_lines.clone()].iter().enumerate() {
                let mut row = (*row).clone();
                row.coordinates = RowCoordinates::Removed {
                    old_line: hunk.old_lines.start + change.old_lines.start + index,
                };
                rows.push(row);
            }
            for (index, row) in new[change.new_lines.clone()].iter().enumerate() {
                let mut row = (*row).clone();
                row.coordinates = RowCoordinates::Added {
                    new_line: hunk.new_lines.start + change.new_lines.start + index,
                };
                rows.push(row);
            }
            old_position = change.old_lines.end;
            new_position = change.new_lines.end;
        }
        while old_position < group.old_lines.end {
            let mut row = new[new_position].clone();
            row.coordinates = RowCoordinates::Context {
                old_line: hunk.old_lines.start + old_position,
                new_line: hunk.new_lines.start + new_position,
            };
            rows.push(row);
            old_position += 1;
            new_position += 1;
        }
        display.push(PatchHunk {
            header: Cow::Owned(format!(
                "@@ -{} +{} @@",
                range(&old_lines),
                range(&new_lines)
            )),
            old_lines,
            new_lines,
            row: rows,
        });
    }
    Ok((display, line_ending_changes))
}

fn parse_range(text: &str, prefix: char) -> Result<Range<usize>, PatchParseError> {
    let text = text
        .strip_prefix(prefix)
        .ok_or(PatchParseError("invalid saved patch range sign"))?;
    let (start, count) = text.split_once(',').unwrap_or((text, "1"));
    let start = start
        .parse::<usize>()
        .map_err(|_| PatchParseError("invalid saved patch line number"))?;
    let count = count
        .parse::<usize>()
        .map_err(|_| PatchParseError("invalid saved patch line count"))?;
    let start = if count > 0 {
        start
            .checked_sub(1)
            .ok_or(PatchParseError("nonempty saved patch range starts at zero"))?
    } else {
        start
    };
    let end = start
        .checked_add(count)
        .ok_or(PatchParseError("saved patch range overflow"))?;
    Ok(start..end)
}

fn source_path(text: &str, prefix: &str) -> Result<Option<String>, PatchParseError> {
    let path = decode_path(text.split('\t').next().unwrap_or(text))?;
    Ok((path != "/dev/null").then(|| path.strip_prefix(prefix).unwrap_or(&path).to_owned()))
}

fn git_paths(text: &str) -> Result<(String, String), PatchParseError> {
    let (old, new) = if text.starts_with('"') {
        let end = quoted_end(text)?;
        (&text[..end], text[end..].trim_start())
    } else if let Some((old, _)) = text.rsplit_once(" b/") {
        (old, &text[old.len() + 1..])
    } else if let Some((old, _)) = text.split_once(" \"") {
        (old, &text[old.len() + 1..])
    } else {
        return Err(PatchParseError("invalid Git patch file paths"));
    };
    let old = source_path(old, "a/")?.ok_or(PatchParseError("Git patch old path is missing"))?;
    let new = source_path(new, "b/")?.ok_or(PatchParseError("Git patch new path is missing"))?;
    Ok((old, new))
}

fn quoted_end(text: &str) -> Result<usize, PatchParseError> {
    let mut escaped = false;
    for (index, byte) in text.bytes().enumerate().skip(1) {
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == b'"' {
            return Ok(index + 1);
        }
    }
    Err(PatchParseError("unterminated quoted patch path"))
}

fn decode_path(text: &str) -> Result<String, PatchParseError> {
    if text.is_empty() {
        return Err(PatchParseError("empty saved patch path"));
    }
    if text.contains('\0') {
        return Err(PatchParseError("saved patch path contains NUL"));
    }
    if !text.starts_with('"') {
        return Ok(text.to_owned());
    }
    let end = quoted_end(text)?;
    if end != text.len() {
        return Err(PatchParseError("unexpected suffix after quoted patch path"));
    }
    let mut bytes = text.as_bytes()[1..end - 1].iter().copied();
    let mut result = Vec::new();
    while let Some(byte) = bytes.next() {
        if byte != b'\\' {
            result.push(byte);
            continue;
        }
        let escaped = bytes
            .next()
            .ok_or(PatchParseError("truncated patch path escape"))?;
        result.push(match escaped {
            b'\\' | b'"' => escaped,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'a' => 7,
            b'b' => 8,
            b'f' => 12,
            b'v' => 11,
            b'0'..=b'3' => {
                let middle = bytes
                    .next()
                    .filter(|byte| (b'0'..=b'7').contains(byte))
                    .ok_or(PatchParseError("invalid octal patch path escape"))?;
                let last = bytes
                    .next()
                    .filter(|byte| (b'0'..=b'7').contains(byte))
                    .ok_or(PatchParseError("invalid octal patch path escape"))?;
                (escaped - b'0') * 64 + (middle - b'0') * 8 + (last - b'0')
            }
            _ => return Err(PatchParseError("unknown patch path escape")),
        });
    }
    String::from_utf8(result).map_err(|_| PatchParseError("saved patch path is not UTF-8"))
}

#[cfg(test)]
mod source_coverage_tests {
    use super::*;
    use crate::source::{Representation, SourcePair, SourceVersion};

    fn source(old: &str, new: &str) -> SourcePair {
        SourcePair {
            old: SourceVersion::new(old.as_bytes().to_vec(), Representation::Raw).unwrap(),
            new: SourceVersion::new(new.as_bytes().to_vec(), Representation::Raw).unwrap(),
        }
    }

    #[test]
    fn saved_patch_source_coverage_checks_unchanged_gaps_and_exact_terminators() {
        let patch =
            UnifiedPatch::parse("--- a/source.rs\n+++ b/source.rs\n@@ -2 +2 @@\n-before\n+after\n")
                .unwrap();
        assert!(patch.file[0].matches_source(&source(
            "prefix\nbefore\nsuffix\n",
            "prefix\nafter\nsuffix\n"
        )));
        for pair in [
            source("prefix\nbefore\nsuffix\n", "other\nafter\nsuffix\n"),
            source("prefix\nbefore\nsuffix\n", "prefix\nafter\nother\n"),
            source("prefix\nbefore\nsuffix\n", "prefix\nafter\nsuffix"),
            source("prefix\nwrong\nsuffix\n", "prefix\nafter\nsuffix\n"),
            source("prefix\nbefore\nsuffix\n", "prefix\nafter\n"),
        ] {
            assert!(!patch.file[0].matches_source(&pair));
        }
        let crlf = UnifiedPatch::parse(
            "--- a/source.rs\n+++ b/source.rs\n@@ -1 +1 @@\n-before\r\n+after\r\n",
        )
        .unwrap();
        assert!(crlf.file[0].matches_source(&source("before\r\n", "after\r\n")));
        assert!(!crlf.file[0].matches_source(&source("before\n", "after\n")));
        let eof = UnifiedPatch::parse("--- a/source.rs\n+++ b/source.rs\n@@ -1 +1 @@\n-before\n\\ No newline at end of file\n+after\n\\ No newline at end of file\n").unwrap();
        assert!(eof.file[0].matches_source(&source("before", "after")));
        assert!(!eof.file[0].matches_source(&source("before\n", "after")));
    }
}
