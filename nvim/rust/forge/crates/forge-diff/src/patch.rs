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
                result.file.push(PatchFile {
                    old_path: Some(old),
                    new_path: Some(new),
                    binary: false,
                    line_ending_changes: 0,
                    hunk: Vec::new(),
                });
                source_header = false;
            } else if let Some(path) = line.strip_prefix("--- ") {
                if source_header || result.file.is_empty() {
                    result.file.push(PatchFile {
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
mod tests {
    use super::*;
    use crate::raw::compute_hunks;
    use crate::source::{Representation, SourcePair, SourceVersion};
    use crate::unified::{write_file_header, write_unified};

    #[test]
    fn display_separates_line_endings_from_large_text_changes() {
        let old = (0..5000)
            .map(|index| format!("line {index}\n"))
            .collect::<String>();
        let new = old
            .replace("line 2500\n", "edited 2500\n")
            .replace('\n', "\r\n");
        let raw = compute_hunks(SourcePair {
            old: SourceVersion::new(old.into_bytes(), Representation::Raw).unwrap(),
            new: SourceVersion::new(new.into_bytes(), Representation::Raw).unwrap(),
        });
        let mut bytes = Vec::new();
        write_file_header("a/source.rs", "b/source.rs", &mut bytes).unwrap();
        write_unified(&raw, "a/source.rs", "b/source.rs", 3, &mut bytes).unwrap();
        let source = String::from_utf8(bytes).unwrap();
        let saved = UnifiedPatch::parse(&source).unwrap();
        assert_eq!(saved.file[0].hunk[0].row.len(), 10000);
        let display = saved.display().unwrap();
        assert_eq!(display.file[0].line_ending_changes, 4999);
        let hunk = &display.file[0].hunk[0];
        assert_eq!(hunk.old_lines, 2497..2504);
        assert_eq!(hunk.new_lines, 2497..2504);
        assert_eq!(hunk.row.len(), 8);
        assert_eq!(
            hunk.row
                .iter()
                .filter(|row| row.kind() == RowKind::Added)
                .count(),
            1
        );
        assert_eq!(
            hunk.row
                .iter()
                .filter(|row| row.kind() == RowKind::Removed)
                .count(),
            1
        );
        assert_eq!(hunk.row[3].old_line(), Some(2500));
        assert_eq!(hunk.row[4].new_line(), Some(2500));
        assert_eq!(
            UnifiedPatch::parse(&source).unwrap().file[0].hunk[0]
                .row
                .len(),
            10000
        );
    }

    #[test]
    fn display_retains_line_ending_only_files_and_terminal_content_changes() {
        let source =
            "--- a/file\n+++ b/file\n@@ -90,2 +90,2 @@\n-first\n-last\n+first\r\n+last\r\n";
        let display = UnifiedPatch::parse(source).unwrap().display().unwrap();
        assert_eq!(display.file[0].line_ending_changes, 2);
        assert!(display.file[0].hunk.is_empty());
        let source = "--- a/file\n+++ b/file\n@@ -1,2 +1,2 @@\n-first\n-last\n\\ No newline at end of file\n+first\r\n+last\r\n\\ No newline at end of file\n";
        let display = UnifiedPatch::parse(source).unwrap().display().unwrap();
        assert_eq!(display.file[0].line_ending_changes, 1);
        assert_eq!(
            display.file[0].hunk[0]
                .row
                .iter()
                .filter(|row| row.kind() == RowKind::Added)
                .count(),
            1
        );
        assert_eq!(
            display.file[0].hunk[0]
                .row
                .iter()
                .filter(|row| row.kind() == RowKind::Removed)
                .count(),
            1
        );
    }

    #[test]
    fn generated_patches_preserve_paths_source_bytes_and_missing_newlines() {
        for (before, after) in [
            ("old\r\n", "new\r\n"),
            ("", "created"),
            ("deleted", ""),
            ("--- old\n", "+++ new"),
        ] {
            let raw = compute_hunks(SourcePair {
                old: SourceVersion::new(before.as_bytes().to_vec(), Representation::GitCanonical)
                    .unwrap(),
                new: SourceVersion::new(after.as_bytes().to_vec(), Representation::GitCanonical)
                    .unwrap(),
            });
            let mut bytes = Vec::new();
            write_file_header("a/folder/a\t中.rs", "b/folder/a\t中.rs", &mut bytes).unwrap();
            write_unified(
                &raw,
                if before.is_empty() {
                    "/dev/null"
                } else {
                    "a/folder/a\t中.rs"
                },
                if after.is_empty() {
                    "/dev/null"
                } else {
                    "b/folder/a\t中.rs"
                },
                3,
                &mut bytes,
            )
            .unwrap();
            let source = String::from_utf8(bytes).unwrap();
            let patch = UnifiedPatch::parse(&source).unwrap();
            let file = &patch.file[0];
            assert_eq!(
                file.old_path.as_deref(),
                (!before.is_empty()).then_some("folder/a\t中.rs")
            );
            assert_eq!(
                file.new_path.as_deref(),
                (!after.is_empty()).then_some("folder/a\t中.rs")
            );
            let mut reconstructed_old = String::new();
            let mut reconstructed_new = String::new();
            for row in file.hunk.iter().flat_map(|hunk| &hunk.row) {
                for output in [
                    row.old_line().map(|_| &mut reconstructed_old),
                    row.new_line().map(|_| &mut reconstructed_new),
                ]
                .into_iter()
                .flatten()
                {
                    output.push_str(row.text);
                    if !row.no_newline {
                        output.push('\n');
                    }
                }
            }
            assert_eq!(reconstructed_old, before);
            assert_eq!(reconstructed_new, after);
        }
    }

    #[test]
    fn partial_hunks_keep_absolute_coordinates_and_do_not_reparse_source_headers() {
        let patch = UnifiedPatch::parse("--- a/file\n+++ b/file\n@@ -100,2 +120,2 @@ fn example\n--- source\n+++ source\n context\n@@ -900 +920 @@\n-old\n+new\n").unwrap();
        assert_eq!(patch.file.len(), 1);
        let hunk = &patch.file[0].hunk[0];
        assert_eq!(hunk.old_lines, 99..101);
        assert_eq!(hunk.new_lines, 119..121);
        assert_eq!(hunk.row[0].text, "-- source");
        assert_eq!(hunk.row[1].text, "++ source");
        assert_eq!(hunk.row[2].old_line(), Some(100));
        assert_eq!(hunk.row[2].new_line(), Some(120));
        assert_eq!(patch.file[0].hunk[1].row[0].old_line(), Some(899));
    }

    #[test]
    fn metadata_only_files_and_octal_unicode_paths_remain_visible() {
        let source = "diff --git a/old name b/new name\nsimilarity index 100%\nrename from old name\nrename to new name\ndiff --git a/image b/image\nnew file mode 100644\nBinary files /dev/null and b/image differ\ndiff --git \"a/\\344\\270\\255.rs\" \"b/\\344\\270\\255.rs\"\nold mode 100644\nnew mode 100755\n";
        let patch = UnifiedPatch::parse(source).unwrap();
        assert_eq!(patch.file.len(), 3);
        assert_eq!(patch.file[0].old_path.as_deref(), Some("old name"));
        assert_eq!(patch.file[0].new_path.as_deref(), Some("new name"));
        assert!(patch.file[1].binary);
        assert!(patch.file[1].old_path.is_none());
        assert_eq!(patch.file[2].new_path.as_deref(), Some("中.rs"));
        assert!(patch.file.iter().all(|file| file.hunk.is_empty()));
    }

    #[test]
    fn malformed_counts_fail_without_partial_results_or_panics() {
        let prefix = "--- a/file\n+++ b/file\n";
        for body in [
            "@@ -1,2 +1,2 @@\n one\n",
            "@@ -0 +1 @@\n-old\n+new\n",
            "@@ -1 +1 @@\n-old\n-extra\n+new\n",
            "@@ -1 +1 @@\n-old\n+new\n+extra\n",
            "@@@ -1 -1 +1 @@@\n",
            "@@ -18446744073709551615,2 +1 @@\n-old\n+new\n",
        ] {
            assert!(
                UnifiedPatch::parse(&format!("{prefix}{body}")).is_err(),
                "{body}"
            );
        }
    }

    #[test]
    fn large_patches_retain_all_source_rows_and_files() {
        let line = "x".repeat(8 * 1024 * 1024 + 1);
        let source = format!("--- a/file\n+++ b/file\n@@ -0,0 +1 @@\n+{line}\n");
        let patch = UnifiedPatch::parse(&source).unwrap();
        assert_eq!(patch.file[0].hunk[0].row[0].text, line);
        let files = "diff --git a/file b/file\n".repeat(4097);
        assert_eq!(UnifiedPatch::parse(&files).unwrap().file.len(), 4097);
        let rows = format!(
            "--- a/file\n+++ b/file\n@@ -0,0 +1,{} @@\n{}",
            262_145,
            "+x\n".repeat(262_145)
        );
        assert_eq!(
            UnifiedPatch::parse(&rows).unwrap().file[0].hunk[0]
                .row
                .len(),
            262_145
        );
    }
}
