//! Unified preview serialization reuses exact raw changes and preserves source line bytes.

use std::io::{self, Write};
use std::ops::Range;

use crate::display::group_hunks;
use crate::raw::RawDiff;

struct SourceLines<'source> {
    remaining: std::str::SplitInclusive<'source, char>,
    position: usize,
}

/// Writes a unified diff from existing raw analysis without building a second comparison.
///
/// Headers name each source, including `/dev/null` for an absent file. Quoting preserves control
/// characters and whitespace in names. Source lines retain CRLF and missing-final-newline markers.
/// Callers supply file modes and other repository metadata when constructing Git extended patches.
/// Writer errors propagate immediately and may leave partial output. This accepts raw preview
/// sources and does not establish eligibility for repository mutations.
pub fn write_unified(
    diff: &RawDiff,
    before_header: &str,
    after_header: &str,
    context: usize,
    output: &mut impl Write,
) -> io::Result<()> {
    if diff.hunks().is_empty() {
        return Ok(());
    }
    output.write_all(b"--- ")?;
    write_path(before_header, output)?;
    output.write_all(b"\n+++ ")?;
    write_path(after_header, output)?;
    output.write_all(b"\n")?;
    let mut old = SourceLines {
        remaining: diff.source().old.text().split_inclusive('\n'),
        position: 0,
    };
    let mut new = SourceLines {
        remaining: diff.source().new.text().split_inclusive('\n'),
        position: 0,
    };
    for group in group_hunks(diff, context) {
        output.write_all(b"@@ -")?;
        write_range(&group.old_lines, output)?;
        output.write_all(b" +")?;
        write_range(&group.new_lines, output)?;
        output.write_all(b" @@\n")?;
        old.skip_to(group.old_lines.start);
        new.skip_to(group.new_lines.start);
        for hunk in &diff.hunks()[group.raw_range] {
            old.write_to(hunk.old_lines.start, b' ', output)?;
            new.skip_to(hunk.new_lines.start);
            old.write_to(hunk.old_lines.end, b'-', output)?;
            new.write_to(hunk.new_lines.end, b'+', output)?;
        }
        old.write_to(group.old_lines.end, b' ', output)?;
        new.skip_to(group.new_lines.end);
    }
    Ok(())
}

/// Writes the Git file heading using the same quoting as unified source headers.
pub fn write_file_header(before: &str, after: &str, output: &mut impl Write) -> io::Result<()> {
    output.write_all(b"diff --git ")?;
    write_path(before, output)?;
    output.write_all(b" ")?;
    write_path(after, output)?;
    output.write_all(b"\n")
}

impl SourceLines<'_> {
    fn skip_to(&mut self, target: usize) {
        for _ in self.position..target {
            self.remaining.next().expect("raw source line missing");
        }
        self.position = target;
    }

    fn write_to(&mut self, target: usize, prefix: u8, output: &mut impl Write) -> io::Result<()> {
        for _ in self.position..target {
            let line = self.remaining.next().expect("raw source line missing");
            output.write_all(&[prefix])?;
            output.write_all(line.as_bytes())?;
            if !line.ends_with('\n') {
                output.write_all(b"\n\\ No newline at end of file\n")?;
            }
        }
        self.position = target;
        Ok(())
    }
}

fn write_range(range: &Range<usize>, output: &mut impl Write) -> io::Result<()> {
    let start = range.start + usize::from(!range.is_empty());
    write!(output, "{start},{}", range.len())
}

fn write_path(path: &str, output: &mut impl Write) -> io::Result<()> {
    if !path.bytes().any(|byte| {
        byte.is_ascii_whitespace() || byte.is_ascii_control() || matches!(byte, b'"' | b'\\')
    }) {
        return output.write_all(path.as_bytes());
    }
    output.write_all(b"\"")?;
    for byte in path.bytes() {
        match byte {
            b'"' => output.write_all(b"\\\"")?,
            b'\\' => output.write_all(b"\\\\")?,
            b'\n' => output.write_all(b"\\n")?,
            b'\r' => output.write_all(b"\\r")?,
            b'\t' => output.write_all(b"\\t")?,
            byte if byte.is_ascii_control() => write!(output, "\\{byte:03o}")?,
            byte => output.write_all(&[byte])?,
        }
    }
    output.write_all(b"\"")
}
