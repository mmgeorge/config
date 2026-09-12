use std::io::{self, Write};
use std::process::{Command, Stdio};

use forge_diff::raw::compute_hunks;
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::unified::{write_file_header, write_unified};

struct LimitedOutput {
    remaining: usize,
}

impl Write for LimitedOutput {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        if self.remaining == 0 {
            return Err(io::Error::other("injected output limit"));
        }
        let accepted = bytes.len().min(self.remaining);
        self.remaining -= accepted;
        Ok(accepted)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn git_applies_and_reverses_exact_unified_bytes_including_crlf_and_missing_newlines() {
    let separated_old = (0..30)
        .map(|line| format!("line {line}\n"))
        .collect::<String>();
    let separated_new = separated_old
        .replace("line 5\n", "changed five\n")
        .replace("line 25\n", "changed twenty-five\n");
    let large_old = "old line\n".repeat(1500);
    let large_new = "new line\n".repeat(1500);
    for (before, after) in [
        ("old\n", "new\n"),
        ("old\r\nstable\r\nlast", "new\r\nstable\r\nlast"),
        ("before", "after\n"),
        ("", "added\n"),
        ("removed", ""),
        (separated_old.as_str(), separated_new.as_str()),
        (large_old.as_str(), large_new.as_str()),
    ] {
        let directory = tempfile::tempdir().unwrap();
        let initialized = Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(directory.path())
            .output()
            .unwrap();
        assert!(initialized.status.success());
        let path = directory.path().join("source name 雪.txt");
        if !before.is_empty() {
            std::fs::write(&path, before).unwrap();
        }
        let diff = compute_hunks(SourcePair {
            old: SourceVersion::new(before.as_bytes().to_vec(), Representation::Raw).unwrap(),
            new: SourceVersion::new(after.as_bytes().to_vec(), Representation::Raw).unwrap(),
        })
        .unwrap();
        let mut patch = Vec::new();
        write_file_header("a/source name 雪.txt", "b/source name 雪.txt", &mut patch).unwrap();
        if before.is_empty() {
            patch.extend_from_slice(b"new file mode 100644\n");
        } else if after.is_empty() {
            patch.extend_from_slice(b"deleted file mode 100644\n");
        }
        write_unified(
            &diff,
            if before.is_empty() {
                "/dev/null"
            } else {
                "a/source name 雪.txt"
            },
            if after.is_empty() {
                "/dev/null"
            } else {
                "b/source name 雪.txt"
            },
            3,
            &mut patch,
        )
        .unwrap();
        for reverse in [false, true] {
            let mut command = Command::new("git");
            command.arg("apply").arg("--whitespace=nowarn");
            if reverse {
                command.arg("--reverse");
            }
            let mut child = command
                .current_dir(directory.path())
                .stdin(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            child.stdin.take().unwrap().write_all(&patch).unwrap();
            let output = child.wait_with_output().unwrap();
            assert!(
                output.status.success(),
                "reverse={reverse}, before={before:?}, after={after:?}: {}\n{}",
                String::from_utf8_lossy(&output.stderr),
                String::from_utf8_lossy(&patch)
            );
            let expected = if reverse { before } else { after };
            if expected.is_empty() {
                assert!(!path.exists());
            } else {
                assert_eq!(std::fs::read(&path).unwrap(), expected.as_bytes());
            }
        }
    }
}

#[test]
fn writer_errors_stop_serialization_and_unchanged_sources_emit_nothing() {
    let old = SourceVersion::new(b"same\n".to_vec(), Representation::Raw).unwrap();
    let equal = compute_hunks(SourcePair {
        old: old.clone(),
        new: old.clone(),
    })
    .unwrap();
    write_unified(
        &equal,
        "a/source",
        "b/source",
        3,
        &mut LimitedOutput { remaining: 0 },
    )
    .unwrap();
    let changed = compute_hunks(SourcePair {
        old,
        new: SourceVersion::new(b"changed\n".to_vec(), Representation::Raw).unwrap(),
    })
    .unwrap();
    let error = write_unified(
        &changed,
        "a/source",
        "b/source",
        3,
        &mut LimitedOutput { remaining: 40 },
    )
    .unwrap_err();
    assert_eq!(error.to_string(), "injected output limit");
}

#[test]
fn file_headings_quote_control_characters_without_creating_extra_lines() {
    let mut output = Vec::new();
    write_file_header("a/line\nname\t\"\\", "b/line\rname", &mut output).unwrap();
    assert_eq!(
        String::from_utf8(output).unwrap(),
        "diff --git \"a/line\\nname\\t\\\"\\\\\" \"b/line\\rname\"\n"
    );
}
