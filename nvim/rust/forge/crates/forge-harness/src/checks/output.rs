/// Select the latest readable output line without diagnostic frames or source excerpts.
pub(super) fn latest_line(output: &str) -> Option<String> {
    let normalized = output.replace('\r', "\n");
    let plain = strip_ansi_escapes::strip(normalized.as_bytes());
    String::from_utf8_lossy(&plain)
        .rsplit(['\r', '\n'])
        .filter_map(message_line)
        .next()
        .map(str::to_owned)
}

/// Preserve message text while skipping decorative rows emitted by diagnostic renderers.
fn message_line(line: &str) -> Option<&str> {
    let line = line.trim();
    if line.is_empty()
        || line.chars().all(|character| {
            character.is_whitespace()
                || matches!(
                    character,
                    '`' | '\''
                        | ','
                        | '-'
                        | '_'
                        | '^'
                        | ':'
                        | '|'
                        | '─'
                        | '│'
                        | '└'
                        | '┌'
                        | '┐'
                        | '┘'
                        | '├'
                        | '┤'
                        | '┬'
                        | '┴'
                        | '┼'
                        | '╭'
                        | '╮'
                        | '╯'
                        | '╰'
                )
        })
    {
        return None;
    }
    if line.starts_with(",-[") || line.starts_with("╭─[") {
        return None;
    }
    if let Some((gutter, _)) = line.split_once(['|', '│']) {
        let gutter = gutter.trim();
        if !gutter.is_empty() && gutter.chars().all(|character| character.is_ascii_digit()) {
            return None;
        }
    }
    if line.starts_with(':') {
        return None;
    }
    let message = line
        .strip_prefix('|')
        .or_else(|| line.strip_prefix('│'))
        .unwrap_or(line)
        .trim();
    (!message.is_empty()).then_some(message)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep the diagnostic cause after its source annotation and closing frame arrive.
    #[test]
    fn diagnostic_frame_keeps_the_latest_message() {
        for output in [
            "Error: nu::shell::error\n  x protocol versions differ\n     ,-[check.nu:921:16]\n 920 | cleanup()\n 921 | error make {msg: failure}\n     : ^^^^^^^^^^^|^^^^^^^^^^^^\n     `----\n",
            "\u{1b}[31merror: protocol versions differ\u{1b}[0m\r\n  ╭─[check.nu:921:16]\r\n 921 │ fail()\r\n     │ ──────\r\n     ╰────\r\n",
        ] {
            assert!(
                latest_line(output)
                    .unwrap()
                    .contains("protocol versions differ")
            );
        }
        assert_eq!(latest_line("\n `----\n │ ───\n"), None);
        assert_eq!(
            latest_line("Building\rFinished dev profile\r\n"),
            Some("Finished dev profile".into())
        );
        assert_eq!(
            latest_line("error\n  | Evidence: /tmp/check\n `----"),
            Some("Evidence: /tmp/check".into())
        );
        assert_eq!(
            latest_line("progress\nhttps://example.com:443\n"),
            Some("https://example.com:443".into())
        );
    }
}
