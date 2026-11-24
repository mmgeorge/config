/// Right-align a tool duration in the transcript's five-cell column.
pub(super) fn tool_duration(milliseconds: Option<u64>) -> String {
    let Some(milliseconds) = milliseconds else {
        return "    —".into();
    };
    let label = duration_label(milliseconds);
    if label.len() <= 5 {
        return format!("{label:>5}");
    }
    format!("{:>4.0e}d", milliseconds as f64 / 86_400_000.0)
}

/// Show sub-second tool durations in milliseconds.
pub(super) fn duration_label(milliseconds: u64) -> String {
    if milliseconds < 1000 {
        return format!("{milliseconds}ms");
    }
    elapsed_label(milliseconds / 1000)
}

/// Show completed seconds below one minute and tenths of larger units thereafter.
pub(super) fn elapsed_label(seconds: u64) -> String {
    let (divisor, unit) = match seconds {
        0..60 => return format!("{seconds}s"),
        60..3600 => (60, 'm'),
        3600..86400 => (3600, 'h'),
        _ => (86400, 'd'),
    };
    let whole = seconds / divisor;
    let tenth = (seconds % divisor) * 10 / divisor;
    if tenth == 0 {
        format!("{whole}{unit}")
    } else {
        format!("{whole}.{tenth}{unit}")
    }
}

/// The earliest presentation instant at which a running elapsed label shows a new value.
///
/// Each label renders whole seconds of its own clock, and clocks start at unrelated instants,
/// so every clock contributes its own next boundary. The presentation refreshes its labels when
/// that instant passes and tells the editor how long to wait for it, so neither side samples on
/// a fixed period that can drift across a boundary and skip a displayed second.
#[derive(Debug, Default)]
pub(super) struct LabelDue {
    due_ms: Option<i64>,
}

impl LabelDue {
    /// Count a label showing whole seconds of a clock that reads `elapsed_ms` at `now_ms`.
    pub fn elapsed(&mut self, now_ms: i64, elapsed_ms: u64) {
        self.at(now_ms.saturating_add(1000 - (elapsed_ms % 1000) as i64));
    }

    /// Count a countdown showing whole seconds, rounded up, until `deadline_ms`.
    pub fn countdown(&mut self, now_ms: i64, deadline_ms: i64) {
        let remaining = deadline_ms.saturating_sub(now_ms);
        if remaining > 0 {
            self.at(now_ms.saturating_add((remaining - 1) % 1000 + 1));
        }
    }

    pub fn due_ms(&self) -> Option<i64> {
        self.due_ms
    }

    fn at(&mut self, due_ms: i64) {
        self.due_ms = Some(self.due_ms.map_or(due_ms, |current| current.min(due_ms)));
    }
}

#[cfg(test)]
mod tests {
    /// Tool rows and headings agree on either side of each second boundary.
    #[test]
    fn tool_rows_and_headings_show_completed_seconds() {
        for (milliseconds, row) in [
            (0, "  0ms"),
            (92, " 92ms"),
            (999, "999ms"),
            (1_000, "   1s"),
            (1_999, "   1s"),
            (2_000, "   2s"),
            (14_300, "  14s"),
            (59_999, "  59s"),
            (60_000, "   1m"),
            (90_000, " 1.5m"),
            (150_000, " 2.5m"),
            (3_599_999, "59.9m"),
            (3_600_000, "   1h"),
        ] {
            assert_eq!(super::tool_duration(Some(milliseconds)), row);
            assert_eq!(super::duration_label(milliseconds), row.trim());
        }
        assert_eq!(super::tool_duration(None), "    —");
    }
}
