/// Keeps a five-cell column by switching units when rounded values reach 100.
pub(super) fn tool_duration(milliseconds: Option<u64>) -> String {
    let Some(milliseconds) = milliseconds else {
        return "    —".into();
    };
    // Sub-second times read in milliseconds, as `duration_label` shows them in headings.
    if milliseconds < 1000 {
        return format!("{:>5}", format!("{milliseconds}ms"));
    }
    for (unit, divisor) in [("s", 100), ("m", 6000), ("h", 360_000), ("d", 8_640_000)] {
        let tenths = milliseconds / divisor + u64::from(milliseconds % divisor >= divisor / 2);
        if tenths < 1000 {
            let label = if tenths % 10 == 0 {
                format!("{}{unit}", tenths / 10)
            } else {
                format!(
                    "{}.{unit_fraction}{unit}",
                    tenths / 10,
                    unit_fraction = tenths % 10
                )
            };
            return format!("{label:>5}");
        }
    }
    format!("{:>4.0e}d", milliseconds as f64 / 86_400_000.0)
}

pub(super) fn duration_label(milliseconds: u64) -> String {
    if milliseconds < 1000 {
        return format!("{milliseconds}ms");
    }
    seconds_label(milliseconds)
}

fn seconds_label(milliseconds: u64) -> String {
    let tenths = milliseconds / 100 + u64::from(milliseconds % 100 >= 50);
    let seconds = tenths / 10;
    let fraction = tenths % 10;
    if fraction == 0 {
        format!("{seconds}s")
    } else {
        format!("{seconds}.{fraction}s")
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
    /// Rows show sub-second durations in milliseconds, as the Thought and Tools headings do.
    ///
    /// The row column rounded to tenths of a second, so one call read `0.1s` on its row and
    /// `92ms` in both headings above it. Every label still fits the five-cell column.
    #[test]
    fn tool_rows_show_sub_second_durations_in_milliseconds() {
        for (milliseconds, row) in [
            (0, "  0ms"),
            (92, " 92ms"),
            (999, "999ms"),
            (1_000, "   1s"),
            (14_300, "14.3s"),
        ] {
            assert_eq!(super::tool_duration(Some(milliseconds)), row);
        }
        assert_eq!(super::tool_duration(None), "    —");
    }
}
