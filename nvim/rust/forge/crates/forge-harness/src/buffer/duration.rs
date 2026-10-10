/// Keeps tool titles at a fixed column as elapsed time grows or changes units.
pub(super) fn tool_duration(milliseconds: Option<u64>) -> String {
    let Some(milliseconds) = milliseconds else { return "     —".into(); };
    let label = duration_label(milliseconds);
    if label.len() <= 6 { return format!("{label:>6}"); }
    for (unit, divisor) in [("m", 60_000), ("h", 3_600_000), ("d", 86_400_000)] {
        let value = milliseconds as f64 / divisor as f64;
        let label = format!("{value:.1}{unit}");
        if label.len() <= 6 { return format!("{label:>6}"); }
    }
    format!("{:>5.0e}d", milliseconds as f64 / 86_400_000.0)
}

pub(super) fn duration_label(milliseconds: u64) -> String {
    if milliseconds < 1000 {
        return format!("{milliseconds}ms");
    }
    let tenths = milliseconds / 100 + u64::from(milliseconds % 100 >= 50);
    let seconds = tenths / 10;
    let fraction = tenths % 10;
    if fraction == 0 {
        format!("{seconds}s")
    } else {
        format!("{seconds}.{fraction}s")
    }
}
