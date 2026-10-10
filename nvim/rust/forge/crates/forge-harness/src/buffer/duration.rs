/// Keeps a five-cell column by switching units when rounded values reach 100.
pub(super) fn tool_duration(milliseconds: Option<u64>) -> String {
    let Some(milliseconds) = milliseconds else { return "    —".into(); };
    for (unit, divisor) in [("s", 100), ("m", 6000), ("h", 360_000), ("d", 8_640_000)] {
        let tenths = milliseconds / divisor + u64::from(milliseconds % divisor >= divisor / 2);
        if tenths < 1000 {
            let label = if tenths % 10 == 0 { format!("{}{unit}", tenths / 10) }
                else { format!("{}.{unit_fraction}{unit}", tenths / 10, unit_fraction = tenths % 10) };
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
