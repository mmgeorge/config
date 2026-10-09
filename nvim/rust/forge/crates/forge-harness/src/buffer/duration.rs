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
