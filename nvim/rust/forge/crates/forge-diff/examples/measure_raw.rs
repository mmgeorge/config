use std::hint::black_box;
use std::time::Instant;

use forge_diff::raw::compute_hunks;
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn main() {
    let ordinary = (0..5000)
        .map(|line| format!("let value_{line} = {line};\n"))
        .collect::<String>();
    let edited = ordinary.replace("value_2500 = 2500", "value_2500 = 7");
    let repeated = "same line\n".repeat(5000);
    let repeated_edit = format!(
        "{}changed\n{}",
        "same line\n".repeat(2500),
        "same line\n".repeat(2499)
    );
    for (name, old, new) in [
        ("unique_single_change", ordinary, edited),
        ("repeated_single_change", repeated, repeated_edit),
        (
            "all_lines_changed",
            "old line\n".repeat(5000),
            "new line\n".repeat(5000),
        ),
    ] {
        let source = SourcePair {
            old: SourceVersion::new(old.into_bytes(), Representation::GitCanonical).unwrap(),
            new: SourceVersion::new(new.into_bytes(), Representation::GitCanonical).unwrap(),
        };
        let mut sample = Vec::with_capacity(25);
        let mut hunk_count = 0;
        for _ in 0..25 {
            let start = Instant::now();
            let result = black_box(compute_hunks(black_box(source.clone())).unwrap());
            sample.push(start.elapsed().as_micros());
            hunk_count = result.hunks().len();
        }
        sample.sort_unstable();
        println!(
            "{{\"fixture\":\"{name}\",\"samples\":25,\"old_bytes\":{},\"new_bytes\":{},\"hunks\":{hunk_count},\"p50_us\":{},\"p95_us\":{},\"max_us\":{}}}",
            source.old.bytes().len(),
            source.new.bytes().len(),
            sample[12],
            sample[23],
            sample[24]
        );
    }
}
