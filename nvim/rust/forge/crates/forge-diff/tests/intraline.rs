use forge_diff::intraline::{
    FallbackReason, IntralinePolicy, IntralineResult, compare_replacement,
};

#[test]
fn multibyte_emphasis_uses_exact_utf8_byte_boundaries() {
    let old = "é🙂old終";
    let new = "é🙂new終";
    let IntralineResult::Emphasis(result) =
        compare_replacement(IntralinePolicy::default(), &[old], &[new])
    else {
        panic!("expected emphasis");
    };
    assert_eq!(result[0].old, vec![6..9]);
    assert_eq!(result[0].new, vec![6..9]);
    assert_eq!(&old[result[0].old[0].clone()], "old");
    assert_eq!(&new[result[0].new[0].clone()], "new");
}

#[test]
fn equal_inserted_and_removed_content_produce_no_empty_spans() {
    let IntralineResult::Emphasis(result) = compare_replacement(
        IntralinePolicy::default(),
        &["same", "", "gone"],
        &["same", "new", ""],
    ) else {
        panic!("expected emphasis");
    };
    assert!(result[0].old.is_empty() && result[0].new.is_empty());
    assert!(result[1].old.is_empty());
    assert_eq!(result[1].new, vec![0..3]);
    assert_eq!(result[2].old, vec![0..4]);
    assert!(result[2].new.is_empty());
}

#[test]
fn pair_and_byte_limits_reject_before_character_comparison() {
    let policy = IntralinePolicy::default();
    assert_eq!(
        compare_replacement(policy, &["old"], &[]),
        IntralineResult::LineStyle(FallbackReason::UnpairedLines)
    );
    assert_eq!(
        compare_replacement(policy, &["x"; 65], &["y"; 65]),
        IntralineResult::LineStyle(FallbackReason::PairLimit)
    );
    let large = "a".repeat(16 * 1024 + 1);
    assert_eq!(
        compare_replacement(policy, &[&large], &[""]),
        IntralineResult::LineStyle(FallbackReason::ByteLimit)
    );
    assert_eq!(
        compare_replacement(policy, &["a\nb"], &["c"]),
        IntralineResult::LineStyle(FallbackReason::InvalidLine)
    );
    assert_eq!(
        compare_replacement(policy, &["a\0b"], &["c"]),
        IntralineResult::LineStyle(FallbackReason::InvalidLine)
    );
    let exact = "a".repeat(8 * 1024);
    assert!(matches!(
        compare_replacement(policy, &[&exact], &[&exact]),
        IntralineResult::Emphasis(_)
    ));
}

#[test]
fn span_exhaustion_discards_earlier_emphasis_and_retains_line_style() {
    let policy = IntralinePolicy {
        output_spans: 2,
        ..IntralinePolicy::default()
    };
    assert!(matches!(
        compare_replacement(policy, &["old"], &["new"]),
        IntralineResult::Emphasis(_)
    ));
    assert_eq!(
        compare_replacement(policy, &["old", "old"], &["new", "new"]),
        IntralineResult::LineStyle(FallbackReason::SpanLimit)
    );
    let disabled = IntralinePolicy {
        output_spans: 0,
        ..policy
    };
    assert_eq!(
        compare_replacement(disabled, &["old"], &["new"]),
        IntralineResult::LineStyle(FallbackReason::SpanLimit)
    );
}

#[test]
fn removing_emphasized_bytes_leaves_identical_unchanged_unicode_content() {
    let fixture = [
        "",
        "é",
        "🙂a",
        "a🙂",
        "e\u{301}終",
        "終é終",
        "abcabc",
        "abXabY",
    ];
    for old in fixture {
        for new in fixture {
            let IntralineResult::Emphasis(result) =
                compare_replacement(IntralinePolicy::default(), &[old], &[new])
            else {
                panic!("expected emphasis");
            };
            let line = &result[0];
            let mut unchanged = Vec::new();
            for (text, spans) in [(old, &line.old), (new, &line.new)] {
                for span in spans {
                    assert!(span.start < span.end);
                    assert!(text.is_char_boundary(span.start) && text.is_char_boundary(span.end));
                }
                assert!(spans.windows(2).all(|pair| pair[0].end <= pair[1].start));
                unchanged.push(
                    text.char_indices()
                        .filter(|(position, _)| !spans.iter().any(|span| span.contains(position)))
                        .map(|(_, character)| character)
                        .collect::<String>(),
                );
            }
            assert_eq!(unchanged[0], unchanged[1]);
        }
    }
}
