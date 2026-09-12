use forge_diff::cache::{AnalysisBytes, AnalysisCache, AnalysisKey, CacheError, CacheLimits};
use forge_diff::raw::{RawDiff, compute_hunks};
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn fixture(text: &str) -> (AnalysisKey, AnalysisBytes, RawDiff) {
    let diff = compute_hunks(SourcePair {
        old: SourceVersion::new(b"old\n".to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(text.as_bytes().to_vec(), Representation::Raw).unwrap(),
    })
    .unwrap();
    let key = AnalysisKey {
        old: diff.source().old.identity(),
        new: diff.source().new.identity(),
    };
    let bytes = AnalysisBytes {
        source: diff.retained_source_bytes(),
        result: diff.retained_result_bytes(),
    };
    (key, bytes, diff)
}

#[test]
fn reservations_enforce_independent_budgets_and_release_on_drop() {
    let mut cache = AnalysisCache::new(CacheLimits {
        entries: 2,
        source_bytes: 10,
        result_bytes: 20,
    });
    let reservation = cache
        .reserve(AnalysisBytes {
            source: 10,
            result: 0,
        })
        .unwrap();
    assert!(matches!(
        cache.reserve(AnalysisBytes {
            source: 1,
            result: 0
        }),
        Err(CacheError::Saturated)
    ));
    assert!(matches!(
        cache.reserve(AnalysisBytes {
            source: 0,
            result: 21
        }),
        Err(CacheError::Saturated)
    ));
    let second = cache
        .reserve(AnalysisBytes {
            source: 0,
            result: 20,
        })
        .unwrap();
    assert!(matches!(
        cache.reserve(AnalysisBytes::default()),
        Err(CacheError::Saturated)
    ));
    drop(reservation);
    assert_eq!(
        cache.usage().bytes,
        AnalysisBytes {
            source: 0,
            result: 20
        }
    );
    drop(second);
    assert_eq!(cache.usage().admitted_entries, 0);
}

#[test]
fn eviction_cannot_release_capacity_retained_by_consumers() {
    let (key, bytes, diff) = fixture("new\n");
    let mut cache = AnalysisCache::new(CacheLimits {
        entries: 1,
        source_bytes: bytes.source,
        result_bytes: bytes.result,
    });
    let reservation = cache.reserve(bytes).unwrap();
    let handle = cache.insert(key, reservation, diff).unwrap();
    let consumer = handle.clone();
    assert!(cache.evict(key));
    assert_eq!(cache.usage().cached_entries, 0);
    assert_eq!(cache.usage().admitted_entries, 1);
    assert!(matches!(cache.reserve(bytes), Err(CacheError::Saturated)));
    drop(handle);
    assert_eq!(consumer.source().new.bytes(), b"new\n");
    assert_eq!(cache.usage().bytes, bytes);
    drop(consumer);
    assert_eq!(cache.usage().bytes, AnalysisBytes::default());
    assert!(cache.reserve(bytes).is_ok());
}

#[test]
fn reserve_evicts_the_least_recently_used_unreferenced_result() {
    let mut cache = AnalysisCache::new(CacheLimits {
        entries: 2,
        ..CacheLimits::default()
    });
    let (first, bytes, diff) = fixture("first\n");
    let reservation = cache.reserve(bytes).unwrap();
    drop(cache.insert(first, reservation, diff).unwrap());
    let (second, bytes, diff) = fixture("second\n");
    let reservation = cache.reserve(bytes).unwrap();
    drop(cache.insert(second, reservation, diff).unwrap());
    drop(cache.get(first).unwrap());
    let reservation = cache.reserve(AnalysisBytes::default()).unwrap();
    assert!(cache.get(first).is_some());
    assert!(cache.get(second).is_none());
    assert_eq!(cache.usage().admitted_entries, 2);
    drop(reservation);
}

#[test]
fn publication_rejects_foreign_identity_and_insufficient_capacity() {
    let mut cache = AnalysisCache::new(CacheLimits::default());
    let mut other = AnalysisCache::new(CacheLimits::default());
    let (key, bytes, diff) = fixture("new\n");
    let reservation = other.reserve(bytes).unwrap();
    assert!(matches!(
        cache.insert(key, reservation, diff),
        Err(CacheError::ForeignReservation)
    ));
    assert_eq!(other.usage().admitted_entries, 0);
    let (key, _, diff) = fixture("new\n");
    let reservation = cache.reserve(AnalysisBytes::default()).unwrap();
    assert!(matches!(
        cache.insert(key, reservation, diff),
        Err(CacheError::InsufficientReservation)
    ));
    let (_, bytes, diff) = fixture("different\n");
    let reservation = cache.reserve(bytes).unwrap();
    assert!(matches!(
        cache.insert(key, reservation, diff),
        Err(CacheError::WrongIdentity)
    ));
    assert_eq!(cache.usage().admitted_entries, 0);
}

#[test]
fn shared_source_allocation_is_charged_once_within_one_result() {
    let source = SourceVersion::new(b"same\n".to_vec(), Representation::Raw).unwrap();
    let diff = compute_hunks(SourcePair {
        old: source.clone(),
        new: source,
    })
    .unwrap();
    assert_eq!(diff.retained_source_bytes(), 5);
    let distinct = compute_hunks(SourcePair {
        old: SourceVersion::new(b"same\n".to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(b"same\n".to_vec(), Representation::Raw).unwrap(),
    })
    .unwrap();
    assert_eq!(distinct.retained_source_bytes(), 10);
}

#[test]
fn saturated_admission_preserves_cached_results_with_live_consumers() {
    let (key, bytes, diff) = fixture("new\n");
    let mut cache = AnalysisCache::new(CacheLimits {
        entries: 1,
        ..CacheLimits::default()
    });
    let reservation = cache.reserve(bytes).unwrap();
    let consumer = cache.insert(key, reservation, diff).unwrap();
    assert!(matches!(cache.reserve(bytes), Err(CacheError::Saturated)));
    assert_eq!(cache.usage().cached_entries, 1);
    assert!(cache.get(key).is_some());
    drop(consumer);
    assert!(cache.reserve(bytes).is_ok());
    assert_eq!(cache.usage().cached_entries, 0);
}

#[test]
fn display_cursors_retain_evicted_analysis_until_the_final_cursor_drops() {
    use forge_diff::body::BodyKind;
    use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayState};

    let (key, bytes, diff) = fixture("new\n");
    let mut cache = AnalysisCache::new(CacheLimits {
        entries: 1,
        ..CacheLimits::default()
    });
    let reservation = cache.reserve(bytes).unwrap();
    let handle = cache.insert(key, reservation, diff).unwrap();
    let mut first = DisplayCursor::new(handle.clone(), 0, BodyKind::Modified);
    let mut second = DisplayCursor::new(handle, 0, BodyKind::Modified);
    assert!(cache.evict(key));
    assert_eq!(cache.usage().bytes, bytes);
    assert!(matches!(cache.reserve(bytes), Err(CacheError::Saturated)));
    let chunk = first.next_rows(ChunkLimits::default());
    assert_eq!(chunk.state, DisplayState::Complete);
    assert_eq!(chunk.rows.len(), 2);
    drop(first);
    assert_eq!(cache.usage().admitted_entries, 1);
    assert_eq!(second.next_rows(ChunkLimits::default()).rows.len(), 2);
    drop(second);
    assert_eq!(cache.usage().bytes, AnalysisBytes::default());
    assert!(cache.reserve(bytes).is_ok());
}
