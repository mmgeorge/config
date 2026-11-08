use forge_diff::cache::{AnalysisBytes, AnalysisCache, AnalysisKey, CacheError, CacheLimits};
use forge_diff::raw::{RawDiff, compute_hunks};
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn fixture(text: &str) -> (AnalysisKey, AnalysisBytes, RawDiff) {
    let diff = compute_hunks(SourcePair {
        old: SourceVersion::new(b"old\n".to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(text.as_bytes().to_vec(), Representation::Raw).unwrap(),
    });
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
fn reservations_track_live_inputs_beyond_cache_budgets_and_release_on_drop() {
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
    let additional = cache
        .reserve(AnalysisBytes {
            source: 1,
            result: 0,
        })
        .unwrap();
    let large = cache
        .reserve(AnalysisBytes {
            source: 0,
            result: 21,
        })
        .unwrap();
    drop(additional);
    drop(large);
    let second = cache
        .reserve(AnalysisBytes {
            source: 0,
            result: 20,
        })
        .unwrap();
    drop(cache.reserve(AnalysisBytes::default()).unwrap());
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
    drop(cache.reserve(bytes).unwrap());
    drop(handle);
    assert_eq!(consumer.source().new.bytes(), b"new\n");
    assert_eq!(cache.usage().bytes, bytes);
    drop(consumer);
    assert_eq!(cache.usage().bytes, AnalysisBytes::default());
    assert!(cache.reserve(bytes).is_ok());
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
