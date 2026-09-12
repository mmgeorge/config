use std::sync::Arc;

use forge_diff::{
    cache::{AnalysisKey, AnalysisStore, CacheError, CacheLimits},
    engine::{DiffEngine, DiffRequest, reservation_bytes},
    raw::compute_hunks,
    source::{Representation, SourcePair, SourceVersion},
    workers::WorkPriority,
};

fn source(text: &[u8]) -> SourcePair {
    SourcePair {
        old: SourceVersion::new(Vec::new(), Representation::GitCanonical).unwrap(),
        new: SourceVersion::new(text.to_vec(), Representation::GitCanonical).unwrap(),
    }
}

#[tokio::test]
async fn speculative_admission_preserves_existing_results_and_shares_display_ownership() {
    let cache = Arc::new(AnalysisStore::new(CacheLimits {
        entries: 1,
        source_bytes: 1024,
        result_bytes: 1024,
    }));
    let pair = source(b"first\n");
    let key = AnalysisKey {
        old: pair.old.identity(),
        new: pair.new.identity(),
    };
    let reservation = cache.try_reserve(reservation_bytes(&pair)).unwrap();
    let retained = cache
        .insert(key, reservation, compute_hunks(pair.clone()).unwrap())
        .unwrap();
    assert!(matches!(
        cache.try_reserve(reservation_bytes(&source(b"second\n"))),
        Err(CacheError::Saturated)
    ));
    assert!(cache.get(key).is_some());
    let engine = DiffEngine::with_cache(Arc::clone(&cache), 1);
    let displayed = engine
        .compare(DiffRequest {
            source: pair,
            priority: WorkPriority::Visible,
        })
        .await
        .unwrap();
    assert!(std::ptr::eq(&*retained, &*displayed));
    assert!(cache.evict(key));
    assert_eq!(cache.usage().admitted_entries, 1);
    drop(retained);
    drop(displayed);
    assert_eq!(cache.usage().admitted_entries, 0);
    assert!(
        cache
            .try_reserve(reservation_bytes(&source(b"second\n")))
            .is_ok()
    );
}
