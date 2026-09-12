use forge_diff::cache::{AnalysisBytes, AnalysisCache, AnalysisHandle, AnalysisKey, CacheLimits};
use forge_diff::raw::RawDiff;

/// Publishes a precomputed test fixture with its measured retained capacities.
pub fn retain(diff: RawDiff) -> AnalysisHandle {
    let mut cache = AnalysisCache::new(CacheLimits::default());
    let key = AnalysisKey {
        old: diff.source().old.identity(),
        new: diff.source().new.identity(),
    };
    let reservation = cache
        .reserve(AnalysisBytes {
            source: diff.retained_source_bytes(),
            result: diff.retained_result_bytes(),
        })
        .unwrap();
    cache.insert(key, reservation, diff).unwrap()
}
