//! Cache eviction releases only cache ownership. Consumer handles retain their admission charge.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::raw::RawDiff;
use crate::source::SourceIdentity;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct AnalysisKey {
    pub old: SourceIdentity,
    pub new: SourceIdentity,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AnalysisBytes {
    pub source: usize,
    pub result: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct CacheLimits {
    pub entries: usize,
    pub source_bytes: usize,
    pub result_bytes: usize,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CacheUsage {
    pub admitted_entries: usize,
    pub cached_entries: usize,
    pub bytes: AnalysisBytes,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CacheError {
    Saturated,
    ForeignReservation,
    InsufficientReservation,
    WrongIdentity,
}

pub struct AnalysisCache {
    limits: CacheLimits,
    accounting: Arc<Mutex<CacheUsage>>,
    entry: HashMap<AnalysisKey, CachedEntry>,
    clock: u64,
}

/// Shares one retention budget between repository reads and scheduled display analysis.
/// Cache locks never cover file reads, comparison, or waits on worker completion.
pub struct AnalysisStore {
    cache: Mutex<AnalysisCache>,
}

impl AnalysisStore {
    pub fn new(limits: CacheLimits) -> Self {
        Self {
            cache: Mutex::new(AnalysisCache::new(limits)),
        }
    }

    pub fn get(&self, key: AnalysisKey) -> Option<AnalysisHandle> {
        self.cache.lock().expect("analysis store poisoned").get(key)
    }

    pub fn usage(&self) -> CacheUsage {
        self.cache.lock().expect("analysis store poisoned").usage()
    }

    pub fn limits(&self) -> CacheLimits {
        self.cache.lock().expect("analysis store poisoned").limits
    }

    pub fn reserve(&self, bytes: AnalysisBytes) -> Result<AnalysisReservation, CacheError> {
        self.cache
            .lock()
            .expect("analysis store poisoned")
            .reserve(bytes)
    }

    /// Admits speculative retention only when unused capacity exists, without evicting results.
    pub fn try_reserve(&self, bytes: AnalysisBytes) -> Result<AnalysisReservation, CacheError> {
        self.cache
            .lock()
            .expect("analysis store poisoned")
            .reserve_with_eviction(bytes, false)
    }

    pub fn insert(
        &self,
        key: AnalysisKey,
        reservation: AnalysisReservation,
        diff: RawDiff,
    ) -> Result<AnalysisHandle, CacheError> {
        self.cache
            .lock()
            .expect("analysis store poisoned")
            .insert(key, reservation, diff)
    }

    pub fn evict(&self, key: AnalysisKey) -> bool {
        self.cache
            .lock()
            .expect("analysis store poisoned")
            .evict(key)
    }
}

/// Reserves one entry plus independently bounded source and result capacities before work begins.
pub struct AnalysisReservation {
    accounting: Arc<Mutex<CacheUsage>>,
    bytes: AnalysisBytes,
}

#[derive(Clone)]
pub struct AnalysisHandle(Arc<RetainedAnalysis>);

struct RetainedAnalysis {
    diff: RawDiff,
    _reservation: AnalysisReservation,
}

struct CachedEntry {
    handle: AnalysisHandle,
    used: u64,
}

impl Default for CacheLimits {
    fn default() -> Self {
        Self {
            entries: 512,
            source_bytes: 128 * 1024 * 1024,
            result_bytes: 64 * 1024 * 1024,
        }
    }
}

impl AnalysisCache {
    pub fn new(limits: CacheLimits) -> Self {
        Self {
            limits,
            accounting: Arc::new(Mutex::new(CacheUsage::default())),
            entry: HashMap::new(),
            clock: 0,
        }
    }

    pub fn usage(&self) -> CacheUsage {
        let mut usage = *self
            .accounting
            .lock()
            .expect("analysis accounting poisoned");
        usage.cached_entries = self.entry.len();
        usage
    }

    /// Evicts least-recently-used cache references as needed. Live handles remain charged.
    pub fn reserve(&mut self, bytes: AnalysisBytes) -> Result<AnalysisReservation, CacheError> {
        self.reserve_with_eviction(bytes, true)
    }

    fn reserve_with_eviction(
        &mut self,
        bytes: AnalysisBytes,
        evict: bool,
    ) -> Result<AnalysisReservation, CacheError> {
        if bytes.source > self.limits.source_bytes
            || bytes.result > self.limits.result_bytes
            || self.limits.entries == 0
        {
            return Err(CacheError::Saturated);
        }
        loop {
            {
                let mut usage = self
                    .accounting
                    .lock()
                    .expect("analysis accounting poisoned");
                if usage.admitted_entries < self.limits.entries
                    && bytes.source <= self.limits.source_bytes - usage.bytes.source
                    && bytes.result <= self.limits.result_bytes - usage.bytes.result
                {
                    usage.admitted_entries += 1;
                    usage.bytes.source += bytes.source;
                    usage.bytes.result += bytes.result;
                    return Ok(AnalysisReservation {
                        accounting: Arc::clone(&self.accounting),
                        bytes,
                    });
                }
            }
            if !evict {
                return Err(CacheError::Saturated);
            }
            let Some(key) = self
                .entry
                .iter()
                .filter(|(_, entry)| Arc::strong_count(&entry.handle.0) == 1)
                .min_by_key(|(_, entry)| entry.used)
                .map(|(key, _)| *key)
            else {
                return Err(CacheError::Saturated);
            };
            self.entry.remove(&key);
        }
    }

    pub fn get(&mut self, key: AnalysisKey) -> Option<AnalysisHandle> {
        self.clock = self.clock.saturating_add(1);
        let entry = self.entry.get_mut(&key)?;
        entry.used = self.clock;
        Some(entry.handle.clone())
    }

    /// Validates identity and retained capacities before publishing the result to consumers.
    pub fn insert(
        &mut self,
        key: AnalysisKey,
        reservation: AnalysisReservation,
        diff: RawDiff,
    ) -> Result<AnalysisHandle, CacheError> {
        if !Arc::ptr_eq(&self.accounting, &reservation.accounting) {
            return Err(CacheError::ForeignReservation);
        }
        if key.old != diff.source().old.identity() || key.new != diff.source().new.identity() {
            return Err(CacheError::WrongIdentity);
        }
        if diff.retained_source_bytes() > reservation.bytes.source
            || diff.retained_result_bytes() > reservation.bytes.result
        {
            return Err(CacheError::InsufficientReservation);
        }
        if let Some(existing) = self.get(key) {
            return Ok(existing);
        }
        let handle = AnalysisHandle(Arc::new(RetainedAnalysis {
            diff,
            _reservation: reservation,
        }));
        self.entry.insert(
            key,
            CachedEntry {
                handle: handle.clone(),
                used: self.clock,
            },
        );
        Ok(handle)
    }

    pub fn evict(&mut self, key: AnalysisKey) -> bool {
        self.entry.remove(&key).is_some()
    }
}

impl std::ops::Deref for AnalysisHandle {
    type Target = RawDiff;

    fn deref(&self) -> &Self::Target {
        &self.0.diff
    }
}

impl Drop for AnalysisReservation {
    fn drop(&mut self) {
        let mut usage = self
            .accounting
            .lock()
            .expect("analysis accounting poisoned");
        usage.admitted_entries -= 1;
        usage.bytes.source -= self.bytes.source;
        usage.bytes.result -= self.bytes.result;
    }
}
