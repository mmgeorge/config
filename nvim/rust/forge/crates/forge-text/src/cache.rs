use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    sync::Mutex,
};

/// Evict memoized results without imposing an input or candidate admission limit.
pub(crate) struct Memo<Key, Value> {
    entry: Mutex<MemoState<Key, Value>>,
    capacity: usize,
}
struct MemoState<Key, Value> {
    values: HashMap<Key, Value>,
    order: VecDeque<Key>,
}
impl<Key: Eq + Hash + Clone, Value: Clone> Memo<Key, Value> {
    /// Create a cache scoped to its immutable model revision.
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            entry: Mutex::new(MemoState {
                values: HashMap::new(),
                order: VecDeque::new(),
            }),
            capacity,
        }
    }
    /// Get a retained result without exposing its mutable storage.
    pub(crate) fn get(&self, key: &Key) -> Option<Value> {
        self.entry.lock().ok()?.values.get(key).cloned()
    }
    /// Retain a complete result after evicting the oldest memoized keys.
    pub(crate) fn insert(&self, key: Key, value: Value) {
        let Ok(mut entry) = self.entry.lock() else {
            return;
        };
        if entry.values.contains_key(&key) {
            return;
        }
        while entry.values.len() >= self.capacity {
            if let Some(expired) = entry.order.pop_front() {
                entry.values.remove(&expired);
            } else {
                break;
            }
        }
        entry.order.push_back(key.clone());
        entry.values.insert(key, value);
    }
}
