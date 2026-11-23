# Synchronization

Examples assume a non-poisoning `lock()`. Follow the project's lock type and poisoning behavior.

## Single owner over shared locks

Send commands to one owning task (with oneshot completion channels when callers await ordered results) instead of sharing `Arc<Mutex<...>>`:

```rust
pub struct Indexer {
  commands: mpsc::Sender<IndexCommand>,
}

enum IndexCommand {
  Upsert(Document),
  Remove(DocumentId),
}
```

## One invariant per lock

Group fields that change together behind one `Mutex<State>` rather than splitting a lock and an atomic. Use atomics only for independent flags (such as shutdown), default to `Mutex`, choose `RwLock` only for measured read contention, and document acquisition order for nested locks.

## Drop guards before effects

Release lock guards before `.await`, I/O, decoding, callbacks, or event emission to prevent stalls and re-entrant deadlocks:

```rust
let request = {
  let mut state = self.state.lock();
  state.next_request()
};
let outcome = self.loader.load(&request.path).await;
self.state.lock().complete(request.id, outcome);
```

## Publish only current, complete outcomes

Workers send completed results to the state owner instead of mutating shared storage mid-flight. The owner discards stale request IDs and preserves the previous accepted value when a replacement fails:

```rust
for (request_id, outcome) in self.completions.try_iter() {
  if self.pending_request != Some(request_id) {
    continue;
  }
  self.pending_request = None;
  match outcome {
    Ok(loaded) => self.publish(loaded),
    Err(failure) => self.record_failure(failure),
  }
}
```

Test out-of-order completion, cancellation, replacement failure, concurrent requests, callback re-entry, and shutdown.
