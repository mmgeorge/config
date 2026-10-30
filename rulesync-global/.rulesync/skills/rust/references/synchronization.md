# Synchronization

Read before adding or restructuring locks, atomics, channels, completion queues, cancellation, or shared mutable state.

Examples use a non-poisoning `lock()` that returns its guard directly. Follow the project's lock implementation and poisoning behavior.

## Contents

- Give mutation one owner.
- Protect one invariant per lock.
- Release guards before effects.
- Publish only current, complete outcomes.
- Verify concurrency failures.

## Give mutation one owner

Do not add synchronization when one task can perform all mutation. Send that task commands instead of sharing the state.

Bad: every caller locks and mutates, so no owner enforces the invariant:

```rust
pub struct Indexer {
  documents: Arc<Mutex<HashMap<DocumentId, Document>>>,
}
```

Good: the receiving task owns the map and applies commands in order:

```rust
pub struct Indexer {
  commands: mpsc::Sender<IndexCommand>,
}

enum IndexCommand {
  Upsert(Document),
  Remove(DocumentId),
}
```

When state must be shared, keep the lock-protected aggregate passive and leave lifecycle decisions on its owner, as the type-modeling reference describes.

## Protect one invariant per lock

Put fields that change together behind one lock.

Bad: a reader can observe the new revision with the old value:

```rust
pub struct AssetSlot {
  value: Mutex<Option<Asset>>,
  revision: AtomicU64,
}
```

Good: one guard covers both fields:

```rust
pub struct AssetSlot {
  state: Mutex<AssetState>,
}

struct AssetState {
  value: Option<Asset>,
  revision: u64,
}
```

Use an atomic only for a value no other field depends on, such as a shutdown flag. Default to `Mutex`, and choose `RwLock` only for demonstrated read contention. Document the acquisition order if nested locking remains. A poison flag does not make partially written state valid.

Reloadable code also follows the shared-runtime rules in the dynamic-libraries reference.

## Release guards before effects

A guard covers reading, deciding, and committing. Release it before `.await`, I/O, decoding, callbacks, event emission, or any call that can re-enter the owner.

Bad: the guard is held across the load:

```rust
let mut state = self.state.lock();
let request = state.next_request();
let asset = self.loader.load(&request.path).await?;
state.publish(request.id, asset);
```

Good: a scope produces an owned request, and `complete` rechecks that it is still current:

```rust
let request = {
  let mut state = self.state.lock();
  state.next_request()
};
let outcome = self.loader.load(&request.path).await;
self.state.lock().complete(request.id, outcome);
```

Bad: an observer that reads the store deadlocks:

```rust
let mut state = self.state.lock();
state.assets.remove(&id);
self.observers.notify(AssetEvent::Removed(id));
```

Good: commit, release, then emit, so observers read the state the event describes:

```rust
let removed = self.state.lock().assets.remove(&id);
if removed.is_some() {
  self.observers.notify(AssetEvent::Removed(id));
}
```

## Publish only current, complete outcomes

A worker owns its computation until it produces a complete outcome. It sends that outcome to the state owner, which validates and publishes it. Do not write partial worker state into shared storage.

Bad: a stale load overwrites a newer one, and a failure discards the previous value:

```rust
while let Ok(outcome) = self.completions.try_recv() {
  self.value = outcome?;
}
```

Good: the owner drains at its mutation boundary, checks request identity, and keeps the previous value visible when a replacement fails:

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

Represent loading in accepted state only before the first value exists. Retain the observed sources on `LoadFailure` when a later source change must retry the load.

## Verify concurrency failures

Repeated sibling acquisitions or one lock per related field indicate that the compound operation belongs elsewhere.

Exercise out-of-order completion, cancellation, replacement failure, concurrent requests, callback re-entry, and shutdown.
