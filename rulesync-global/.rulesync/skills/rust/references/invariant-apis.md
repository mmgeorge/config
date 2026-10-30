# Invariant-preserving APIs

Read before designing public methods that read or mutate state, accessors, guards, transactions, or interior mutability.

Each pattern keeps the invariant inside the type, so callers cannot break it through any sequence of public calls.

## Contents

- Mutate through invariant-preserving methods.
- Combine check-then-act.
- Validate scoped edits before committing.
- Restore invariants when a guard drops.
- Stage changes in a borrowing transaction.
- Keep interior mutability private.
- Assign only after fallible steps.

## Mutate through invariant-preserving methods

Bad: a caller can push out of order, and `binary_search` then returns wrong answers:

```rust
impl Timeline {
  pub fn entries_mut(&mut self) -> &mut Vec<Entry> {
    &mut self.entries
  }
}
```

Good: insertion keeps the order, and reads receive a slice:

```rust
impl Timeline {
  pub fn insert(&mut self, entry: Entry) {
    let index = self.entries.partition_point(|existing| existing.at <= entry.at);
    self.entries.insert(index, entry);
  }

  pub fn entries(&self) -> &[Entry] {
    &self.entries
  }
}
```

Return `&[T]` or an iterator rather than `&Vec<T>`, so callers depend on the contents and not on the container.

## Combine check-then-act

Bad: the state can change between the check and the insert, and a lock-protected version races:

```rust
if !sessions.contains(&id) {
  sessions.insert(id, Session::start(id)?);
}
```

Good: one operation owns both steps:

```rust
let session = sessions.get_or_start(id)?;
```

## Validate scoped edits before committing

Callers edit a draft freely. The owner validates when the closure returns and commits only a valid result, so a rejected edit leaves the previous value in place:

```rust
impl Document {
  pub fn edit(&mut self, change: impl FnOnce(&mut Draft)) -> Result<(), ValidationError> {
    let mut draft = self.current.to_draft();
    change(&mut draft);
    self.current = draft.validate()?;
    Ok(())
  }
}
```

## Restore invariants when a guard drops

Give callers `&mut` to one element through a guard whose `Drop` restores the collection invariant, as `BinaryHeap::peek_mut` does:

```rust
pub struct EntryMut<'timeline> {
  timeline: &'timeline mut Timeline,
  index: usize,
}

impl Deref for EntryMut<'_> {
  type Target = Entry;

  fn deref(&self) -> &Entry {
    &self.timeline.entries[self.index]
  }
}

impl DerefMut for EntryMut<'_> {
  fn deref_mut(&mut self) -> &mut Entry {
    &mut self.timeline.entries[self.index]
  }
}

impl Drop for EntryMut<'_> {
  fn drop(&mut self) {
    self.timeline.entries.sort_by_key(|entry| entry.at);
  }
}
```

## Stage changes in a borrowing transaction

A transaction borrows its store mutably, so the borrow checker rejects other use of the store until the transaction ends. Changes stay staged until `commit`, and dropping an uncommitted transaction discards them:

```rust
pub struct Transaction<'store> {
  store: &'store mut Store,
  pending: Vec<Change>,
}

impl Store {
  pub fn begin(&mut self) -> Transaction<'_> {
    Transaction { store: self, pending: Vec::new() }
  }
}

impl Transaction<'_> {
  pub fn commit(self) -> Result<(), CommitError> {
```

Callers sequence the operations without seeing partial state:

```rust
let mut transaction = store.begin();
transaction.rename(id, name);
transaction.move_to(id, folder);
transaction.commit()?;
```

## Keep interior mutability private

Bad: callers can hold the borrow across a re-entrant call, which panics at runtime:

```rust
pub fn cache(&self) -> RefMut<'_, HashMap<Key, Value>> {
  self.cache.borrow_mut()
}
```

Good: each borrow starts and ends inside one `&self` method:

```rust
pub fn cached(&self, key: &Key) -> Option<Value> {
  self.cache.borrow().get(key).cloned()
}
```

The same rule applies to `MutexGuard` and `RwLockWriteGuard`. The synchronization reference covers guard scope.

## Assign only after fallible steps

Bad: `?` returns after `name` changes but before `path` does, leaving them inconsistent:

```rust
self.name = new_name;
self.path = self.root.join(&self.name).canonicalize()?;
```

Good: every fallible step completes before any assignment:

```rust
let path = self.root.join(&new_name).canonicalize()?;
self.name = new_name;
self.path = path;
```
