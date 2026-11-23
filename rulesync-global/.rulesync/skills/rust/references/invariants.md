# Invariants

Patterns that keep invariants inside the owning type so no sequence of public calls can violate them.

## Scoped validated edits

Let callers edit a mutable draft inside a closure and validate once before committing, leaving the prior value untouched on error:

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

## Guard-restored invariants

Expose `&mut` to one element through a `Drop` guard that restores collection invariants on drop (like `BinaryHeap::peek_mut`):

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

## Borrowing transactions

Borrow the store mutably for a transaction so the borrow checker blocks interleaved access and uncommitted drops discard staged changes:

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
```

## Baseline method rules

- Return `&[T]`, `&str`, or iterators for reads rather than `&Vec<T>` or `&mut` collection references.
- Combine check-then-act sequences into one method (`sessions.get_or_start(id)?`).
- Keep `RefMut`, `MutexGuard`, and `RwLockWriteGuard` private to `&self` methods (see [synchronization.md](synchronization.md)).
- Complete every `?` step before assigning to `self`.
