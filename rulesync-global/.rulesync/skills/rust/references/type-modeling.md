# Type modeling

Read before adding domain types, newtypes, enums, state transitions, behavioral owners, private state aggregates, `Result` success or failure types, retained fields, constructors, operation names, or type-erased stores.

## Contents

- Encode invariants in types.
  - Validate once at construction.
  - Validate deserialized values.
  - Distinguish confusable values.
  - Name choices and states with enums.
  - Represent absence with `Option`.
  - Convert numbers without loss.
  - Match exhaustively.
  - Destructure in conversions.
  - Encode operation order.
  - Release resources with guards.
  - Keep closed sets closed.
  - Make required data unrepresentable as missing.
- Assign behavior by ownership.
- Separate success and failure state.
- Name stored values and operations.
- Confine type erasure.

## Encode invariants in types

Each pattern below moves a runtime check into the type system, so a violation fails `cargo check` instead of a test or production path.

### Validate once at construction

Keep the field private and validate in `TryFrom` or a fallible constructor. Consumers accept the validated type and never recheck it:

```rust
pub struct PortNumber(NonZeroU16);

impl TryFrom<u16> for PortNumber {
  type Error = PortError;

  fn try_from(value: u16) -> Result<Self, PortError> {
    NonZeroU16::new(value).map(Self).ok_or(PortError::Zero)
  }
}
```

### Validate deserialized values

Bad: private fields stop direct construction, but the derived `Deserialize` writes the fields without calling `new`, so `{"min": 5.0, "max": 1.0}` loads successfully:

```rust
#[derive(Deserialize)]
pub struct Extent {
  min: f64,
  max: f64,
}
```

Good: deserialization passes through the same validation as `new`:

```rust
#[derive(Deserialize)]
#[serde(try_from = "RawExtent")]
pub struct Extent {
  min: f64,
  max: f64,
}

#[derive(Deserialize)]
struct RawExtent {
  min: f64,
  max: f64,
}

impl TryFrom<RawExtent> for Extent {
  type Error = ExtentError;

  fn try_from(raw: RawExtent) -> Result<Self, ExtentError> {
    Extent::new(raw.min, raw.max)
  }
}
```

The Facet derive has the same gap. Facet 0.46.5 provides a container proxy and an invariants function for this. Check the installed version's attribute spelling before using either.

### Distinguish confusable values

Bad: every argument is a `u64`, so a swapped call compiles:

```rust
fn schedule(session_id: u64, turn_id: u64, delay_ms: u64) {
```

Good: each value has its own type. Prefer standard types such as `Duration`, `PathBuf`, and `NonZero*` before defining a newtype:

```rust
fn schedule(session: SessionId, turn: TurnId, delay: Duration) {
```

A `type SessionId = u64;` alias provides none of this protection, because the alias and `u64` remain the same type. Define `struct SessionId(u64);` instead.

When values share one representation but belong to different spaces or units, a marker type parameter keeps them apart without duplicating the type:

```rust
pub struct WorldSpace;
pub struct ScreenSpace;

pub struct Point<Space> {
  x: f32,
  y: f32,
  space: PhantomData<Space>,
}

impl Camera {
  pub fn project(&self, point: Point<WorldSpace>) -> Point<ScreenSpace> {
```

### Name choices and states with enums

Bad: the call site does not say what `true` means:

```rust
store.open(&path, true)?;
```

Good:

```rust
store.open(&path, OpenMode::CreateIfMissing)?;
```

Bad: flags and co-dependent options allow a request that is loading, ready, and failed at once:

```rust
pub struct Request {
  loading: bool,
  body: Option<Vec<u8>>,
  error: Option<RequestError>,
}
```

Good: each variant holds exactly the data valid in that state:

```rust
pub enum RequestState {
  Pending,
  Loading,
  Ready(Vec<u8>),
  Failed(RequestError),
}
```

### Represent absence with `Option`

Bad: every reader must know that `0` means unknown, and the compiler cannot remind them:

```rust
pub received_at_ms: i64,
```

Good: callers must handle the missing case:

```rust
pub received_at: Option<Timestamp>,
```

### Convert numbers without loss

Bad: `as` silently truncates a length above `u32::MAX`:

```rust
let count = items.len() as u32;
```

Good: widening uses `From`, and narrowing reports overflow:

```rust
let count = u32::try_from(items.len())?;
let total = u64::from(count);
```

### Match exhaustively

Bad: a new `ToolStatus::Interrupted` variant silently counts as finished:

```rust
let finished = match status {
  ToolStatus::Running => false,
  _ => true,
};
```

Good: adding a variant fails compilation here until someone decides how to treat it:

```rust
let finished = match status {
  ToolStatus::Running => false,
  ToolStatus::Succeeded | ToolStatus::Failed | ToolStatus::Denied => true,
};
```

`matches!` and `if let` treat every unlisted variant as non-matching. Use them only when that answer is correct for any future variant. A foreign `#[non_exhaustive]` enum requires `_`.

### Destructure in conversions

Bad: a new `ProviderUpdate::phase` field compiles and is silently dropped:

```rust
fn notification(update: &ProviderUpdate) -> Notification {
  Notification { address: update.address, text: update.text.clone() }
}
```

Good: complete destructuring fails compilation until the new field is handled:

```rust
fn notification(update: ProviderUpdate) -> Notification {
  let ProviderUpdate { address, text, received_at: _ } = update;
  Notification { address, text }
}
```

Write `field: _` to drop a field deliberately. `..Default::default()` hides new fields the same way field access does.

### Encode operation order

Consume `self` when calling an operation out of order is a bug. The compiler then rejects editing or resubmitting a draft after submission:

```rust
pub struct DraftReview {
  answers: Vec<Answer>,
}

pub struct SubmittedReview {
  id: ReviewId,
  answers: Vec<Answer>,
}

impl DraftReview {
  pub fn submit(self, store: &ReviewStore) -> Result<SubmittedReview, SubmitError> {
```

Use a state enum instead when runtime data, such as a provider event stream, drives the transitions.

### Release resources with guards

Bad: an early return skips the release:

```rust
let id = registry.subscribe(listener);
process(&registry)?;
registry.unsubscribe(id);
```

Good: dropping the guard releases the subscription on every path:

```rust
#[must_use = "dropping the subscription unsubscribes immediately"]
pub struct Subscription {
  id: SubscriptionId,
  registry: Weak<Registry>,
}

impl Drop for Subscription {
  fn drop(&mut self) {
    if let Some(registry) = self.registry.upgrade() {
      registry.unsubscribe(self.id);
    }
  }
}
```

Mark handles, guards, builders, and pure results `#[must_use]` so discarding one warns.

### Keep closed sets closed

Use an enum for a closed set of implementations, so adding one fails compilation at every `match`. Keep `dyn Trait` for open sets such as plugins.

When each implementation has its own data type, tie it to the trait with an associated type so one backend's value cannot reach another:

```rust
pub trait Renderer {
  type Buffer;

  fn draw(&mut self, buffer: &Self::Buffer);
}
```

### Make required data unrepresentable as missing

Bad: every reader repeats a runtime assumption:

```rust
let root = self.roots.first().expect("workspace has a root");
```

Good: the type guarantees the primary root exists:

```rust
pub struct Workspace {
  primary_root: Root,
  additional_roots: Vec<Root>,
}
```

Reserve `expect` for invariants the type system cannot express, and state the invariant in its message.

## Assign behavior by ownership

Distinguish these roles without requiring a separate type for each:

- A domain type models a concept such as a document, asset key, or revision. Its methods express that concept or preserve its validity.
- A behavioral owner coordinates one responsibility. It owns the decisions, transitions, and effects that complete each operation.
- A private state aggregate groups internal storage, such as fields behind one mutex. Keep it passive when grouping storage is its only responsibility.

For example, `AssetStoreData` can hold lock-protected fields while `AssetStore` owns loading, replacement, and removal. Do not mechanically move every operation onto the storage type. Give a contained queue or state machine methods when it enforces its own contract. Synchronization alone does not justify a second behavioral owner.

Prefer immutable configuration, derived values, and operation-local state. Before retaining a field, identify which operation creates it, which reads or changes it, and which clears it. Place it on the owner of that complete lifecycle.

Introduce another behavioral owner only for a distinct lifecycle, invariant, algorithm, or reusable contract. If no precise noun describes its responsibility, check whether it combines unrelated state before choosing a broader name.

## Separate success and failure state

Use `Result<Success, Failure>` when success and failure carry different state. Let each branch own the context needed to process that outcome. Avoid a wrapper around another `Result` unless the wrapper has a lifecycle independent of either branch.

For an asset load, a result can separate a complete value from the context required for retry:

```rust
type AssetLoadResult = Result<LoadedAssetSet, AssetLoadFailure>;
```

The success owns the decoded assets and their provenance. The failure owns the attempted source and error information. These are illustrative roles, not a requirement to rename an existing API.

## Name stored values and operations

Use `*_by_*` field names only for an actual lookup direction. Expose lifecycle in temporal names: use `pending_request` when scheduling creates it and completion clears it. Avoid `latest` or `current` when they conceal replacement or cleanup. Use `next_request_id` only for a stored next identifier. Derive collection size instead of maintaining a redundant count.

Give a public operation its domain name without repeating the receiver's domain, so the call reads as owner, action, and object. Repeated `start`, `process`, `handle`, or `request` layers indicate an operation split across owners without reason.

Use `execute_*` for a private helper only when it crosses from preparation into effects. Use `Worker` only for an independent execution lifecycle or scheduling policy.

Provide required dependencies in the owner's constructor. Add a separate default constructor only when a valid default state has independent consumers.

## Confine type erasure

Keep erasure inside heterogeneous registries, caches, or plugin stores. Recover the concrete type when creating a typed handle, then retain typed state instead of repeatedly downcasting.

Review `PhantomData<T>` beside an erased payload, repeated `downcast::<T>()`, and runtime type-mismatch errors inside a generic wrapper. They indicate that erased state escaped its owning boundary. A marker parameter such as `Point<WorldSpace>` is not erasure.
