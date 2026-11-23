# Testing

A test suite earns its cost by failing on real regressions and staying green otherwise. Write high-signal tests rather than chasing coverage percentages that pin incidental implementation details.

## Integration and unit tests

- Prefer integration tests in `tests/` that drive the crate through its public interface, covering lifecycles, persistence, concurrent handoff, plugins, and host/library boundaries. A multi-crate test belongs in the crate whose contract it verifies.
- Use unit tests **only** for parsers, codecs, and algorithms. Keep unit tests inline in `#[cfg(test)] mod tests { ... }` (or in `foo/tests.rs` via `#[cfg(test)] mod tests;` when large).
- Mock external services and I/O boundaries so tests stay isolated and fast. Do not mock internal crate logic just to avoid testing real collaborators.

## Avoid test hooks

Never add `#[cfg(test)]` fields, methods, enum variants, or branches to production types. Reach timeouts and failure states through a paused Tokio clock or a boundary production already exposes (a trait object, constructor parameter, or fake peer). If none reaches a behavior, leave it untested or ask the user before adding a hook. Inline `#[cfg(test)]` modules and test-only fixture constructors that build ordinary production state are fine.

## Bound test runtime

Keep each test under one second when possible, using mocks rather than waiting on external services. Fix slow tests before adding exceptions. When a test genuinely crosses a slow boundary, add a named per-test nextest timeout override with a comment explaining why, rather than raising the default timeout. For nextest profiles, filtersets, retries, or setup scripts, use the `nextest` skill.
