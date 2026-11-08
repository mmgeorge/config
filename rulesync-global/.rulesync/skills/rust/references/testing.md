# Testing

A test suite earns its cost by failing when behavior breaks and staying green when it does not. Full coverage is not a goal: a test that restates the implementation, pins incidental details, or never fails for a real regression adds maintenance and run time without adding signal. Write only high-value, high-signal tests.

## Integration tests

Prefer integration tests that drive the crate through its public interface. Cover lifecycle, persistence, concurrent handoff, plugins, rendering, and host/library boundaries, because those failures surface only when components run together. Place integration tests in the `tests/` directory at the root of the crate they exercise, and follow that directory's existing fixture layout. A test that spans several crates belongs to the crate whose contract it verifies.

Add mocking infrastructure rather than relying on live or external services or tools.

## Unit tests

Use unit tests sparingly, for algorithms, parsers, and codecs whose input space a public-interface test cannot reach economically. Keep each unit test in the inline `#[cfg(test)]` module of the file it covers. Mock an external boundary only when isolating it proves logic the crate owns.

## Keep test hooks out of production types

A `#[cfg(test)]` field, method, enum variant, or branch on a production type gives the test build a code path the shipped build never runs. A test that forces a deadline, scripts a runner, injects a failure, or holds a barrier through such a hook proves the hook works, not the product. Reach timeouts and failure states through a paused Tokio clock or a mock at a boundary production already has, such as a trait object, a constructor parameter, or a fake peer process. When none of those reaches a behavior, leave it untested, or ask the user for specific approval before adding a hook. Inline `#[cfg(test)]` test modules and test-only fixture constructors that build ordinary production state are not hooks.

## Bound test runtime

Fast iteration requires quick running tests. Try to every test under one second when possible. Use nextest when installed. It also provides a way to specify a test timeout.

Fix a test that exceeds the timeout before considering an exception. When a test cannot meet the bound, such as one that crosses a real external boundary, add a per-test override that names it, and state the reason in a comment beside the override.

Ideally use mocks to bound runtime (rather than waiting for an external service to respond, a mock can immediately inject the a external response).

Never raise the default timeout to accommodate a slow test. For any other nextest configuration, such as profiles, filtersets, test groups, retries, setup scripts, or CI integration, use the `nextest` skill.
