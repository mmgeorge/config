# Testing

A test suite earns its cost by failing when behavior breaks and staying green when it does not. Full coverage is not a goal: a test that restates the implementation, pins incidental details, or never fails for a real regression adds maintenance and run time without adding signal. Write only high-value, high-signal tests.

## Prefer integration tests

Prefer integration tests that drive the crate through its public interface. Cover lifecycle, persistence, concurrent handoff, plugins, rendering, and host/library boundaries, because those failures surface only when components run together. Place integration tests in the `tests/` directory at the root of the crate they exercise, and follow that directory's existing fixture layout. A test that spans several crates belongs to the crate whose contract it verifies.

Use unit tests sparingly, for algorithms, parsers, and codecs whose input space a public-interface test cannot reach economically. Keep each unit test in the inline `#[cfg(test)]` module of the file it covers. Mock an external boundary only when isolating it proves logic the crate owns.

## Bound test runtime

A slow test hides a hang or a real-clock wait, and one such test stalls every later run. Try to every test under one second when possible. Run tests with `cargo nextest run`, which terminates a test that exceeds its time budget and reports it as a failure.

When creating a project, or working in a workspace that lacks one, add `.config/nextest.toml` at the workspace root with a five-second default per-test timeout:

```toml
[profile.default]
slow-timeout = { period = "5s", terminate-after = 1 }
```

Fix a test that exceeds the timeout before considering an exception. When a test cannot meet the bound, such as one that crosses a real external boundary, add a per-test override that names it, and state the reason in a comment beside the override:

```toml
# Spawns the real compiler toolchain, which takes several seconds to start.
[[profile.default.overrides]]
filter = 'test(=toolchain_round_trip)'
slow-timeout = { period = "30s", terminate-after = 1 }
```

Never raise the default timeout to accommodate a slow test. For any other nextest configuration, such as profiles, filtersets, test groups, retries, setup scripts, or CI integration, use the `nextest` skill.
