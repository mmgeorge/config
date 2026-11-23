# Performance

Use Criterion for timings and Samply for CPU profiles, adapting to existing repo conventions.

## Setup

1. Add Criterion to `[dev-dependencies]` (`default-features = false`) with a shared `[[bench]]` target (`name = "benchmarks"`, `harness = false`), registering modules under `benches/suites/` from `benches/benchmarks.rs` so normal benchmarking and `--profile-time` sampling run identical code.
2. Set `debug = true` under workspace `[profile.bench]`.
3. Compile via `--no-run --message-format=json`, resolve the benchmark binary with `cargo_metadata::Message`, and run it directly under Samply with `--bench`, `--profile-time`, and filter flags. In `--save-only` mode, pass `--save-only` and `--unstable-presymbolicate` (after checking Samply version support) to produce `profile.json.gz` and `profile.json.syms.json` headless.

## Headless measurement workflow

1. **Baseline:** Run Criterion and read `new/benchmark.json`, `new/estimates.json`, and `new/sample.json` (nanoseconds).
2. **Profile:** Run the helper with `--save-only`, decompress `profile.json.gz`, select the benchmark process, walk `samples.stack` through `stackTable.frame`/`stackTable.prefix`, resolve via `frameTable.func` → `funcTable.resource` → `resourceTable.lib` → `libs`, and map addresses against the sidecar's `known_addresses`, `symbol_table`, and `string_table`.
3. **Report hotspots:** Aggregate leaf weights for self samples and stack weights for inclusive samples (counting each function once per sample). Cite workload, duration, sample denominator, and unresolved symbols. Samples are not invocation counts.
4. **Verify:** Re-run unprofiled Criterion with identical fixtures and report before/after estimates, confidence intervals, absolute delta, and percentage change. A changed flamegraph share alone does not prove a speedup.

Note: the sidecar schema is unstable (inspect before parsing), `symbolicated: false` can still appear when symbols are in the sidecar, and setup subprocesses may appear outside the timed workload.
