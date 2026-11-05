# Recommended Rust benchmarking and profiling setup

Use Criterion for benchmark timings and Samply for CPU profiles when setting up a Rust repository. Adapt this recommended setup to existing conventions; each repository owns its instructions for running benchmarks.

1. Add Criterion and a shared benchmark target to the crate's `Cargo.toml`.

   ```toml
   [dev-dependencies]
   criterion = { version = "...", default-features = false }

   [[bench]]
   name = "benchmarks"
   harness = false
   ```

2. Use one unified benchmark suite structure: `benches/benchmarks.rs` registers workload modules with `criterion_group!` and `criterion_main!`, and `benches/suites/` groups scenarios by behavior.

   ```text
   crates/<crate>/benches/
     benchmarks.rs
     suites/
       query/
         points.rs
   ```

   Define each workload once, with shared deterministic fixtures and setup. Benchmarking and profiling select the same target and scenario filter: normal Criterion mode measures it, while `--profile-time` repeats it for sampling. Do not maintain separate profiling implementations that can drift from the measured workload.

3. Enable debug information in the workspace root's `Cargo.toml` while retaining benchmark optimizations.

   ```toml
   [profile.bench]
   debug = true
   ```

4. Provide a profiling helper that compiles with Cargo's `--no-run` and JSON message output, then resolves the benchmark executable with `cargo_metadata::Message`. After a successful build, launch that executable directly under Samply with Criterion's `--bench`, `--profile-time`, and benchmark filter. Preserve the package working directory and debug artifacts; never put the build command inside the recording.

5. Make the helper's `--save-only` mode pass both `--save-only` and `--unstable-presymbolicate` to Samply. This produces a recording and symbol sidecar without a server or browser; document both output paths and verify the installed Samply version supports these flags.

## Agent workflow

Benchmark representative workloads first to identify slow regions at the granularity the benchmarks cover and establish a baseline. Profile a slow workload to locate hotspots and justify a proposed change, then benchmark again to verify its effect with concrete before-and-after numbers. Use the repository's documented commands without opening a GUI.

1. Run the benchmark and read Criterion's `new/benchmark.json`, `new/estimates.json`, and `new/sample.json` under its results directory. For standard wall-clock measurements, estimates and sample times use nanoseconds; compare the same estimator and confidence interval before and after a change.
2. Run the profiling helper with `--save-only` and confirm successful exit, nonempty benchmark samples, and a symbol sidecar. With Samply's default output name, the files are `profile.json.gz` and `profile.json.syms.json`; retain them together.
3. Decompress the recording and select the benchmark process. Follow `samples.stack` through `stackTable.frame` and `stackTable.prefix`; resolve frames through `frameTable.func`, `funcTable.resource`, `resourceTable.lib`, and the recording's `libs`. Match library identity to the sidecar, then map relative frame addresses through `known_addresses`, `symbol_table`, and `string_table` to obtain function names.
4. Aggregate sample weights at leaf frames for self samples and along stacks for inclusive samples, counting a function once per sample when reporting inclusive percentages. Support proposals with measured hotspots: for example, "X accounts for 99% of benchmark-thread samples, inclusive of its callees, in this capture; optimize that path first." Cite the recording, workload, capture duration, and sample denominator, and distinguish self from inclusive cost. Report unresolved symbols rather than guessing names; samples are not invocation counts or exact per-call timings.
5. After changing the code, rerun the same unprofiled benchmark with matching fixtures, build settings, and hardware. Report the before/after estimates, confidence intervals, absolute difference, and percentage change alongside correctness checks. For example, 10.0 ms to 8.0 ms is a 2.0 ms reduction, or 20% lower elapsed time; a changed flame-graph percentage alone does not demonstrate a speedup.

The sidecar format is unstable: inspect the emitted schema before implementing a reader. The recording can still say `symbolicated: false` because symbols remain in the sidecar; startup subprocesses such as Criterion's `cargo metadata` and setup work can also appear outside the timed workload.

References: [Criterion](https://bheisler.github.io/criterion.rs/book/), [profiling](https://bheisler.github.io/criterion.rs/book/user_guide/profiling.html), [Cargo artifact messages](https://doc.rust-lang.org/cargo/reference/external-tools.html#artifact-messages), and [Samply](https://github.com/mstange/samply).
