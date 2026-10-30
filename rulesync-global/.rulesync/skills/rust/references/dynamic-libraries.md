# Dynamic libraries and hot reload

Read before changing Rust dylibs, FFI, native loading, host/plugin contracts, shared runtime dependencies, or hot-reload unload order.

## Contents

- Identify the ABI and runtime owners.
- Separate runtime and build tooling.
- Coordinate build identities.
- Retain each loaded generation.
- Diagnose loader and runtime failures.
- Verify the affected boundary.

## Identify the ABI and runtime owners

Inspect whether the boundary uses coordinated Rust builds or an explicit external ABI before changing types.

For a Rust ABI boundary, keep the toolchain, dependency identities, features, target, and shared types compatible across all consumers. A package version alone does not prove identity.

For independently built plugins, use the project's explicit versioned ABI contract. A C ABI calling convention alone does not make Rust-owned containers, trait objects, or layouts stable. Specify representation, ownership, allocation, destruction, and failure behavior at the boundary.

Trace crossing types, vtables, globals, thread-local values, allocators, and callbacks to their owning module. When both sides depend on one runtime instance, use the project's shared owner and facade structure. Do not impose an aggregate DLL on a project that uses another valid ABI.

A second dylib can embed a second copy of a transitive dependency and its globals. Trace transitive ownership instead of assuming shared package names imply shared runtime state. Reflection metadata and private statics need the same lifetime and visibility review as function pointers.

Keep native event types and windowing dependencies on one compatible identity when the host and plugin exchange them. Copy borrowed events into owned boundary values before retaining them beyond the callback.

## Separate runtime and build tooling

Keep a long-running build orchestrator independent of the library it replaces. On Windows, a process holding that DLL loaded can prevent replacement.

Proc macros and build scripts execute on the compiler host. Give them direct compiler-host dependencies instead of routing them through the application's shared runtime facades. The same rule applies to tooling-only parsing, code generation, or synchronization libraries.

Review the full dependency graph of each process. A shared helper can introduce an unwanted runtime dependency transitively even when the orchestrator's manifest looks independent.

## Coordinate build identities

For coordinated Rust dylibs, build the host, plugin, and shared-runtime consumers using the same package selection, features, target, profile, and toolchain. Preserve that selection on reload builds. Separate Cargo invocations can resolve different feature graphs and overwrite a shared artifact with an incompatible identity.

Launch the agreed artifacts without an incidental narrower build that replaces a shared DLL. Rebuild all consumers and restart the host when a shared type or runtime contract changes.

Use the project's canonical entrypoint definition or export macro. Do not duplicate an export symbol or hand-write an alternate entrypoint contract.

## Retain each loaded generation

Keep the library handle alive through every call and through destruction of values whose methods, drop glue, vtables, or callbacks reside in that generation.

Shared executor identity alone does not make spawning from a reloadable library safe. Track generation-owned futures, blocking jobs, timers, callbacks, and threads through the shutdown barrier. Requesting cancellation is insufficient until the work can no longer execute or destroy generation-owned values.

Persistent registries must retain no callback, vtable, metadata reference, borrowed string, or destructor from an unloaded generation. Logging and tracing callsites require this review too. Use a host-owned forwarding contract when a process-wide registry outlives the plugin.

Before unloading a generation:

1. Stop new calls and work from entering the generation.
2. Close registration for generation-owned work and callbacks.
3. Cancel and await tracked work, and join generation-owned threads.
4. Destroy generation-owned state while its code remains loaded.
5. Clear or unregister bridges, callbacks, and persistent metadata references.
6. Unload only after no persistent owner retains a generation address.

If work or callbacks cannot be drained, keep the module loaded and report the blocked unload. Do not reclaim code while another thread can still execute it.

## Diagnose loader and runtime failures

Compare the actual loaded module with the intended artifact before changing scheduling or disabling validation. Inspect import/export identities, dependency resolution, build inputs, and artifact copy order.

On Windows, a missing-procedure failure such as error 127 requires checking the named procedure and dependent DLL exports. A mismatched Rust crate identity can change exported symbols even when the expected source function exists. Use available PE inspection tools such as `llvm-readobj --coff-imports` and `llvm-readobj --coff-exports`. On other platforms, inspect the corresponding native loader and symbol information.

For missing thread-local or validation state, trace both calls to the dependency instance that executes them before comparing thread identifiers. Two module copies can hold different state on the same operating-system thread. Do not suppress validation to hide that mismatch.

## Verify the affected boundary

Inspect the resolved dependency and feature graph for every affected build root. Confirm that build tooling has no path to a replaceable runtime owner.

Use the configured compiler and build checks for the coordinated consumers. Compare native imports and exports when a loader failure involves symbol identity.

Exercise the actual boundary when needed: entrypoint negotiation, allocation and destruction, callback cleanup, logging, task cancellation, and repeated load/unload. Verify startup failure and blocked shutdown as well as successful reload. Use the project's existing runtime driver and process-ownership rules.
