# Dynamic libraries and hot reload

## ABI, build identity, and tooling separation

- Coordinated Rust dylibs require identical toolchain, features, target, profile, package selection, and dependency identities across host, plugin, and shared runtime. Preserve those exact flags on reload builds and rebuild all consumers when shared contracts change.
- Independently built plugins require an explicit versioned ABI specifying layout, ownership, allocation, drop, and failure behavior. A C calling convention alone does not stabilize Rust containers or trait objects.
- Keep build orchestrators independent of libraries they replace (holding a DLL open blocks replacement on Windows). Give proc macros and build scripts direct compiler-host dependencies rather than shared runtime facades.
- Trace crossing types, vtables, globals, thread-locals, reflection metadata, and callbacks to their owning module. A second dylib can embed a duplicate copy of a transitive dependency and its statics. Copy borrowed native events into owned boundary values before retaining them.

## Unload barrier

Keep each library handle alive until every value, vtable, future, timer, callback, thread, and persistent registry entry (including logging/tracing callsites) referencing that generation is gone:

1. Stop new calls and work from entering the generation.
2. Close registration for generation-owned work and callbacks.
3. Cancel and await tracked work, and join generation-owned threads.
4. Destroy generation-owned state while its code stays loaded.
5. Clear bridges, callbacks, and persistent metadata references.
6. Unload only after no persistent owner holds a generation address.

If work fails to drain, keep the module loaded and report the blocked unload.

## Diagnostics

- On Windows, error 127 (missing procedure) usually signals mismatched mangled crate identities. Inspect imports and exports with `llvm-readobj --coff-imports` and `llvm-readobj --coff-exports`.
- For missing thread-local or validation state on the same OS thread, trace which loaded copy of the dependency executed each call.
