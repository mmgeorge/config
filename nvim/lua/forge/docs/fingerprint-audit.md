# Forge fingerprint audit

The AI comparison-token design documented below was retired on 2026-09-11.
About now generates once per Status open without repository snapshots or identity
comparisons. The commit editor reuses that draft unconditionally, and Ctrl-A
explicitly regenerates from staged contents. The earlier measurements below are
historical. Other subsystem hash boundaries remain as documented.

Forge uses Git object IDs and filesystem metadata for ordinary observation and AI
message reuse. Exact byte checks remain where a mismatch can authorize a destructive
write, reuse analysis for different text, or restore the wrong persisted object.
This audit covers first-party Lua and Rust under Forge and the sidecar integration.
Git's own hashing, dependency internals, and ordinary hash-table indexing are not
additional Forge filesystem fingerprint passes.

## Removed work

| Location | Previous work | Replacement |
| --- | --- | --- |
| `commit_message/context.rs` | Read and SHA-256 hash both sides of every path for a reuse check, then calculate every diff and assemble a prompt. | Compare sorted paths, HEAD IDs, modes, existing index IDs, and sampled worktree metadata. A reuse request acquires no source content and computes no diffs. |
| `commit_message/context.rs::oversized_identity` | Stream the entire oversized worktree file solely to hash bytes excluded from model context. | Removed. Metadata describes that worktree version. Existing index IDs describe staged versions. |
| `commit_message/context.rs` | Hash the complete comparison into a digest. | Bounded cache compares the metadata/ID descriptor directly and assigns an opaque identity token. |
| `commit_message/mod.rs` | SHA-256 hash workspace, comparison digest, and model into another cache key. | Compare the structured serialized key directly. The executable no longer depends directly on `sha2`. |
| `ai_commit.lua::ensure` | Request a fingerprint even when no cached message exists, then request generation. | A cold draft generates directly. Matching pending About generation is shared with a commit editor. |
| `snapshot/collect.rs::read_index_file` | Stream and SHA-256 hash the index and shared indexes on each observation/conversion fence. | Read at most the final 32 bytes of each index file, reusing Git's stored checksum. Retain length. Fall back to modification time when the checksum is omitted. |
| `content.rs::read_object` | Recompute a Git blob's checksum after looking it up by its existing object ID. | Trust the object ID supplied by the Git object database. Header kind and size admission remain. Integrity auditing belongs to Git rather than every UI read. |
| `content.rs` | Materialize a SHA-256 source identity for ordinary content acquisition even when no consumer requested it. | Ordinary acquisition leaves the source identity lazy. Explicit identity requests and actual diff analysis still materialize it. |
| `completion.rs` | Hash each of two reference listings and retain the digest as an additional page identity. | Compare the bounded listings directly and use the existing snapshot revision as the page token. |
| `render/syntax_engine.lua` | SHA-256 the complete diff in both syntax-cache paths. | Index the cache by filename, side, and the existing diff string. No cryptographic hashing or concatenated copy of the diff is needed. |

Prompt generation also stops acquiring source pairs after filling its 180,000-byte
diff budget. It can still append bounded metadata summaries for remaining paths.
When Git reports that index and worktree match, generation reads the indexed blob
instead of converting the same worktree file again.

## Metadata identity contract

The comparison cache tracks at most 10,000 path keys and four recent versions per
path. A source version records an existing Git object ID, a worktree stamp, or both.
When Git reports an unchanged index/worktree relationship, the cache associates the
observed metadata with that blob ID. Staging a complete file can therefore preserve
the About identity without hashing the file. Later worktree edits do not invalidate
the recorded staged object identity.

The worktree stamp includes length, modification time, creation time where available,
permissions, file type, and symlink target. Unix stamps also include device, inode,
and change time. Metadata deliberately does not promise to detect a rewrite that
preserves all sampled metadata. That tradeoff applies to an AI draft or UI cache,
not permission to destroy unobserved contents. A changed metadata stamp can cause
regeneration even when the rewritten bytes happen to be identical.

Comparison descriptors use direct equality. Their cache retains at most 64 entries
and 8 MiB of descriptor bytes. The legacy wire field and operation named
`fingerprint` now carry/request an opaque comparison identity, not a content hash.
Reuse-only requests use the already verified initial snapshot and omit the second
post-content snapshot, because they acquire no content. Generation retains that
post-content verification. Response counters `source_requests` and `diff_pairs` make zero-content reuse checks
observable. Native Git status may perform its own worktree reads to determine status.
These counters cover Forge's additional source-acquisition and diff work.

Index checks no longer have a 64 MiB scan limit because each stamp reads at most
32 bytes. The existing limit of 256 shared-index files bounds directory admission.
The checksum tail accommodates SHA-1 and SHA-256 indexes. Git may refresh shared-index
modification times without changing their contents, so a present checksum takes
precedence over modification time. Omitted checksums use metadata instead.

## Retained hashes and their specific purpose

| Location | Retained input | Failure prevented and why metadata/Git IDs do not replace it |
| --- | --- | --- |
| `forge-diff/source.rs::SourceIdentity::from_bytes` | Acquired text bytes plus representation. Computed lazily once per shared source version. | The diff/syntax cache accepts unsaved, reconstructed, and supplied text that has neither a filesystem timestamp nor an existing blob ID. Reusing analysis for different bytes gives wrong ranges and hunk actions. This hashes already acquired bytes, not a separate file read. |
| `forge-diff/raw.rs::make_hunk` | Two existing source identities, representation tags, and four byte offsets. | Several hunks share one file and metadata stamp. A stable hunk target must identify the exact source pair and range, including after optimistic projection and refresh. It does not reread or rehash the full file. |
| `forge-git/writer/precondition.rs::fingerprint` | Selected worktree bytes for destructive operations. Streamed through 8 KiB of scratch space. | Discard/restore can permanently overwrite an intervening edit. A same-length edit with preserved timestamps must reject that destructive write. Whole-file stage uses current content, unstage uses index identity, and hunk stage uses metadata. Those paths do not use this full-file fingerprint. |
| `forge-git/writer/precondition.rs` and `forge-status/mutation.rs` Git blob computation | Acquired canonical or reconstructed source bytes. | An optimistic or selected hunk result is newly constructed content with no existing Git ID. Its Git blob ID binds the expected index content and enables comparison with authoritative settlement. These are Git-format object identities rather than an extra filesystem freshness hash. |
| `forge-git/content/conversion.rs::configuration_digest` | Bounded resolved Git configuration, up to 1 MiB. | Global/local/includes and resolved configuration control canonical bytes. A worktree file's metadata or index blob ID does not identify those conversion settings. A compact digest keeps cached provenance bounded without retaining a configuration copy per source. |
| `forge-git/content/conversion.rs::attribute_digest` | Six resolved conversion attributes, up to 64 KiB. | Attribute precedence can change normalization without changing source-file metadata. Retained canonical content must not be accepted for a destructive action under different filters or encoding/EOL rules. The compact digest identifies resolved values, not every attribute file's bytes. |
| `forge-git/content/conversion.rs` Git blob computation | Canonical conversion output where the conversion path requires an index comparison. | Newly converted bytes do not yet have an existing object ID. Their Git-format ID is needed to compare against the indexed object. |
| `forge-harness/storage/objects.rs::{put,copy_exact}` | Checkpoint object bytes. | Persistent objects are addressed and verified by their contents. Paths/timestamps can refer to a different or corrupt file after restart. A mismatched restore must fail before replacing user files. Copy verification occurs while the bytes are already being copied. |
| `forge-harness/checkpoint/mod.rs` | Captured staged-entry listing and checkpoint manifest. | A durable checkpoint must identify the exact staged selection and manifest used for restore, including after process restart. A transient repository revision or filesystem timestamp cannot identify the captured historical state. |
| `forge-harness/plan/mod.rs`, `plan/review_source.rs`, `plan/review_annotation.rs`, and `plan/review_document.rs` | Reviewed/submitted plan documents and annotations. | Approval must bind to the exact reviewed content. Accepting a changed plan because its path or metadata matched could execute different work. Saved digests also preserve this boundary across restarts. |
| `forge-harness/plan/mod.rs::content_digest`, `control_tools/runtime.rs`, and broker plan comparison | Normalized questions, options, and plan documents already in memory. | Provider IDs can change between repeated questions. Persisted answer reuse must match the question and choices, and approval must become stale when plan content changes. These records have no Git ID or file timestamp. |
| `forge-harness/rustdoc/resolver.rs::cache_path` | Documentation lookup key. | Produces a bounded, filesystem-safe persistent filename from arbitrary lookup text. Replacing it with file metadata cannot locate the cached item. This is not hashing document contents. |
| `forge-status/ignored.rs::path` | Repository identity string. | Produces a bounded, filesystem-safe persistent filename for repository-specific ignore settings. It does not scan repository files. |
| `forge-review/service/{file,document,thread_projection}.rs` and `forge-buffer/markdown.rs` standard hashers | Existing path/row/thread/annotation identifiers and ranges. | Produce compact presentation identifiers for independent blocks and anchors. These are in-memory identifiers, not file-freshness fingerprints or cryptographic content scans. |
| `views/harness/timeline_status.lua::status_trace` | Serialized trace fields. | This variable is named fingerprint, but it performs direct string equality to suppress duplicate trace events. No hashing or filesystem I/O occurs. |

Cryptographic hashing is therefore absent from AI comparison identity, ordinary
index observation, revision-page identity, and the Lua diff-syntax cache. It remains
at exact analysis, destructive mutation, conversion-provenance, and durable-content
boundaries, plus small persistent-name inputs.


## Verification and measurements

Read-only comparison requests against `D:/config` used fresh release hosts before
and after the changes. The first HEAD request includes host startup. The checkout
was being edited during this work, so these are workload measurements rather than
an isolated fixed-fixture microbenchmark. No model request was made by this probe.

| Request | Before | After |
| --- | ---: | ---: |
| HEAD comparison | 10,927 ms | 562 ms |
| First staged comparison | 900 ms | 466 ms |
| Repeated staged comparison | 849 ms | 453 ms |

Every final response reported `source_requests = 0` and `diff_pairs = 0`.
Cold About generation no longer makes the preliminary comparison request at all.
These measurements are comparison latency, not total model generation latency.

153 Rust tests passed: 69 Git unit tests, 27 Git integration tests, 49 Status tests,
7 commit-message tests, and the native revision-page protocol regression. Six Lua
suites passed. The final rebuilt-host native comparison test passed in 0.45 seconds,
and the real Status -> stage all -> cc test passed in 1.89 seconds, both exit 0
with 60-second timeouts. That integration test asserts one generation request,
immediate cached About text while validation is blocked, no cold comparison, and
zero content/diff work during reuse. The manual 100x32 terminal check confirmed
cached text, subject cursor placement, intact Git comments, and one model call.

The final executable build used `cargo build --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 --bin forge`
in `nvim/rust/forge`. It exited 0 in 43.83 seconds with a 300-second timeout.
Artifact: `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
Detailed commands and regression results are retained in
`nvim/tests/forge/plans/status_contention.md`.
