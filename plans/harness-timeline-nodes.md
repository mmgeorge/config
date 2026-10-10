# Harness timeline nodes and streaming

The timeline must use one node model for streaming, expansion, and loading. A tool finishing must change its lifecycle without leaving a preview behind or making Lua infer its state from native fold ranges. This plan consolidates the agreed behavior and adds the shared Rust/Lua IPC contract.

## Scope and ownership

Rust owns node identity, hierarchy, source lifecycle, expansion intent, and loaded content. Lua sends user intent and applies published content and metadata together. Native folds are a presentation of node state, not another owner of expansion state.

Nodes represent exchanges, messages, tool groups, tools, changes, files, and hunks. Messages remain non-foldable. File hunks retain the existing ForgeStatus presentation without caret markers. Tools retain dot markers.

The existing incremental stream parser, changed-block updates, bounded content pages, and 15 fps publication limit remain in use. The change makes these operations node-aware instead of introducing a second streaming pipeline.

## Shared node contract

Keep these concepts separate in the schema:

| Field | Contract |
| --- | --- |
| Identity and parent | Rust assigns a stable node ID, parent ID, kind, and sibling order. Resizing and streaming preserve identity. |
| Generation | A replacement incarnation receives a new generation. Delayed actions cannot modify its replacement. |
| Source lifecycle | Live or settled describes whether source content can still advance. Fold state does not determine lifecycle. |
| Default display | The lifecycle and node role select heading, preview, or full display. |
| Explicit expansion | An optional user choice overrides the default. An absent choice differs from an explicit close. |
| Effective display | Rust resolves the default and explicit choice. Lua renders that result. |
| Content revision | A monotonic revision identifies the content version independently of node generation. |
| Loaded extent | Loaded rows, bytes, and continuation availability describe the delivered body, independently of source size. |

Loading and failure handling must not overload source lifecycle or expansion intent. Associate pending requests and reported failures with the addressed node and generation. A failed load preserves the last published content and permits an explicit retry without an automatic retry loop.

Every delivered body block identifies its owning node. Lua can therefore address the same node from its heading or body without searching unrelated transcript content.

## IPC and streaming

1. Define the node schema in the shared Rust protocol and validate the same fields in Lua. Update the wire version with the contract.
2. Send expansion, load-more, and retry actions with document identity, view identity, node identity, generation, and an ordered request sequence. Include viewport geometry for page sizing.
3. Address streamed content changes to existing nodes. Append or replace only changed content blocks and publish lifecycle or display transitions as node metadata changes.
4. Deliver content changes, node state, and fold boundaries in one document revision. Reject stale generations and stale user intents before changing visible state.
5. Keep closed bodies out of IPC. Retain source content in Rust for later expansion without forcing parsing or copying of closed historical output.

Parent expansion reveals immediate children according to their own state. It does not recursively open or deliver all descendant bodies. Closing a parent removes its delivered subtree and page allocations while retaining child expansion choices for the open presentation.

## Display and loading behavior

- The latest tool in a live group receives at most four preview rows unless explicitly closed. Other tool bodies remain closed by default.
- Settling the exchange removes automatic previews. Reopening a settled group shows tool headings. Explicitly opening a tool shows its paged output without the four-row preview limit.
- Tool and file bodies initially load two viewport heights. Approaching within one viewport height of the loaded boundary requests another page.
- A load extends the existing body rather than clearing it. Closing content removes its rows without inserting blank placeholders.
- All windows attached to the same presentation share expansion intent. Scope changes preserve node choices. Retiring the presentation ends that retention scope.
- A heading remains actionable even when its closed body has no native fold. Caret direction follows effective node display rather than native fold existence.

## Object flows

**Stream and settle.** A provider delta updates the tool's retained source and incremental parser. Rust publishes changed visible blocks and node metadata, then marks the exchange settled and removes automatic previews in the same publication that changes the display state.

**Expand, page, and close.** A user action identifies the node from its heading or body. Rust validates the action, prepares the requested bounded body, and publishes it atomically. Scrolling extends that body near its boundary. Closing releases the delivered subtree while preserving explicit descendant choices.

**Race or failure.** A close, replacement, or newer action invalidates an older response through generation and sequence checks. Failed preparation retains the previous frame and reports the affected node. Retrying uses current identity and revision rather than replaying an obsolete request.

## Implementation sequence

1. Consolidate node types and action schemas in `forge-buffer`, Harness buffer modules, `forge-protocol`, and `forge.protocol`.
2. Make the Harness node owner resolve lifecycle defaults and user overrides. Remove parallel tool-preview and fold-preference state where it duplicates that ownership.
3. Route streaming and body loading through node identities and indexed content ranges. Preserve the existing incremental parser and shared immutable text storage.
4. Update `forge.buffer`, Harness presentation, native folds, and marker rendering to consume the published node contract. Capture cursors only immediately before synchronous publication.
5. Remove obsolete action paths and document the final ownership and failure boundaries in the architecture guide.

## Verification and performance criteria

Cover live previews, settlement, explicit close, full tool expansion, parent close/reopen, nested file content, and paging. Include split windows, scope switches, delayed responses, generation replacement, load failures, and explicit retries.

Exercise the real Rust-to-Lua protocol and a fresh Neovim host. Assert that content and fold metadata commit together, closed nodes retain visible actionable headings, and cursor restoration never uses a snapshot captured before asynchronous work.

For a fixed-size delta, increasing unrelated settled history must not increase the number of projected entries or copied text bytes. A stream update must touch only changed visible blocks and required node metadata. Expansion must visit only the selected subtree and delivered prefix. Record callback duration, affected blocks, projected entries, delivered bytes, and retained bytes in performance fixtures.

Use release tests and the same target directory as the executable. Rebuild the executable for protocol or Rust changes before native-host verification. A protocol mismatch must produce an explicit error instead of admitting incompatible node data.

Completion requires the shared node contract to govern streaming and folding end to end, with the lifecycle, race, paging, and performance cases verified. Existing implementation changes do not by themselves establish that every criterion is complete.

## Implementation evidence

The implementation now publishes the shared node contract, uses node-addressed expansion and loading, retains incremental tool parsing, and formats tool source in bounded prefixes. Closing a parent retains descendant choices, settled tools omit automatic previews, and marker rendering works when a closed node has no native fold. Body splices repair enclosing fold endpoints before later heading updates adopt those current ranges.

Verification completed on 2026-10-10:

| Check | Result | Timeout |
| --- | --- | --- |
| Harness Rust buffer suite | 94 passed, exit 0 | 300 seconds including compilation |
| Session regression suite repeated with independent process state | 18 passed in each of three runs, exit 0 | 300 seconds per run including compilation |
| Affected Lua checks | 14 passed, exit 0 | 30 seconds per check |
| Fresh native Harness, checkpoint/restart, and Status hosts | Three passed, exit 0 | 120 seconds per host |
| Release executable | Built successfully, exit 0, 14.23 seconds | 300 seconds |

The Rust checks and executable use `D:/.cache/nvim/rust-sidecar/forge/build` with the same release profile. The final build recompiled `forge-harness` and `forge`.

Run the Rust checks and build from `nvim/rust/forge`:

```text
cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib buffer:: --no-fail-fast
cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build -p forge-harness --lib buffer::session::tests:: --quiet
cargo build --manifest-path Cargo.toml --target-dir D:/.cache/nvim/rust-sidecar/forge/build --profile release --bin forge --locked --timings
```

Run each Lua check from the repository root with `nvim --headless -u NONE -i NONE -c "set rtp+=nvim" -l nvim/tests/forge/<check>.lua`. The checked fixtures are `harness_tool_toggle`, `harness_tool_output`, `harness_fold_lifecycle`, `harness_sections`, `harness_file_paging`, `harness_publication`, `harness_wire_version`, `streaming_performance`, `harness_content_layout`, `harness_exchange_folds`, `harness_plan_folds`, `harness_fold_markers`, `harness_transport_failure`, and `harness_status_hint_async`. The fresh-host fixtures are `harness_host`, `harness_checkpoint_host`, and `status_host`.

The streaming fixture measured 100 updates over 30,006 rows at 0.4357 ms p95 and 1.0851 ms maximum callback duration, with at most 115 sequence visits. These are fixture measurements, not a bound on arbitrary provider or editor workloads. The executable is `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`. Existing hosts require a restart to load it.
