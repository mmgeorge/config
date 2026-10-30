# Text correction verification

The Windows implementation passed the mandatory contextual contrasts and editor ownership checks on 2026-10-29. The user selected Windows-only acceptance; this report makes no macOS verification claim.

The test machine runs Windows 11 Pro, build 26300, on an AMD Ryzen 9 9900X with 12 cores and 24 logical processors. Tests used Neovim 0.12.6, Rust 1.94.0, and the incremental Cargo development profile.

## Linguistic results

The frozen model passes all 18 development occurrences, including `receive`/`relieve`, `form`/`from`, `then`/`than`, `loose`/`lose`, valid controls, and forward-typing prefixes. The separate 32-occurrence held-out set corrected 15 of 16 errors and preserved all 16 correct controls; `You may loose the connection` remains a missed correction.

The [evaluation record](../../rust/forge/crates/forge-text/evaluation/README.md) includes the source harness, complete outputs, asset provenance, and frozen parameters. These small samples establish regression behavior and do not establish general grammatical accuracy.

## Editor and host coverage

Sixteen focused Lua fixtures cover real Insert-mode input, source syntax, context coordinates, ownership, and the native host. Each fixture ran independently; no repository-wide test suite was used.

| Fixture | Behavior checked |
| --- | --- |
| `text_rejection.lua` | Target-only edits, immediate rejection, repeated delimiters, and ordinary Backspace. |
| `text_boundaries.lua` | Punctuation, Unicode punctuation, Tab, newline, capitalization rejection, and paste exclusion. |
| `text_context.lua` | UTF-8 offsets, quoted contractions, inline/fenced code, paths, incremental changes, and a complete 4,096-line paragraph. |
| `text_eligibility.lua` | Actual Lua Tree-sitter comments, inline-comment extraction, code exclusion, and native guards. |
| `text_mappings.lua` | Existing mapping fallback, restoration, and preservation of later mappings. |
| `text_backspace_encoding.lua` | Actual Ultimate Autopair Vimscript-expression fallback, repeated deletion, paired deletion, and Harness correction rejection. |
| `text_punctuation.lua` | Removal of spaces before typed punctuation, combined spelling, rejection, quoted prose, and protected inline code. |
| `text_wordlist.lua` | Asynchronous file loading, concurrent readers, comments, duplicates, reload, and invalid-file errors. |
| `text_wordlist_host.lua` | The default `repo` entry, explicit spelling protection, and removal taking effect after a save. |
| `text_native_region.lua` | The authoritative editable-region adapter and invalidation before draft capture. |
| `text_stale_reply.lua` | Continued typing, Backspace before reply, and submission invalidation. |
| `text_lifecycle.lua` | Navigation, owner disposal, macros, undo, and redo. |
| `text_async_lifecycle.lua` | Reverse reply order, host-generation replacement, and profile mismatch. |
| `text_completion.lua` | Completion insertion exclusion followed by a real typed delimiter and rejection. |
| `text_host.lua` | Cold input, actual host initialization/correction, both wire decisions, session prohibition, provider independence, and shutdown. |
| `text_latency.lua` | Sustained input, visible redraw, and progressively longer complete contexts. |

The existing `client_request_contract.lua`, `client_callback_isolation.lua`, `protocol_contract.lua`, `harness_input_state.lua`, and `github_review_draft.lua` fixtures also pass. Eight targeted Rust tests cover candidate completeness, the contextual gate, capitalization, Unicode sentence boundaries, malformed assets/long words, wire shapes, profile isolation/reload, and ownership of abandoned workers through shutdown.

Terminal MCP drove an isolated visible Neovim process through Markdown, actual Lua comments, a `gitcommit` buffer, and the real Harness composer using the local mock backend. Read-back and screenshots confirmed correction, rejection, untouched source strings and Git-generated lines, and a submitted prompt retaining its rejected spelling. The owned test editor and host were stopped afterward; the user's editor was not reloaded or terminated.

## Latency

The retained [measurement JSON](../../rust/forge/crates/forge-text/evaluation/windows-latency.json) records 120 completed words at 20 words per second in a fresh editor. All 120 corrections applied and all 120 produced measured redraws; cold initialization took 1,782 ms while typing remained usable.

| Measurement | p95 | p99 |
| --- | --- | --- |
| Typed boundary to applied edit. | 1.43 ms | 1.55 ms |
| Typed boundary through Neovim redraw. | 2.05 ms | 2.15 ms |

The progressively longer single-sentence contexts, with 10, 100, 1,000, and 10,000 preceding words, reached redraw in 1.49, 1.47, 3.91, and 26.46 ms respectively. A separate 10,000-fragment paragraph stress case reached 45.08 ms; neither case truncated input. A preliminary concurrent trial observed 118 applied corrections out of 120 boundaries, which motivated recording applied and stale counts separately rather than assuming every submitted boundary applied. The isolated retained run applied all 120.

Redraw measurements end at Neovim's decoration-provider redraw completion, not at physical display scanout. Ordinary short prose meets the initial 16 ms p95 / 32 ms p99 targets on this machine; large contexts can exceed the short-prose budget and remain asynchronous and eligible.

## Reproduction and checks

Run each Lua fixture from the repository root with `nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/<fixture>.lua`. Native fixtures copy the already-built host into isolated temporary state and never invoke a build themselves.

Compiler and lint checks passed for `forge-text`, `forge`, and `forge-protocol` with `cargo +1.94.0 check --profile dev --locked --all-targets` and the corresponding `clippy -- -D warnings`, selecting those packages explicitly. `cargo fmt --all -- --check` and Lua language-server checks for the changed text modules and configuration passed. Targeted Rust tests use `cargo +1.94.0 test --profile dev --locked -p forge-text --lib <exact-test-name> -- --exact`.

The first native build took 5m 32s because the new SymSpell dependency required updating the shared `smallvec` dependency. The final incremental build took 13.45s. Both ran directly through Cargo outside Terminal MCP, without cleaning the target directory or setting a command deadline.

The retained model can still miss contextual errors, and macOS behavior remains unverified. Disable `text_correction.enabled` to invalidate pending work and restore owned mappings without changing existing text.

## Backspace integration follow-up

The initial mapping fixture tested a Lua callback returning encoded keys but missed the actual Ultimate Autopair Vimscript expression. That expression already returned encoded keys; converting its result again produced the reported `word<80>kb` corruption. The fallback now follows the original mapping's `replace_keycodes` flag, and the new real-plugin fixture reproduced the failure before the fix and passes afterward.

Terminal MCP verified the corrected mapping in an isolated editor with the repository's actual autopair configuration, both in Markdown and the Harness composer. Repeated Backspace deleted text normally, while punctuation cleanup and correction rejection remained reversible. This follow-up changed Lua only and required no native rebuild.

## Harper integration

The local Harper 3.0.0 server is managed by Mason and supplies diagnostics and guarded code actions through Neovim's LSP client. Source comments, Markdown, commit messages, prompts, and native editable regions reuse the existing prose classification. Harper starts from a cache directory, and custom Forge URIs retain a stable server-side document identity even when Harness renames a composer after session initialization.

`harper_host.lua` verifies an actual diagnostic and grammar fix. `harper_scopes_host.lua` verifies source/code separation, UTF-16 coordinates, custom URI rename, prompts, commit commentary, native read-only guards, stale code actions, and a 9,001-line document exceeding Harper's default 120 KB cutoff. Terminal MCP separately exercises the actual Harness composer and a Markdown buffer. These changes require no Forge native rebuild.
