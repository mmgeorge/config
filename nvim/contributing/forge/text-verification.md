# Text correction verification

A correction that fires on valid words, or arrives after the user has moved on, is worse than none, so this feature needs evidence for both accuracy and latency. This reference records what was measured on Windows on 2026-10-29, and how to repeat it. macOS behavior is unverified.

The test machine ran Windows 11 Pro build 26300 on an AMD Ryzen 9 9900X, with Neovim 0.12.6, Rust 1.94.0, and the Cargo development profile.

## Accuracy

The frozen model passes all 18 development occurrences. The separate 32-occurrence held-out set corrected 15 of 16 errors and preserved all 16 correct controls, and `You may loose the connection` stays a missed correction. These small samples catch regressions, and they do not establish general grammatical accuracy. The [evaluation record](../../rust/forge/crates/forge-text/evaluation/README.md) holds the harness, outputs, asset provenance, and frozen parameters.

## Latency

The [measurement file](../../rust/forge/crates/forge-text/evaluation/windows-latency.json) records 120 completed words at 20 words per second in a fresh editor. All 120 corrections applied and redrew, and cold initialization took 1,782 ms while typing stayed usable.

| Measurement | p95 (ms) | p99 (ms) |
| --- | ---: | ---: |
| Typed boundary to applied edit | 1.43 | 1.55 |
| Typed boundary through Neovim redraw | 2.05 | 2.15 |

Single-sentence contexts with 10, 100, 1,000, and 10,000 preceding words reached redraw in 1.49, 1.47, 3.91, and 26.46 ms. A 10,000-fragment paragraph reached 45.08 ms, and neither case truncated input. Large contexts can exceed the 16 ms p95 target for short prose, and they stay asynchronous. Measurements end at Neovim's redraw completion, not at the physical display.

## Fixtures

Run each fixture from the repository root in a fresh Neovim. Native fixtures use the already-built host and never build it:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/text_rejection.lua
```

| Fixture | Behavior checked |
| --- | --- |
| `text_rejection` | Target-only edits, immediate rejection, and ordinary Backspace |
| `text_boundaries` | Punctuation, Tab, newline, capitalization, and paste exclusion |
| `text_context` | UTF-8 offsets, inline and fenced code, paths, and a 4,096-line paragraph |
| `text_eligibility` | Lua Tree-sitter comments, code exclusion, and native guards |
| `text_mappings` | Existing mapping fallback and restoration |
| `text_backspace_encoding` | The real Ultimate Autopair expression mapping, and repeated deletion |
| `text_punctuation` | Removing spaces before typed punctuation, and protected inline code |
| `text_wordlist`, `text_wordlist_host` | Asynchronous file loading, reload, and spelling protection |
| `text_native_region` | Editable-region invalidation before draft capture |
| `text_stale_reply`, `text_async_lifecycle` | Continued typing, reply order, and host replacement |
| `text_lifecycle` | Navigation, owner disposal, macros, undo, and redo |
| `text_completion` | Completion insertion followed by a typed delimiter |
| `text_host` | Host initialization, wire decisions, and shutdown |
| `text_latency` | Sustained input and longer contexts |
| `harper_host`, `harper_scopes_host` | Harper diagnostics, fixes, coordinates, and a document past its 120 KB default cutoff |

Rust tests run with `cargo test --profile dev --locked -p forge-text --lib <exact-test-name> -- --exact`. Harper changes need no native rebuild.

## Known limits

The model can miss contextual errors. Disabling `text_correction.enabled` cancels pending work and restores owned mappings without changing existing text.

The Backspace fallback must follow the original mapping's `replace_keycodes` flag. Converting an expression mapping's result a second time produced the `word<80>kb` corruption that `text_backspace_encoding` now covers.
