# Text correction verification

A correction that fires on valid words, or arrives after the user has moved on, is worse than none, so this feature needs evidence for both accuracy and latency. This reference records what was measured on Windows on 2026-10-29, and how to repeat it. macOS behavior is unverified.

The test machine ran Windows 11 Pro build 26300 on an AMD Ryzen 9 9900X, with Neovim 0.12.6, Rust 1.94.0, and the Cargo development profile.

## Accuracy

The historical `en-US-bigram-3` model passed all 18 development occurrences. The separate 32-occurrence held-out set corrected 15 of 16 errors and preserved all 16 correct controls, while `You may loose the connection` stayed a missed correction. These small samples did not establish general grammatical accuracy. The [evaluation record](../../rust/forge/crates/forge-text/evaluation/README.md) preserves that harness and its outputs.

The current `en-US-bigram-4` model preserves recognized English and configured vocabulary words. Production model and host fixtures exercise `two`, `too`, and `off`, full sentences and typing prefixes, and the explicit contraction and capitalization rules. Contextual substitutions such as `form` to `from`, `then` to `than`, and `loose` to `lose` are no longer automatic corrections.

## Latency

The [measurement file](../../rust/forge/crates/forge-text/evaluation/windows-latency.json) records 120 completed words at 20 words per second in a fresh editor. All 120 corrections applied and redrew, and cold initialization took 1,782 ms while typing stayed usable. A paced benchmark fixture produced these numbers; it typed on a real-time cadence, so the suite no longer runs it, and `text::sustained_input` checks the same input without timing it.

| Measurement | p95 (ms) | p99 (ms) |
| --- | ---: | ---: |
| Typed boundary to applied edit | 1.43 | 1.55 |
| Typed boundary through Neovim redraw | 2.05 | 2.15 |

Single-sentence contexts with 10, 100, 1,000, and 10,000 preceding words reached redraw in 1.49, 1.47, 3.91, and 26.46 ms. A 10,000-fragment paragraph reached 45.08 ms, and neither case truncated input. Large contexts can exceed the 16 ms p95 target for short prose, and they stay asynchronous. Measurements end at Neovim's redraw completion, not at the physical display.

## Fixtures

Run fixtures through the Lua runner, which gives each one a fresh Neovim. Native fixtures use the already-built host and never build it:

```nu
nu .forge/checks/lua-tests.nu nvim/lua/forge/tests/unit/text/context.test.lua
```

| Fixture | Behavior checked |
| --- | --- |
| `context::context` | UTF-8 offsets, inline and fenced code, paths, and a 4,096-line paragraph |
| `text::eligibility` | Lua Tree-sitter comments, code exclusion, and native guards |
| `text/wordlist.test.lua::text_wordlist`, `text::wordlist` | Asynchronous file loading, reload, and spelling protection |
| `text::stale_reply`, `text::async_lifecycle` | Continued typing, reply order, and host replacement |
| `text::text` | Host initialization, wire decisions, and shutdown |
| `text::sustained_input` | 120 consecutive corrections, and contexts up to 10,000 preceding words |
| `text::harper_scopes` | Harper diagnostics, fixes, coordinates, and a document past its 120 KB default cutoff |

Rust tests run with `cargo nextest run --cargo-profile dev --locked -p forge-text --lib -E 'test(=<exact-test-name>)'`. Harper changes need no native rebuild.

## Known limits

The model deliberately leaves contextual errors unchanged when the original word is recognized. Unknown spellings still use frequency-based suggestions, so their automatic corrections can be wrong. Disabling `text_correction.enabled` cancels pending work and restores owned mappings without changing existing text.

The Backspace fallback must follow the original mapping's `replace_keycodes` flag. Converting an expression mapping's result a second time produces `word<80>kb` corruption.
