# Commit-message generation

A generated message is only as accurate as the context it receives, and a raw diff of a large change either exceeds the model's budget or buries the edit in lockfile noise. `GenerationService` therefore builds a compact, syntax-aware excerpt, and keeps generation in conversations separate from interactive Harness.

## Context selection

HEAD generation includes tracked changes and untracked files, staged generation compares the index with HEAD, and amend generation reads the latest commit's old and new objects. All three, and PR descriptions, use one native diff and excerpt formatter. Ignored paths and `filter=lfs` paths are excluded before any content is read. Other binary and unsupported sources keep an inventory entry without an excerpt.

Status chooses staged changes when any exist and otherwise uses the HEAD comparison.
`forge.integrations.ai_commit` retains drafts by workspace and comparison for the
editor process. A new Status context reuses that cache without validating intervening
Git changes; its source-change tracking starts from the reopened view.
The shared Status `regenerate_about` command forces a new About request through
the same generation owner used by the commit editor. That owner cancels the prior
request, rejects superseded responses, and transfers waiting consumers to the
replacement, so an open commit editor receives the replacement rather than an
obsolete result.

## Excerpt rules

The system instructions and user prompt together hold at most 500,000 UTF-8 bytes, counted before JSON escaping. The prompt lists changed paths with full counts, then excerpts in path order, with test and spec paths last. Emission stops at the byte limit.

| Content | Treatment |
| --- | --- |
| Declarations | Each matched declaration keeps its enclosing signatures and its first 20 changed body rows. Added and removed functions keep only attributes, documentation, and signatures. |
| Context | Two unchanged rows surround each change, and omissions render as `... N lines (+A -D)`. |
| Imports | Standalone imports count in the inventory but are left out of excerpts. |
| `.md` and `.txt` files | Every changed row is kept. |
| Renamed files | They appear in the list without excerpts. |
| License and lock files | They appear only in the inventory. Matching uses the final filename, case-insensitively. |
| Unavailable grammars or malformed syntax | Plain diff excerpts are used. Optional syntax work shares a two-second deadline. |

Overloads are never paired by position, and ambiguous matches keep body excerpts.

`ForgeDebugCommitPromptPreview [head|staged|latest_commit]` returns the exact system and user strings with their byte count, without calling a provider.

## Temporary generation

`HarnessService.generate_text` runs commit and PR text generation through the shared temporary-generation lifecycle. Each request captures worktree identity, branch, HEAD, and recent reachable commit messages, then supplies the prepared evidence to a fresh provider conversation. It never resumes or appends to the interactive conversation.

`background.settings` continues to store the independently selected provider and per-provider model, reasoning, and service-tier preferences. Those user settings are preserved. Commit generation no longer depends on a persistent `BackgroundManager` conversation or its compaction policy; the existing Background timeline describes retained Background work, not these temporary requests.

The shared helper resolves the selected model, supplies no host tools or selected resources, denies permission callbacks, disables writes, and requests native read-only sandboxing where supported. Adapters disable native tools, inherited resources, and hooks only through supported controls. Native defaults may remain; the workflow does not promise complete suppression. See [Providers](providers.md) for the adapter contract and minimum CLI versions.

The helper collects final answer text, applies the output validator, and closes the temporary binding. Cancellation, timeout, or provider failure still triggers independent cleanup. A primary generation failure and a cleanup failure are both retained. Generation failures notify even after the buffer closes and never replace an existing draft. A failed request does not change provider preferences, and the next request starts fresh.

When a CLI cannot launch or is below its adapter's minimum version, generation reports the selected executable and normalized cause. Versions above the minimum remain eligible without an exact-version certification gate.
