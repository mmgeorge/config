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

## Background conversations

`HarnessService.background` owns persistent, tool-free conversations independently of interactive Harness. A Background conversation opens with only `BACKGROUND_SYSTEM_MESSAGE` as its instructions, which each request's `TextGeneration::background_input` follows with its task name, response requirements, and evidence. It advertises no Harness tools and selects no skills or MCP servers, writes are off, and `ProviderPermissionContext::deny_all_tools` rejects every native tool, command, and file request its turns make, whatever saved rules or approval mode would allow. Codex additionally runs Background turns in a read-only sandbox without network access, so commands it runs without asking cannot change anything. Each request captures worktree identity, branch, HEAD, and the full messages of the ten most recent reachable commits, which supply terminology rather than evidence. A conversation resumes while its branch and model stay unchanged and its previous HEAD remains an ancestor. Otherwise a new conversation starts, and the old timeline is kept. An operating-system lock serializes work across editors.

`background.settings` stores the Background provider, per-provider model and thinking defaults, and a compaction policy at 128,000 tokens. The Background provider is chosen independently of interactive Harness: Lua sends it in every generation selection, and `BackgroundManager` runs it in its own runtime, so the active chat's provider never decides which provider generates. Changing it starts a new Background conversation, like a model change. `:ForgeDebugBackgroundTimeline` inspects these conversations. Generation failures notify even after the buffer closes, and they never replace an existing draft.

Background generation cancels when its request is cancelled, its two-minute deadline expires, or provider observation fails. Observation failure does not imply user cancellation. A failed generation or unreliable observer retires its runtime, and the next explicit request retries pending cleanup before acquiring a replacement. Cleanup failure keeps the old owner fenced; successful cleanup permits a fresh runtime to resume the retained conversation without replaying the failed request.
