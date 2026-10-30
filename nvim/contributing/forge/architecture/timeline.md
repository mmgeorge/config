# Harness timeline and presentation

A transcript that Lua assembles from provider events can disagree with durable history, and a large tool output can freeze the editor. Rust therefore projects the session into one ordered timeline and publishes changes as revisioned operations, and Lua renders only what it receives. Provider lifecycle events never change transcript records in Lua.

## Timeline stream

Each session controller owns one `TimelineStream`. It compares stable entry identities and emits ordered `insert`, `replace`, `remove`, `tool_output`, and `message` operations under a monotonic revision. Initial load, reconciliation, resume, and preview carry full snapshots, and streaming carries only changed items. Prompt submission creates no Lua-owned interaction: broker admission emits the first patch, with the durable interaction and a `Working` status. Provider deltas for one owner coalesce for at most 16 ms or 64 KiB.

`TimelineProjector` combines interactions, plan lifecycle records, and executions into one ordered presentation, placing plan events by their `ExchangeAnchor`. `ExchangeLayout` keeps provider activity inside the activity fold and renders artifacts, plan submissions, verification results, and checkpoint changes before the final response. Check runs and session events get a `TimelineOwner` at creation, either the session or an exchange position, and that owner never changes.

## Exchange content

An exchange owns an ordered `ExchangeEntry` list of commentary, steering, responses, and typed event references. Commentary owns its tool records, reported file changes, and completed check runs. Steering and events stay siblings of commentary, so collapsing commentary never hides steering. A tool keeps its original commentary owner after later commentary arrives, and a tool with no active commentary creates a synthetic "Considering initial steps" commentary.

While an exchange runs, active commentary stays expanded so its trailing tool group can show its count, the latest tool heading, and up to that tool's last four output rows. Previews use their actual row count without blank padding, and tools with no output reserve no preview rows. This includes the synthetic "Considering initial steps" commentary. A completed tool keeps its preview until a successor or commentary boundary replaces it. Commentary settles and folds when its activity is no longer live, and live expansion overrides clear at that transition. Full output stays available for expansion. After the exchange completes, the activity node collapses, and the final response stays outside the fold.

File writes in one activity section share a `Changed N files` node that composes reported hunks into an original-to-final diff, without reading the workspace. Failed and interrupted edits keep their diagnostic but never count as changes. Commands, formatters, and generators stay ordinary tools, because their filesystem effects come only from checkpoint comparison.

## Node state and paging

`NodeMap` owns the expansion choices, node incarnations, and output sources for one open presentation. `NodeState` travels inside block metadata, so one publication changes text, node state, and visible ranges together. Lua sends `set_expansion`, `load_more`, and `retry_loading` actions with a view sequence and node generation, and every window showing the presentation shares the choice.

The timeline has no aggregate byte, row, or block limit. Only individual deliveries are bounded: tool output arrives in 16 KiB source chunks with an initial 64 KiB formatted prefix, and expanded bodies load two window heights at a time in 64 KiB pages, with at most eight section requests per viewport pass. A failed section reports once and waits for an explicit retry.

`ToolOutputView` keeps the ANSI parser across deltas, and `ToolOutputSnapshot` shares output storage with separate output windows without copying or reparsing. `BufferText` shares immutable packed rows through `Arc`. Fenced code is published first and highlighted later by `TranscriptSyntax` jobs, each with a 10-second deadline, so highlighting cannot delay text.

Reasoning stays in durable history but never renders as commentary.

## Check results

An active check is status, not history. `TimelineStream::update_checks` replaces only the status during a run, and it removes the status atomically when it inserts the final result. Completed results form one Checks root that groups runs by Formatting, Lint, Validation, Tests, and Builds, then by source file or target. The root counts each check's latest outcome, so an earlier failure stops counting after a successful retry, while Run rows keep every attempt. The supervisor reads an optional generic JSON report from callbacks and never parses runner output itself.

`CheckSupervisor` holds admission and records under one lock, and the active run owns the source-work permit until its processes are collected. Process cancellation runs in an independent task, so blocked output cannot delay termination. `CheckPublication` coalesces intermediate snapshots in a separate persistence worker and flushes the terminal snapshot before the result returns. Automatic repair reopens the owning exchange through a dedicated transition that keeps its prompt, usage, and checkpoint.

## Status

`SessionPhase` projects exactly one visible phase, and `buffer/status.rs` combines it with optional `CheckActivity`. The primary label takes its own row, followed by check progress, then the latest reasoning preview when the `status_thoughts` preference is on. Rust derives elapsed time from its own start timestamp, and Lua supplies only the one-second sync cadence.

`WorkflowActivity` builds lifecycle text from `LifecycleAction`. Started enters a task or phase, Continued starts another automatic exchange, and Resumed restarts paused or interrupted work. The status row's `BlockMetadata.status` carries its row, animation flag, and hint context, and Lua reads these from committed metadata, never from label text. One timer per transcript updates only the spinner extmark.

## Usage and durations

Each `Turn` keeps native input, cached input, reasoning, and output counts. Codex cumulative snapshots and Copilot `assistant.usage` call identities are persisted in `ExchangeMetrics`, so replay counts each report once.

The outer duration covers tools and delegated waits during active execution, and it freezes during user-input and permission waits. Tool time is the union of overlapping calls on that same clock. Throughput divides generated tokens by active time outside tool, approval, and delegated waits. Each `ToolCall` keeps its first running and first terminal timestamps, and a call first seen at completion shows `—`.

## Transcript window and pickers

The Harness controller binds a native transcript document, a native status document, and a Lua-owned composer buffer, all owned by `OpenPresentation`. The status document has its own revision, so status-only updates leave transcript rows unchanged. `HarnessStatusView` owns the split above the composer and a separate display buffer, so local notices never modify the native replica. Lua fills unused transcript space with virtual rows, so the live tail ends at the bottom without changing native text.

Switching a saved session into the current tab releases both window-bound presentations before transferring the panes. Their buffers, drafts, and queued input remain session-owned. The receiving session then binds fresh presentation, composer-height, and scroll observers to its new windows; closing only the old tab does not release those subscriptions.

Composer height follows wrapped rows within the configured bounds, and `smoothscroll` lets tall drafts scroll within one physical line. If the whole draft fits, the viewport starts at the first line, which stops Insert-mode scrolling from hiding the top of a short draft.

The status split has a fixed viewport at row one. Its owner resets vertical and horizontal scroll offsets after layout and scroll events without moving focus or the transcript. Status sizing remains content-driven, bounded by available window space, and the separate status-details view retains normal scrolling for overflow.

`HarnessResponseReveal` observes the latest top-level exchange's committed node lifecycle.
After transcript, status, and local status layout publication, a new transcript
revision in a live exchange follows the tail when the owning composer window is
focused, including tool activity before any response text. Focus is checked at
publication, and unchanged revisions do not replay navigation. Focused transcripts
retain the shared cursor and selection preservation contract.
A newly settled exchange reveals its response only when focus is outside the transcript. Each visible window measures from the exchange's prompt through the tail in screen rows, including wrapping and the winbar. An exchange that fits aligns to the bottom. A taller exchange places its prompt at the top, exposing the beginning rather than the end of the response. Folded responses retain their positions. Initial adoption and timeline switches establish a baseline without replaying navigation, and skipped reveals are consumed rather than deferred.

Harness Markdown handlers clip decorations to the current message ranges before `render-markdown` caches them, because incremental parser trees can otherwise restore deleted list bullets onto a separator.

Only the transcript window owns the winbar. It shows confirmed values, and a single trailing `*` marks a model, thinking, or speed change that is waiting for current work to stop. The winbar shows `resolving model` until Codex resolves `default` through `model/list`. `runtime_resolved` events update the model without waiting for the turn to finish.

`views/picker/` owns every compact Harness decision surface: one state and layout model, a renderer, and a float anchored to the bottom of the transcript and composer. Structured rows use stable field IDs. The picker hides the terminal cursor through a paired global `guicursor` transition and restores it afterward. Window-local highlighting cannot hide a focused terminal cursor, so keep this transition.

`/sessions` previews sessions in the transcript window without taking a lease. Each visited session has its own transcript buffer, and `session_navigation.lua` resumes the session on `BufEnter`.
