# Plan review

A review that drifts from the submitted revision, or loses comments when a request fails, cannot serve as the approval gate for implementation. PlanReview therefore shows one captured revision, checks that revision again on every edit and decision, and keeps unsaved feedback through failures.

[Planning](planning.md) covers how a plan is written and what happens after acceptance. Editable-region and draft-comment mechanics are in [Buffers and rendering](buffers-and-rendering.md).

## Document

PlanReview opens with one compact block snapshot. Modified files contribute changed chunks and enclosing declaration boundaries through the shared diff presentation, and later revisions send disjoint block patches compared by stable block identity. Sections appear in order: proposed declaration changes, Tests, Checks, and Verification. Rust test attributes and test-only `cfg` gates are masked in both views.

Declaration comparisons first match unique Tree-sitter callable owners, then run the histogram matcher on the gaps between them. This follows Git's [patience](https://github.com/git/git/blob/c51408889c3b372814bc20b9431c3b74d0d268eb/xdiff/xpatience.c) approach, with callable identities as anchors. Removed-row navigation opens the previous proposal, or the source baseline on the first submission, and never the current worktree.

## Navigation

`.` and `open` resolve the selected token through saved Tree-sitter coordinates. A target inside the plan is revealed in PlanReview, opening folds and restoring omitted context. A target outside the plan opens its real source buffer.

Source-backed definitions use an already-running project language server on demand. Rust supplies the real path, digest, position, and filetype, Lua loads the file into a hidden buffer and requests `textDocument/definition` without autostarting a server, and Rust validates the document, revision, view, and digest before mapping the result. Requests have a five-second deadline, discard superseded results, and are never cached. No generated source, stub project, or dependency download is involved.

Inline-code identifiers in prose resolve against the complete proposed declaration index. Unique matches get their declaration-kind color, and ambiguous names open a chooser. An unmatched name may query `workspace/symbol` on at most four existing clients, with at most 256 results each and a five-second deadline. That query excludes captured files, so deleted declarations cannot reappear.

## Review edits

Reviewers can rename declarations and delete tests in place. Renames preview every affected span. `session.plan.test.delete` removes only the selected inventory entries, and `session.plan.check.update` changes the plan's check selection through the same versioned path. Each edit validates the captured view and source version and returns an updated projection, so the review never reloads. Confirmed deletions show immediately and restore on failure.

The plan keeps an ordered `manual_changes` history, which acceptance, revision feedback, and resumed execution show to the agent. Each edit advances the review digest, while submitted snapshots stay unchanged. Accepting or requesting changes freezes one snapshot of the edited draft and records one revision.

## Annotations and questions

Annotations attach to a source line, and `Ctrl-S` saves feedback and submits unanswered questions. Revision requests carry structured comment anchors. Feedback formatting finishes before the broker changes plan state, so an invalid anchor cannot drop comments from a revision.

`session.plan.question.answer` forks the source session once per question. Each fork keeps conversation history and receives the saved design, plan ID, version, source digest, and exact selected text with canonical plan line numbers. Up to three lines before and after the complete selection are labeled separately as context. Semantic declaration excerpts retain their own file line numbers, distinct from plan line numbers. All excerpts come from the captured review snapshot, not current workspace files. The fork answers about the selected text without writing files or changing the plan. Answers persist in the annotation store. Editing a question invalidates its answer, so a late response cannot attach to different text. `/config` exposes a provider-local Ask model, thinking level, and speed.

## Decisions

Only one decision can be pending at a time. Submitting a decision checks that the plan and buffer still match the captured revision, hides the review, and returns focus to Harness while the broker continues. A failed request reopens the review without losing unsaved feedback. Harness never opens PlanReview automatically.

Accepting nonempty inline comments uses the same revision feedback as requesting changes. Each comment retains its saved source anchors and any associated question-and-answer thread. Question-only threads do not become revision instructions. `PlanAcceptance` retains the captured comments until execution choices are confirmed. Harness then revises the plan instead of entering implementation.

`ActiveTask.auto_accept_plan` controls acceptance independently of the plan lifecycle. `/plan accept on/off` changes it without replacing the task or invalidating an active provider turn. Accepting inline feedback enables it; requesting changes preserves it. Complete submissions consult the current flag, and accepting a submission does not consume it. Execution inherits the planning task's policy. Clearing or replacing the task discards it.

`ActiveTask.plan_execution` retains selected execution settings and review context separately from acceptance policy. The input coordinator rechecks the flag under control admission before promoting a current submitted document, including recovery after interrupted promotion. Drafts, stale submissions, and paused tasks cannot promote. Review entries retain their originating exchange ID; revision dispatch resumes that exchange and its checkpoint rather than displaying another task-start heading.

After submission, a prompt for the selected waiting Plan task becomes a plan discussion. A successful edit there applies `ChangesRequested` and requires a new submission or question before completion.

## Question picker

A question-only turn saves `PlanElicitation` under an `AwaitingInput` plan and opens the shared picker across the bottom of the Harness surface. Provider choices default to the keys in `picker.choice_keys`. After the final answer, a review page lists every answer before the provider resumes.

Answer and skip requests update only the durable elicitation. Later prompts run read-only follow-ups, where the provider may keep the questions, replace them through `question_ask`, record an explicit user answer through `question_answer`, or remove them through `question_withdraw` when no decision remains. A model recommendation never counts as an answer. Revisions keep only answers that remain valid, and missing answers become best-judgment decisions only when the user explicitly continues.
