# Diff presentation and analysis

Each view that rendered its own diffs would disagree about hunks, gutters, and folds, and a slow comparison could stall the editor. `forge-diff` therefore owns one presenter for every view and one bounded analysis engine, and neither one rejects a file for its size.

[Buffers and rendering](buffers-and-rendering.md) covers publication.

## Shared presenter

`forge-diff::presentation::DiffPresenter` owns file and hunk headings, row styling, gutter widths, and fold boundaries for Status, PlanReview, PR and commit reviews, walkthroughs, and Harness change trees. Consumers supply rows, block identities, and domain targets, and the presenter never mutates repositories. It keeps each syntax capture's parser language, preserves query precedence for overlapping captures, and sizes gutters from the whole display group, so chunks of one hunk stay aligned.

## Views of a comparison

`DiffViewState` holds independent All/Public visibility and Full/Semantic presentation per document, bound to `ov` and `ox`. Source views start Full/All, and PlanReview is Semantic-only and starts Public. `SourceDiffProjection` keeps one exact comparison with separate Full, semantic-heading, and declaration-body cursors. `SourceComparison` matches declarations and classifies raw edits against them.

`SavedDiffProjection` validates saved patch evidence against complete captured sources, and falls back explicitly to raw rows when evidence is missing or mismatched. Harness resolves those sources from checkpoint storage, never from the current worktree.

## Compact rows

`DisplayCursor` keeps changed rows, context between grouped edits, and enclosing structural boundaries, and omits ordinary context outside each group. Each delivery targets 256 source rows and 128 KiB of text, but a single long line always makes progress and is never split. No file size or line count disables expansion.

Intraline emphasis compares at most 64 paired lines, 16 KiB, and 256 spans per replacement. Larger or unpaired replacements fall back to base added and removed styles.

## Analysis engine

`ForgeRuntime` owns one `DiffEngine` with four job slots on an `AnalysisPool` of at most four threads. Requests check the cache and join matching in-flight work before reserving a job, and one analysis serves at most 64 waiting callers. Dropping the last waiter removes queued work, while running native work finishes and its result is discarded. Foreground, visible, and speculative queues keep FIFO order, and a foreground consumer promotes matching queued work. Priority never interrupts running code, because the histogram algorithm has no cancellation checkpoint.

Shutdown drains analyses and repository reads within the two-second host deadline, and reports unfinished work instead of releasing it. Content size and cache-budget exhaustion never reject an explicit analysis.
