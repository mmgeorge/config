# Architecture Decision Record Profile

*Read when recording a significant design decision for future maintainers.*

## Goal

Show why one design choice answered a specific problem under explicit constraints, why credible alternatives were rejected, and what ongoing costs and failure modes the team accepted. A future maintainer should be able to tell whether the assumptions behind the decision still hold before reopening it.

## Sections

Write these sections in order.

### Title and Status

- Give the record a concrete title naming the choice and its purpose (for example, `ADR-004: Journal unsent edits in IndexedDB for offline sync`).
- Record status (`Proposed`, `Accepted`, `Rejected`, `Superseded`, or `Deprecated`), date, authors, reviewers, and any record it supersedes.

### Context

- Describe the current behavior and the concrete failure or constraint forcing a change.
- State the requirements fairly without slanting them toward the chosen option.

### Decision

- State the chosen design in the present tense: which components change, who owns each piece of data, and what invariant holds across the boundary.

### Alternatives Considered

- Evaluate each credible alternative against the exact same constraints and state the decisive reason it was rejected.

### Consequences

- Document what the decision enables, what operational or code complexity it adds, and any new failure modes other components must handle.

### Verification

- State how automated tests, benchmarks, or lint rules verify the decision's guarantees.

## Leave Out

- Full system overviews, API manuals, or implementation ticket lists (link those documents instead).
- Re-litigating settled decisions unless constraints changed or measurements show a failure.

## Example

> ### ADR-004: Journal Unsent Edits in IndexedDB for Offline Sync
>
> **Status:** Accepted (March 2026)
>
> **Context:** The document editor applies edits in memory immediately and pushes autosaves directly from memory to the `DocumentApi`. When a tab crashes, reloads, or loses connectivity before an autosave completes, every unconfirmed edit is lost. Any replacement must keep typing off the network path while keeping unsent edits durable across crashes and offline periods.
>
> **Decision:** The `EditorView` continues mutating the open document in memory immediately and appends each sequenced `DraftRecord` to a per-document IndexedDB `DraftJournal`. A single background `SyncWorker` drains pending records in sequence order to the `DocumentApi` and deletes each entry only after the server confirms persistence.
>
> **Alternatives:**
> - *In-memory retry queue:* Avoids local storage writes, but loses every unsent edit if the browser tab crashes or reloads.
> - *`localStorage` snapshots:* Simpler API than IndexedDB, but runs synchronously on the main thread and hits tight 5 MiB caps on large documents.
>
> **Consequences:** Unsent edits now survive crashes and offline intervals at the cost of a per-edit IndexedDB append and a background sync loop to maintain. Storage quota exhaustion becomes a new failure mode: when IndexedDB rejects a write, the `EditorView` marks the document unsaved and warns the user that new changes will not survive a reload until space is freed.
>
> **Verification:** An end-to-end Playwright test disconnects network routing mid-edit, reloads the tab, restores connectivity, and asserts that every edit reaches the server in sequence order with zero duplicates.
