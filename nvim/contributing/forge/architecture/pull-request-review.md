# Pull request review

A comment posted twice, or a description overwritten by a stale save, is visible to everyone on the pull request. Forge therefore keeps unsent text in Neovim, sends only explicit captures, and treats a lost response as unknown rather than failed. Lua owns unsent fields and local drafts until the host acknowledges their revisions, and Rust owns submitted state and reconciliation.

[GitHub services](github-and-review.md) covers the remote queue and uncertainty scopes. [Buffers and rendering](buffers-and-rendering.md) covers editable regions.

## Creating a pull request

The workflow pins two commits before generating anything: the selected base branch and the captured head. When `origin` is configured, it fetches only the base branch, and a failed fetch stops creation instead of reusing a stale ref. Git reads and the fetch share a 30-second deadline.

The `context` operation on `repository.generation` compares the merge base of the pinned commits with the head and reads committed objects only, so base-only changes and uncommitted work never enter the prompt. It first runs `git merge-tree --write-tree --quiet`. Exit status 1 returns `conflicts`, and Lua stops before any model request, push, or PR creation. The check changes no branch, index, or working file, although Git can write unreferenced merge objects.

Descriptions use the commit-message formatter, and the `pull_request` operation sends evidence to `HarnessService.background`, so PR and commit generation share a model and conversation. Generation rejects a HEAD that differs from the captured revision. Lua never launches a provider CLI for this. See [Commit messages](commit-messages.md).

## Lifecycle and fields

`github.pull_request` binds each transition to the repository, number, and expected node identity. Rust reads current remote state before choosing at most two writes. Merged PRs reject lifecycle changes, an already satisfied target performs no write, and a proven second-step rejection returns the confirmed intermediate state. After an unknown result, Lua requests exactly one read-only reconciliation and reports the original failure.

Title and body edits are optional fields of the same route. Omitted fields stay unchanged, an empty body clears the description, and blank or multiline titles are rejected. Matching remote text is a confirmed no-op. Otherwise one `updatePullRequest` mutation must confirm identity and the submitted values.

## Review documents

`ReviewService` holds at most 64 open PR documents and admits at most 32 asynchronous jobs, using the `GithubService` queue for every remote operation. Each `ReviewDocument` owns an `EditStore` of accepted field text and saved baselines. A region edit carries document, region, revision, and sequence identities, and a stale edit leaves the field unchanged. Document identities include a service UUID, so a request from an old host cannot reach a replacement host's document.

Typing sends no request. `<C-s>` or `:w` captures the exact native field text as an immutable save, including CR characters and trailing empty rows. Confirmation advances only the submitted baseline, so text typed after the capture stays dirty. An unknown outcome holds later publication until `github.review.document.reconcile` reads remote truth, and a failed observation keeps the capture and offers retry through `gR`. Lua runs one active publication and keeps at most one queued explicit action.

Opening a PR keeps the current buffer until the title, metadata, and description are ready. Files, checks, conversation, and commits then load concurrently, and a failed section shows its diagnostic without hiding the others. Lua keeps at most eight PR documents with their buffers. Reopening shows retained text immediately and refreshes clean fields while keeping local edits.

Rust validates reviewer names before any field write, so a bad reviewer request cannot cause a partial save. GitHub has no reviewer-operation identity, so recovery closes the unknown operation after observing the current reviewer set.

## Comments

`CommentStore` gives each comment or draft a stable `CommentId`, and each displayed copy a separate `CommentOccurrenceId`. Focus selects exactly one occurrence, and removing an occurrence never discards its body or reply draft.

A remote update that differs from local text keeps a conflict candidate beside the local text and baseline, and the user resolves it with KeepLocal or TakeRemote. Pending saves reject refreshes, and unresolved conflicts reject saves.

A save captures create, edit, reply, or delete intent, and one pending operation per comment prevents repeated posting. Edit and delete read the remote owner first and require its exact node, database identity, and PR-scoped URL. Creation verifies the parent PR, and confirmation requires the echoed receipt, the expected subject, viewer ownership, and the exact body.

A lost creation response stays unresolved, because neither an absent comment nor a matching body proves what happened. The `gR` picker offers remote observation, a linked confirmed comment ID, not-dispatched rejection, or close-unknown.

Browser actions resolve an explicit comment occurrence or source anchor, so adjacent code never inherits a nearby comment URL.

## Batched review

A `ReviewDocument` is in `Overview` or `Batched` mode. Batched mode adds an editable summary and a viewed-file set, both stored in the durable review draft. Lua changes these only through `github.review.document.batch.begin`, `github.review.document.file.viewed.set`, and `github.review.document.batch.submit`.

A submission captures the summary and every inline draft and sends them in one review request. The operation ID and complete capture reach durable storage before dispatch. An unknown outcome survives reopen, blocks another submission, and requires explicit recovery through `gB`. A missing recovery journal cannot establish confirmation.

## Commit details

Commit detail reads resolve a full SHA to its first parent, and expansion compares those two commits. A root commit shows an explicit unavailable state. `CommitDiffStore` keeps at most 64 records and 16 MiB for reuse, and a larger diff still renders completely from its response.
