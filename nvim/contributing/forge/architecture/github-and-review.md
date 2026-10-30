# GitHub services

A remote GitHub write can succeed while its response is lost, and two writers to the same pull request can interleave. Forge therefore gives the shared host sole ownership of remote operations, durable GitHub data, and reconciliation. No GitHub request needs Harness initialization, and GitHub failures never invalidate Harness state.

This chapter covers remote execution, storage, and issues. [Pull request review](pull-request-review.md) covers PR creation, review documents, and comments.

## Native `gh` execution

Every GitHub read and write runs through one `GhClient` owned by `ForgeRuntime`. At most four `gh` processes run across all checkouts. Each request passes explicit hostname routing and GraphQL variables through an owned temporary input file, under a 120-second deadline.

`process-wrap` contains each command in a Windows Job Object or a Unix process group. Cancellation or deadline expiry terminates that container, waits for exit, and drains both streams before releasing admission. Windows `KillOnDrop` also covers an unexpected host exit, and a Unix process group has no equivalent after forced host death.

Decoders keep complete collections without count limits and treat an empty collection as success. Missing fields, malformed JSON, invalid pagination, GraphQL errors, and command failures stay errors, with categories for rate limits, missing repositories, authentication, and local Busy admission. Failure diagnostics keep at most 64 KiB, and that budget never applies to successful content.

## Repository storage and leases

Repository data lives under `stdpath("data")/forge/github/<host>/repos/<owner>/<repo>/`. `RepositoryLease` coordinates processes through lock files beside the repository directory, so deleting and recreating the directory cannot split cooperating hosts across different locks. Ordinary operations take a shared lease, deletion takes an exclusive lease, and an incompatible request reports Busy immediately.

`GithubService` admits at most 8 blocking storage jobs and 16 asynchronous remote jobs, keeps admitted jobs after a caller drops its receiver, and drains them at shutdown. Remote waits never hold a database handle, so storage transactions proceed while a sync waits on the network.

Deletion runs through the `delete_cache` operation of `github.issue.list`. Rust takes exclusive ownership, rejects redirected paths, closes database handles, and removes the directory. A removal failure reports the error and can leave partial deletion.

## Issue synchronization

`issue_index.sync_repo` sends one typed `github.issue.sync` request, and `GithubService` owns pagination, freshness, rate-limit delays, page persistence, and leases.

Initial history resumes the saved cursor. Once history is complete, refresh starts at the newest page and stops at the high-water mark fixed before the refresh. Each page of at most 100 records commits its cursor, high-water state, and revision in one transaction, then publishes an open-issue snapshot, so a failed later page keeps earlier progress. Rate-limit failures get at most three retries with sixty-second timers. A non-manual refresh skips remote reads only when its history is complete, its snapshot exists, and its last check is under ten minutes old.

A process can die between a page commit and snapshot publication. Every preload therefore first sends `reconcile_snapshot`, which compares the published snapshot with committed state and rebuilds a missing or mismatched file without contacting GitHub. A database without a committed page reports not ready, not an empty success. Lua adopts only the reconciled revision.

`#` completion filters the adopted snapshot synchronously, and never issues a storage request, disk read, or GitHub search per keystroke. Lua coalesces sync requests by database path, and it rejects a resolved hostname that differs from `repo_cache.hostname()` before dispatch.

## Issue details and metadata

`github.detail` reads one issue through `gh issue view`, persists it under the repository lease, and returns a normalized `DetailRecord`. The decoder keeps every comment, label, assignee, and project item, and rejects mismatched repository or issue URLs. Lua shares one pending fetch per issue and may show a cached detail while refreshing.

The issue buffer `forge://issue/` uses `acwrite`, so `:write` produces an explicit field capture. Typing sends no request, and closing a dirty buffer hides it, so reopening restores unsaved text.

`github.metadata` merges contributors and collaborators into a case-insensitive map of unique logins. One failed source still yields metadata with its diagnostic, and failure of both keeps the previous file. A metadata job honors a ten-minute freshness window and atomically replaces `metadata.json`. Invalid cached JSON reports a failure instead of an empty result. Lua reads this file and never writes it.

Responses larger than one transport frame use request-correlated segmented delivery. Frame size bounds each chunk, never the total response.

## Remote mutations

`RemoteQueue` orders every write by normalized hostname, repository, and issue or PR number. It admits at most 64 operations and 64 resources, with FIFO order per resource and independent progress across resources. A running operation keeps ownership across multi-step transitions. `GithubService.run_mutation` owns queue admission, task retention, and shutdown, and keeps active writes after their response receivers drop.

A write with an unknown outcome marks its resource uncertain under an evidence scope: the pull request, one comment node, or one creation operation. Any unresolved scope blocks later writes to that resource, and only a read-only reconciliation in the matching scope clears it, so a title observation cannot settle a comment write.

Mutations echo a `clientMutationId` that the response must return. This correlates the immediate response but gives no server-side idempotency, so only a proven pre-dispatch failure counts as a rejection, and every other failure is reported as unknown. No mutation retries automatically. Uncertainty is process-local, and the review service persists submitted captures through its recovery store.
