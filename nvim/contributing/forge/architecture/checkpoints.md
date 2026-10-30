# Checkpoints and restoration

Undoing an agent's work is only safe if Forge knows exactly what the files contained before it started, and an interrupted restore must not leave a truncated file. Harness therefore captures content before the first provider turn, keeps it in a verified object store, and journals every restoration so a restart can finish it. Capture never creates commits, changes refs, stages files, or writes the user's index.

[Git repositories](git-repositories.md) covers source reads, and [Harness](harness.md) covers the exchange lifecycle.

## Capture

Every Git interaction captures a baseline before its first provider turn and a terminal checkpoint when it completes, fails, or cancels. The first baseline includes pre-existing staged and unstaged content. `CheckpointRecord` stores a Git tree ID, complete content overrides, deletions, modes, sampled checkout conversion rules, and Git LFS exclusions. Status refreshes a private index copy through `GIT_INDEX_FILE`. LFS paths are never opened, filtered, or restored.

The initial status inventory defines the candidate set, so a clean file edited afterward belongs to the next checkpoint. This is per-file capture, not an atomic filesystem snapshot. Clean files resolve through the recorded tree, and only changed files need content acquisition. Capture streams each file into `ObjectStore::put_file`, compares source stamps before and after, and publishes an object only when both agree. A matching status observation reuses the previous record without enumeration or copying. The `checkpoint_latency` release example enforces a one-second budget for capture and persistence on a large worktree.

## Tool barrier

An edit made before the baseline exists cannot be undone, so tools wait for capture. The broker persists the baseline before releasing the exchange's tool barrier. Codex waits on a temporary PreToolUse command that runs `forge checkpoint-wait`, Copilot uses its pre-tool hook, and providers without a hook wait before the model request. A resumed Codex thread with background terminals also waits before inference, because terminal stdin bypasses its hook.

Acquisition failure or the 25-second barrier deadline denies tool execution. If initial acquisition fails, finalization can retry, and because the closed barrier prevented edits, recovery records the current workspace as both boundaries. Capture runs on the shared read pool during provider reasoning, but it waits for intersecting mutation scopes first.

## Change attribution

At terminal capture, `ProviderChangeIndex` collects paths from successful structured file changes in the main timeline and every child-agent turn. The comparer produces a diff restricted to those paths and a full checkpoint diff, both from the same baseline-to-terminal comparison, so repeated edits and reversions collapse into one patch. The full comparison supplies **Workspace changes**, which can include command, formatter, child, and external edits. Provider attribution stays internal and never claims exclusive authorship. Non-Git exchanges keep provider-reported edits without any checkpoint or rollback.

## Comparison

Checkpoint comparisons run on the runtime `DiffEngine`. `forge-diff::raw` uses histogram comparison with `imara-diff`, and its hunk identities include both source hashes, representations, and exact byte ranges. `SourceVersion` keeps complete bytes with a lazy SHA-256 identity. Saved patch presentation compares LF and CRLF lines by text and reports line-ending conversions separately, while the raw patch and restore keep the original bytes.

Finalization commits the terminal checkpoint and exchange together, and only a successful commit replaces live comparison state. A failure keeps the earlier evidence and records the error. An empty patch shows no workspace-change row, and missing evidence shows **Workspace changes unavailable**.

## Object store

`ObjectStore` owns the content-addressed checkpoint directory independently of SQLite. Publication writes and syncs a same-directory temporary file without replacing an existing identity, and reads verify content hashes. `ObjectStore::get` has no content-size threshold, so file size alone never yields `Diff unavailable`.

## Restoration

`session.exchange.rollback.prepare` computes only the paths that changed between the expected and target checkpoints, saves recovery copies of the affected files, and stores an immutable preview outside the worktree. A changed HEAD or a later edit adds an overwrite warning. `session.exchange.rollback.apply` carries only the preview ID, and revalidates the paths and HEAD under the index, worktree, and shared-reference scopes. Unsafe paths, symlink ancestors, missing objects, and unsettled exchanges reject before mutation.

`ObjectStore::restore_file` replaces each destination from a verified same-directory temporary file, so an interrupted write cannot leave a truncated file. A separate progress journal records Applying, FilesComplete, or Complete, and each file operation compares its actual state with both original and target content, so a restart resumes correctly. A file in a third state stops the restore with a visible error. A pending restore blocks new workspace work until Harness undo offers to continue it or restore the pre-restore files.

Restoration covers file content only. Git's index, branches, commits, and ignored output stay outside its scope, and because it never writes the index, staging changes do not invalidate a preview.

The checkpoint store has no legacy decoder. Opening it removes sessions that own old-format checkpoint records, and keeps preferences, prompt history, and provider-owned sessions.
