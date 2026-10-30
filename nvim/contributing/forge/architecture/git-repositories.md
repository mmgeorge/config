# Git repositories and content

A review or restoration that reads the current worktree when it meant the captured one silently acts on the wrong bytes. Rust therefore owns repository identity, observation, and source acquisition, and every consumer keeps immutable source identities instead of re-reading files.

[Status](status.md) covers the Status view and index writes, and [Checkpoints](checkpoints.md) covers capture and restoration. Host workers and cancellation are in [Host and protocol](host-and-protocol.md).

## Repository identity

`forge-git` discovers worktree roots, private Git directories, common Git storage, and index paths. `WorktreeId` and `GitStorageId` are distinct opaque types: linked worktrees share storage but keep separate worktree identities and indexes, bare repositories have storage without a worktree, and unborn repositories need no index. A directory outside any repository becomes an untracked workspace, while configuration and I/O errors propagate.

`RepositoryPath` keeps raw Git bytes separately from its display label, and rejects empty, NUL-containing, absolute, and dot-component paths. Argument conversion preserves exact bytes on Unix, rejects unrepresentable Windows paths, and produces one argument without shell quoting. Code that runs Git must still disable pathspec interpretation explicitly.

On Windows, the initialize handshake loads a cached Git system prefix from `stdpath("cache")/rust-sidecar/forge/git-config-location.json`, which avoids a `git --exec-path` subprocess at startup. A miss uses gix discovery and saves the result, and cache failures warn without blocking startup. Explicit Git environment overrides bypass the cache. `:ForgeGitConfigCacheReset` clears the file, and Neovim must restart to take effect.

## Repository store and read admission

`ForgeRuntime` builds one `RepositoryStore`, which keeps at most 32 repository handles and admits four native reads through `BlockingReadPool`. The pool counts queued jobs, running jobs, and completed results awaiting collection, and rejects over-capacity requests immediately. It tracks declared input bytes without a byte ceiling. A worker keeps its reservation through caller cancellation until its result is collected. Dropping a `ReadTask` requests cancellation, but it cannot interrupt a native call already blocked in the operating system.

Clients hold `Arc<RepositoryState>` leases. Discovery deduplicates by worktree identity, or by storage identity for bare repositories. At capacity, the store evicts the least recently used unleased handle or returns an error.

Each read creates worker-local gix state, and its result carries the repository generation. Invalidation advances the generation and expires the shared gix handle, so the next worker reopens it. This matters because gix's timestamp-based index cache would otherwise keep stale staged entries when Git replaces the index with an equal or older timestamp. Storage invalidation advances every linked worktree's generation.

Shutdown closes admission, requests cancellation, waits up to two seconds, and reports unfinished reads as a host error.

## Observation

`StatusReader::status` combines HEAD-to-index and index-to-worktree events, requests every untracked path, evaluates submodule state, and tracks staged renames. It never writes gix's optional index refresh. A rename affects its removed origin, while a copy affects only its destination.

`RepositoryState::observe` serializes collection per repository and adopts only the latest requested result for the current generation, with no I/O under the state mutex. The collector reads HEAD and an index stamp, enumerates status twice, and rejects the result unless both enumerations, the stamp, and HEAD agree. Collection observes cancellation, invalidation, and a 30-second deadline.

Stamps sample metadata, such as file kind, size, and modification time, so an edit that preserves every sampled field goes undetected until later source verification. Index stamps read only the final 32 bytes of the index and each `sharedindex.<hash>` file.

## Content acquisition

`RepositoryState::content` acquires a Git object, an index stage, a worktree file, or a supplied source on the shared read pool, and returns distinct ready, binary, missing, unavailable, and failed outcomes. `FileContent` wraps `forge_diff::source::SourceVersion` with its provenance. No content request has a byte or line limit.

Object reads check hash format, blob kind, and advertised size, disable replacement refs, and verify bytes against the requested object ID when an exact identity is required. Worktree reads check metadata before opening, after opening, after streaming, and at the path afterward, and they reject growth or shrinkage instead of returning a partial source. Leaf opens do not follow symlinks, so a symlink's content is its target text. Directories and special files return unavailable.

Worktree requests select `Raw` or `GitCanonical` bytes. Canonical reads run the native gix conversion pipeline (line endings, text attributes, ident, and UTF-8 or UTF-16 encodings) before UTF-8 validation. Other encodings return unsupported. Configured clean or process filters return external-filter unavailable, because no filter process can start. Attribute resolution applies `$GIT_COMMON_DIR/info/attributes` after nested worktree rules, which corrects the precedence in the pinned gix-worktree.

## Statistics and caches

Statistics compare only changed sides with the pinned `gix-imara-diff` histogram algorithm, and counts and display hunks come from the same comparison. Each repository keeps a count cache of at most 8,192 entries and 4 MiB, keyed by object IDs, modes, and worktree metadata, so an edit that preserves time and size can leave stale counts. `RepositoryStore` and `DiffEngine` share one `AnalysisStore` of at most 512 entries, 128 MiB of sources, and 64 MiB of results. Explicit analysis may exceed those budgets and returns a live handle without caching. Cache entries never authorize a mutation.

## Revision completion

`RepositoryState::refresh_revisions` enumerates local and remote branches and keeps the complete list. Two equal enumerations fence detected ref changes, and a failed refresh keeps the accepted snapshot. `repository.revision.list` returns that snapshot in pages of at most 128 KiB, and later pages read the same snapshot without re-enumerating.

`revisions.lua` serves revision arguments for `ForgeBranchDiff`, `ForgeBranchDiffFile`, and `ForgeFileRevision` synchronously from cached strings, with no Git execution in the callback. A cold or stale lookup schedules one refresh, and stale refreshes run after ten seconds while keeping the previous list.

## Commands

`forge_git::command::read_command` owns one child and drains both pipes concurrently, preserving binary output and exit status. Cancellation, timeout, or reader errors terminate the child, wait for it, and join both readers. `file_command` streams large blob output into a temporary file with a 30-second deadline. Only diagnostic commands keep bounded output tails.
