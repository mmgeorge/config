# Async and Git Operations in Interactive Neovim Plugins

This reference establishes asynchronous process execution, Git state management, optimistic UI state management, patch manipulation, and Tree-sitter scheduling patterns for Neovim plugins.

## Non-Blocking Asynchronous Process Execution

Never invoke synchronous process functions such as `vim.fn.system`, `vim.fn.systemlist`, or `vim.system(...):wait()` from render paths, keymaps, autocommands, cursor handlers, or interactive callbacks. Route subprocess calls through the asynchronous backend seam in `git/git_backend.lua` (`system_text_async`, `systemlist_async`, `run_git_async`), which wraps `vim.system` with structured callbacks.

Always request explicit stream capture. On Windows, child processes spawned with implicit stdio can emit `dtable::stdio_init: couldn't make stderr distinct from stdout` into editor buffers. Pass explicit descriptors:

```lua
vim.system(command, { text = true, stdin = input, stdout = true, stderr = true }, function(result)
  vim.schedule(function() cb(result) end)
end)
```

To test tool availability, use `vim.fn.executable("gpg") == 1` rather than `os.execute("which gpg")`. The backend wraps `vim.system` in `pcall` and invokes the callback with `code = -1` and the diagnostic message in `stderr`/`output` on spawn failure, ensuring callbacks are never silently dropped.

## Scheduled Editor Mutations and Error Reporting

Process callbacks execute outside the main Neovim event loop. Wrap every buffer or window mutation in `vim.schedule`. Within the scheduled callback, evaluate `result.code` before mutating editor state and report actionable error notifications:

```lua
vim.schedule(function()
  if result.code ~= 0 then
    notifications.error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return
  end
  -- Apply buffer mutations safely
end)
```

Distinguish failed requests from successful empty responses. Never cache a failed request as empty data, leave uncancelled UI indicators active, or return empty completion lists without reporting the error.

## Monotonic Request ID Cancellation

Assign an incrementing request ID to each asynchronous refresh and capture it locally. Discard callbacks with stale IDs to prevent older asynchronous responses from overwriting newer state:

```lua
session.status.request_id = (session.status.request_id or 0) + 1
local request_id = session.status.request_id

git_backend.git_root_async(function(cwd, root_err)
  local latest = session.states and session.states[buf] or render_state
  if not (latest and latest.request_id == request_id) then return end
  -- State remains current, proceed with render
end)
```

Re-check `request_id` at every asynchronous hop (root lookup, status load, metadata load). Multi-hop operations like PR discovery must verify the request ID before requesting full details and again before rendering the final result.

## Optimistic Journal and Authoritative Synchronization

Keep the rendered UI state decoupled from raw Git repository state. Store a confirmed baseline alongside ordered optimistic layers. Staging or unstaging appends an optimistic layer, projects the section and per-file diff caches, and renders immediately before the background Git command runs:

```lua
mutation_coordinator.enqueue(root, {
  paths = path_list,
  on_enqueue = function(task)
    status_sync.apply_optimistic(root, task.burst_id, entries, target_section)
  end,
  execute = function(done)
    index_mutation.execute_async(root, mutation, done)
  end,
})
```

Subsequent keypresses interact with the optimistic UI model. When an earlier mutation completes while newer layers remain queued, commit the resolved layer and replay pending layers over the updated baseline.

## Serialized Index Mutations and Path Synchronization

Parallel index operations (`git add`, `git restore --staged`, `git rm --cached`, `git apply --cached`) contend for `.git/index.lock`. Serialize index writes through a mutation coordinator keyed by repository root.

When the queue drains and a quiet window (120 ms) expires, request an authoritative snapshot for the union of modified paths using three path-scoped commands:

- `git --no-optional-locks -C <root> status --porcelain=v2 -z --untracked-files=all -- <paths>`
- `git --no-optional-locks -C <root> -c core.quotepath=false diff --no-color --no-ext-diff --unified=0 -- <paths>`
- `git --no-optional-locks -C <root> -c core.quotepath=false diff --no-color --no-ext-diff --unified=0 --cached -- <paths>`

If the snapshot matches the optimistic state, retire the confirmed layers without redrawing the buffer. If a semantic mismatch occurs, replace the affected paths and execute a single corrective render.

## Cursor Restoration Invariants

Differentiate passive rerenders from user mutations to avoid unexpected cursor jumps:

- **Passive Asynchronous Rerenders:** When Tree-sitter context or syntax highlighting finishes, preserve the user's current line. Capture the stable item ID and line index inside the callback immediately before line mutations.
- **Stage and Unstage Rerenders:** Do not explicitly restore or reposition the cursor. Allow Neovim to retain cursor placement relative to minimal line diffs.
- **Discard Operations:** Provide an explicit semantic target selected prior to deletion since the removed item no longer exists.

## Minimal Line Edits via Diff Reconciliation

When updating buffer contents from an authoritative snapshot, avoid clearing and rewriting the whole buffer. Compute line edit ranges with `vim.diff` using histogram indices and apply disjoint edits from bottom to top. Bottom-up edits preserve line numbers of earlier spans and prevent unnecessary extmark invalidations or `CursorMoved` event cascades.

## Patch Construction and Hunk Parsing

Retrieve raw hunks with zero context (`--unified=0`) so the UI layer can manage its own syntax context:

```lua
local old_start, old_count, new_start, new_count, context =
  line:match("^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@ ?(.*)")
```

### Raw Hunks vs Virtual Display Hunks

Maintain two distinct models:
- **Raw Hunks:** Zero-context hunks emitted directly by Git. These form the authoritative source of truth for line mappings, staging, unstaging, discard, and comments.
- **Display Hunks:** Merged or expanded representations with syntax-aware context. Never pass virtual display headers back to Git commands.

### Hunk Staging with `git apply --cached`

Apply zero-context patches to the index using `git apply --cached --unidiff-zero`:

```lua
index_mutation.execute_async(root, {
  direction = "stage",
  target_list = { { kind = "hunk", path = filename, diff = diff } },
}, callback)
```

Include the full file header (`diff --git`, `---`, `+++`) alongside the hunk body and trailing newline. Pass `--reverse` for unstage operations.

## Untracked and Empty File Handling

Untracked files do not produce diffs from `git diff`. Read untracked file contents through asynchronous libuv file operations, synthesize a `new file` patch with `@@ -0,0 +1,N @@` headers and `+` line prefixes, and route the patch through the renderer.

In the per-file diff cache, store `false` to represent "inspected, no diff present" and reserve `nil` for "invalidated, must fetch".

## Asynchronous Tree-sitter Parsing

Never call synchronous `parser:parse()` in render paths or cursor handlers. Use ranged asynchronous parsing:

```lua
local parse_ok, parsed = pcall(function()
  return parser:parse({ target, 0, target + 1, 0 }, function(first, second)
    local trees = type(first) == "table" and first or second
    vim.schedule(function() finish(trees) end)
  end)
end)
if not parse_ok then
  cb(nil)
elseif parsed then
  vim.schedule(function() finish(parsed) end)
end
```

Render fallback context headers immediately and upgrade asynchronously when the Tree-sitter callback resolves. Validate that the active request ID remains current before updating the buffer.
