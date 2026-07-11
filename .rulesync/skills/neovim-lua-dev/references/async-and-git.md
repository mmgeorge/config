# Async & Git in Interactive Neovim Plugins

*When to read this:* before any code that shells out to `git`/`gh`, parses a diff, stages/unstages hunks, runs `git commit` from inside nvim, or does Tree-sitter work in a render/keymap/autocmd path. This is the rule set for keeping the editor responsive and never corrupting git state. For Trouble/Snacks render mechanics see trouble-and-snacks.md; for the bug catalog see common-bugs.md.

The plugin lives at `D:\config\nvim\lua\diff_review`. Every pattern below is taken from it.

---

## Never block the UI: the async process seam

Do **not** call `vim.fn.system`, `vim.fn.systemlist`, or `vim.system(...):wait()` from a render path, keymap, autocmd, cursor handler, or anything that runs while the user waits. Route every subprocess through the async backend seam in `git/git_backend.lua` (`system_text_async`, `systemlist_async`, `run_git_async`), which wraps `vim.system` with a callback.

Always request explicit capture. On Windows, MSYS/Cygwin children spawned with implicit stdio emit `dtable::stdio_init: couldn't make stderr distinct from stdout` into a status buffer. Pass all three:

```lua
vim.system(command, { text = true, stdin = input, stdout = true, stderr = true }, function(result)
  vim.schedule(function() cb(result) end)
end)
```

For "is this tool installed?" use `vim.fn.executable("gpg") == 1`, never `os.execute("which gpg")` — `which` is itself an MSYS child that leaks the same stdio error. The backend `pcall`s `vim.system` and, on spawn failure, still calls `cb` with `code = -1` and the message in `stderr`/`output` — a failed spawn must never silently drop the callback.

---

## Schedule editor mutations; make failures loud

Process callbacks run off the main loop. Wrap every buffer/window mutation in `vim.schedule`. Inside the schedule, branch on `result.code` **before** touching the editor and surface a real message:

```lua
vim.schedule(function()
  if result.code ~= 0 then
    notifications.error("Stage failed: " .. (result.output ~= "" and result.output or ("git exited " .. result.code)))
    return
  end
  -- safe to mutate buffers here
end)
```

"Request failed" must stay distinct from "succeeded with zero results." Do not cache a failed request as empty data, leave a spinner running, or return an empty list from a completion source. The backend preserves combined stdout+stderr in an `output` field (`git_backend.system_output`) precisely so the notification keeps the actionable text. The same loud bias applies to any caught error that affects a visible workflow — notify, don't log-and-swallow.

---

## Monotonic request-id cancellation

Every async refresh bumps a counter and captures it; stale callbacks bail. See `views/status/render_orchestrator.lua`:

```lua
session.status.request_id = (session.status.request_id or 0) + 1
local request_id = session.status.request_id
git_backend.git_root_async(function(cwd, root_err)
  local latest = session.states and session.states[buf] or render_state
  if not (latest and latest.request_id == request_id) then return end
  -- ...still current; render
end)
```

Re-check `request_id` at **every** async hop (root lookup, status load, metadata load), not just the outermost one — a slow first call can otherwise repaint over a newer refresh that already completed.

PR discovery follows the same rule across two hops. First list every PR for the named
head branch, then fetch full details only for the selected candidate. Re-check the status
request id before starting the detail request and again before rendering its result. The
selection policy belongs above the transport: newest active PR first, newest closed PR as
a user-confirmed fallback, and merged PRs excluded.

Composite PR lifecycle changes also run serially. `CLOSED -> DRAFT` reopens before it
converts, while reopening a draft as `OPEN` must mark it ready after the reopen. If the
second mutation fails, return and render the intermediate GitHub state before notifying
the failure. Hiding that partial success leaves the editor claiming `CLOSED` after GitHub
already reopened the PR.

---

## Optimistic UI model, reconcile after the queue is idle

Keep the UI model separate from backend (git) state. A user action updates the in-memory model and renders **immediately**; the async git command reconciles afterward. Never flash a loading state during an action-triggered refresh — only on first load or when there is no useful previous model. In `views/status/actions.lua` a stage does:

```lua
status_mark_diff_paths_pending(action_entries, { "unstaged", "staged" })
status_apply_optimistic_entries(action_entries, "staged", target_id)  -- model + render now
status_enqueue_operation(function(done)
  git_data().stage_patch_async(diff, function(ok)
    -- failure already notified inside stage_patch_async
    done()
  end)
end)
```

If the user stages then immediately unstages before git finishes, the second action operates on the optimistic model, and the git commands still run in order. Reconcile from git **once the queue drains**, not after each command while later ones are pending.

---

## The mutation queue: sequential, FIFO, debounced reconcile

Parallel `git add` / `git restore --staged` / `git checkout` / `git apply --cached` race on `.git/index.lock`. Run index mutations async but **sequentially**. `render/mutation_queue.lua` is a tiny FIFO: `enqueue` appends and calls `run_next`, which refuses to start a second op while `running`, and flushes `on_idle` callbacks only when both `running` is false and the queue is empty.

```lua
status.operation_queue_model = status.operation_queue_model or mutation_queue.new()
mutation_queue.enqueue(status.operation_queue_model, operation)
mutation_queue.on_idle(status.operation_queue_model, function()
  status_request_reconcile(status.reconcile_buf, status.reconcile_target_id)
end)
```

Reconcile is **debounced and cancelable** (`actions.lua`, `status_reconcile_delay_ms = 120`). `status_request_reconcile` bumps `reconcile_generation`, then `vim.defer_fn`s a check; if a newer generation exists or operations are still pending, it bails. Critically, **enqueuing another mutation also bumps `reconcile_generation`**, which cancels any pending reconcile — so `S S S` does not repaint the backend snapshot from after the first stage on top of the optimistic UI. While mutations are queued or running (`status_operations_pending()`), suppress unrelated full status loads and async enrichment rerenders; the final debounced reconcile refreshes from git once the burst settles.

---

## Cursor restore: passive rerender vs explicit action target

There are two distinct flows, and conflating them makes the cursor jump:

- **Passive async rerender** (Tree-sitter context arrived, syntax finished): no explicit target means "preserve wherever the user is **now**." Capture the stable item id + raw cursor line *inside the callback, immediately before mutating lines* — never when the request started, because the user moves while git/Tree-sitter work is in flight.
- **Action rerender** (stage/unstage/discard): pass an explicit semantic `target_id` chosen by the action (the next hunk, or the destination section header).

In `render_orchestrator.lua`, `preserve_current_cursor = target_id == nil and fallback_line == nil` encodes exactly this. Bad: `local id = cursor_target(buf); load_async(function(r) render(r, id) end)`. Good: capture inside the callback.

---

## Parsing the unified-diff `@@` header

Fetch raw hunks with **zero context** so the UI can inject its own. The diff command in `git_backend.git_diff_command` is `git -C cwd -c core.quotepath=false diff --no-color --no-ext-diff --unified=0`. The header parser (`git_data.lua`):

```lua
local old_start, old_count, new_start, new_count, context =
  line:match("^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@ ?(.*)")
-- a missing count means 1: old_count == "" and 1 or tonumber(old_count)
```

`+new_start` is the **1-based** line in the new file where context *starts*, not the first changed line; with `--unified=0` there is no context, so it is the first change. The body parser counts `+`/`-`/` ` prefixes against the declared counts to know when a hunk ends, and treats `\ No newline at end of file` lines (`^\`) as trailers.

---

## Raw hunks vs virtual display hunks

Keep two models. **Raw hunks** (zero-context, exactly as git emits) are the source of truth for patches, line mappings, staging, unstaging, discard, viewed state, and comments. The **display layer** may add bounded syntax-aware context and merge adjacent raw windows into one rendered hunk with a single header — that merge is presentation only.

Never feed a synthesized virtual `@@ +N -N` display header back to git: it may summarize several zero-context hunks and omit or invent context. Before any operation that writes git or durable review state, expand back to the raw hunks. Tests should assert both sides: raw-action tests check the original hunk diff/line mapping; UI tests check that adjacent changes render without duplicate headers.

---

## Staging / unstaging individual hunks

Use `git apply --cached`. Because hunks are fetched with `--unified=0`, you **must** pass `--unidiff-zero`, or git rejects zero-context patches. `git_data.stage_patch_async`:

```lua
git_backend.run_git_async(
  { "apply", "--cached", "--whitespace=nowarn", "--unidiff-zero", "-" },
  diff .. "\n",
  function(result) if not result.ok then notifications.error(...) end end
)
```

- The patch text must include the file header (`diff --git`, `---`, `+++`) **and** the hunk (`@@` + lines), with a trailing newline.
- `--whitespace=nowarn` avoids whitespace noise; `--unidiff-zero` is mandatory for `--unified=0` hunks.
- `run_git_async` runs from the git root (`git -C root`), so paths in the patch are root-relative.
- For unstage, add `--reverse` — same patch, reversed application.

---

## Empty new files and untracked files

`git diff --cached` for an empty new file produces `new file mode` but **no `@@` header**. A parser that only emits hunks on `@@` will drop the file; cross-check `git diff --name-status` (or `--cached --name-status`) for files that produced zero hunks and represent them explicitly.

Untracked files have no git diff at all — `git diff` and `git diff --cached` ignore them. Do not shell out for one. Synthesize a "new file" patch from disk (`git_data.build_untracked_diff`): `vim.fn.readfile`, prefix every line with `+`, header `@@ -0,0 +1,N @@`, and skip binary files (a NUL byte in any line). Feed it through the normal render path so it previews as one all-additions hunk.

**The false-sentinel cache trap** (`views/diff_buffer.lua`): the per-file cache `session.file_diffs` is keyed by filename, and the "do I need to fetch?" guard must be `== nil`, not falsy. Untracked/empty files yield no diff; if you cache `nil` there, every cursor move re-runs the fetch (for tracked files, two full-repo `git diff` calls — visibly laggy). Cache the boolean `false` for "checked, no diff" and reserve `nil` for "invalidated, must re-fetch":

```lua
session.file_diffs[filename] = diff_text or false   -- false = checked-but-empty
-- guard elsewhere:  if not session.file_diffs or session.file_diffs[filename] == nil then fetch() end
```

Set `nil` only on real invalidation (e.g. `BufLeave` back to a tracked file).

---

## Windows path normalization

Windows paths use `\`; git emits `/`. Normalize both sides before comparing, every time:

```lua
if vim.fs.normalize(paths.repo_file_path(cwd, hunk.file)) == vim.fs.normalize(filename) then ... end
```

`git_data.file_diff_and_flags_async` does exactly this to match a hunk's git-relative path against an absolute buffer path.

---

## Driving `git commit` from inside nvim (the fake-editor bridge)

You cannot run `git commit` with an interactive editor inside the running nvim — git blocks on an editor it can't reach. Point `GIT_EDITOR` at a **headless child nvim** that RPCs back to the parent (`integrations/commit.lua`, modeled on Neogit):

1. Run `vim.system({ "git", "commit" }, { cwd = root, env = { GIT_EDITOR = editor, GIT_SEQUENCE_EDITOR = editor, NVIM = server }, text = true, stdout = ..., stderr = ... }, on_exit)`. Stream stdout/stderr into a **console buffer shown in the existing diff-preview window** (save its buffer + winbar first) — this is where pre-commit hook output appears before the editor opens. No new float.
2. `editor` is `nvim --headless --clean --noplugin -n -R -c 'set runtimepath^=<root>' -c "lua require('diff_review.integrations.commit').client()"`, built with `vim.fn.shellescape` per token because git parses it through a shell. `--clean` needs the `runtimepath^=` so the child can `require` the module.
3. The child `client()` reads `COMMIT_EDITMSG` from `argv()`, `serverstart()`s its own address, `sockconnect`s to `$NVIM` (the parent), and `rpcrequest`s `nvim_command` to call the parent's `editor(target, child_addr)`. Then it **keeps running** — git stays blocked on it.
4. The parent's `editor()` swaps that same window's buffer to the message buffer. On submit (`<C-c><C-c>`) it writes the file and `rpcnotify`s the child `qall` (exit 0 → git commits); on abort (`<C-q>`/`q`) it sends `cq` (exit non-zero → git aborts). A `BufWipeout` autocmd also sends abort, so closing the buffer any other way never hangs git.
5. `on_exit` (`M._finish`) hands the window back: success/abort restore the saved buffer + winbar and refresh; a real failure shows git's captured output until `q`. Distinguish abort from failure with a flag (`st.aborted`) — both exit non-zero, but only failure should show the error.

Gotchas: set `NVIM` explicitly in the job env, don't assume inheritance; `serverstart()` if `v:servername` is empty; wrap `client()` in `pcall` and `cq` on any error so a child that never opened a buffer can't hang git; gate `auto_preview` on `session.suspend_preview` while the commit borrows the preview window, or a cursor move clobbers it; keep the **console buffer `bufhidden = "hide"`** (not `"wipe"`) so it survives the console→message→preview window swap and still has the captured hook output to show on failure.

---

## Async Tree-sitter in render paths

Tree-sitter parsing janks status buffers and previews. Never call synchronous `parser:parse()` in a renderer, preview, keymap, cursor handler, or autocmd. Use the ranged async form `LanguageTree:parse({ srow, scol, erow, ecol }, on_parse)`: render a cheap fallback (git's `@@` context) immediately, then upgrade when the callback fills the cache. `git_data.compute_hunk_context_async`:

```lua
local parse_ok, parsed = pcall(function()
  return parser:parse({ target, 0, target + 1, 0 }, function(first, second)
    local trees = type(first) == "table" and first or second
    vim.schedule(function() finish(trees) end)
  end)
end)
if not parse_ok then cb(nil)
elseif parsed then vim.schedule(function() finish(parsed) end) end  -- parse returned trees synchronously
```

Two subtleties: `parse` may return trees **synchronously** (already parsed) *or* via the callback — handle both, and guard with a `done` flag so `finish` runs once. The callback signature varies across Neovim versions, hence `type(first) == "table" and first or second`. Pair the upgrade with a `request_id`/stale check so a late parse doesn't repaint an older hunk (see Monotonic request-id above).
