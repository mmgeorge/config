# Rendering, Highlights, Virtual Text & Folds for Big Buffers

*When to read this:* before you render large diffs/lists, add per-row highlights or virtual text, touch the decoration provider, or implement folding/lazy syntax in a buffer that can reach tens of thousands of lines.

## The one insight: do NOT virtualize buffer TEXT

Neovim does not virtualize text. If you "page" logical rows into a small physical
buffer to fake scrolling, you break everything that reads real lines: `/` search,
`m`/extmarks, undo, `:split` independence, native folds, `gd`, and yank ranges. The
DiffReview render engine learned this the expensive way — the earlier viewport
projection had to be disabled (`status_diff_viewport_enabled = false` is now the
default) so that `init.lua`'s ~19.9k-line diff renders as **37,034 real buffer lines**
with instant native scroll and search.

Rule: **every logical row is a REAL buffer line.** Write them all with
`nvim_buf_set_lines` (see `status_write_rendered_buffer`). Do not be afraid of the line
count — Neovim handles a 37k-line buffer fine. What was slow was never the lines; it was
eagerly computing and baking **per-row decoration** (syntax/background/intraline
highlights) for rows nobody is looking at.

So the move is: keep all text real, but **scope the expensive decoration to the
viewport** via `vim.api.nvim_set_decoration_provider`. Its `on_win`/`on_line` callbacks
fire only for VISIBLE rows on each redraw, and they follow the viewport as the user
scrolls. You are not highlighting everything — you **postpone highlight application to
the instant a row becomes visible.** That is what made the 37k-line diff scroll
instantly.

```lua
-- render/decoration.lua / status_render.lua: provider fires per visible row.
vim.api.nvim_set_decoration_provider(ns, {
  on_win = function(_, _, buf, toprow, botrow)
    if buf ~= target_buf then return false end
    -- record/refresh the visible window; optionally schedule prewarm for it
    return true                       -- true => on_line will be called for this win
  end,
  on_line = function(_, _, buf, row)  -- row is 0-based, VISIBLE rows only
    local spans = status.diff_row_spans and status.diff_row_spans[row + 1]
    if spans then status_emit_row_spans(buf, ns, row, spans, true) end -- ephemeral=true
  end,
})
```

## Persistent vs EPHEMERAL extmarks

Split decoration by lifetime and let each kind live in its own namespace.

- **Structural chrome = persistent.** Headers, section lines, file/hunk rows,
  `line_hl_group`, and the gutter go into the stable `status_ns` once per render, applied
  by `status_apply_rendered_extmarks`. They survive until the next full render and do not
  depend on visibility.
- **Diff-body decoration = ephemeral.** Background bands, tree-sitter syntax spans, and
  intraline character highlights are emitted from the decoration provider's `on_line`
  into the separate `status_decorate_ns`, **only for visible rows.** Pass `ephemeral =
  true` to `nvim_buf_set_extmark`; Neovim drops them automatically after the redraw, so
  there is nothing to clear and no stale marks accumulate.

`status_emit_row_spans(buf, ns, row, spans, ephemeral)` is the single emit path used for
both: the `ephemeral` flag is the only difference. Precompute `spans` keyed by buffer
line (`status.diff_row_spans[line]`) so `on_line` stays a cheap table lookup — never
parse there (see below).

## THE TRAP: the gutter MUST stay a PERSISTENT extmark

This is the non-obvious constraint that will silently waste an afternoon. By the time
`on_line` runs, the line's **layout is already fixed.** Any decoration that *changes
layout* — inline virtual text that adds columns — is ignored when emitted ephemerally
from the provider. It simply never renders.

The gutter (line numbers + `+`/`-`/` ` sign) is exactly that: inline virtual text.

```lua
-- render/diff_render.lua: the gutter is inline virt_text => it shifts the row layout.
row.extmark = {
  virt_text = hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl),
  virt_text_pos = "inline",   -- LAYOUT-CHANGING -> cannot be ephemeral
}
```

Therefore: **only NON-layout decoration may be ephemeral.** That means `hl_group`
(background bands, syntax fg, intraline) and `line_hl_group`, which paint existing cells
without moving them. The gutter's inline `virt_text` must be a **persistent** extmark in
`status_ns`, applied up front in `status_apply_rendered_extmarks`, not emitted from
`on_line`. If you try to viewport-scope the gutter, the numbers vanish. Keep the gutter
persistent; viewport-scope only the colors.

## Defer APPLICATION first; defer COMPUTE second

The provider defers *application* — the cheapest, highest-leverage win, because it
removes per-row work for off-screen rows with no change to how spans are produced.
Compute can stay eager: tree-sitter parses are bounded by prewarm budgets plus the size
gate, so a bounded eager compute is fine.

The further optimization is **lazy per-visible-row compute**: parse/highlight a row only
when it first becomes visible, cache the result, and repaint. Keep `on_line` itself
synchronous and cache-only — if the span is missing, return and let an async parse fill
the cache, then redraw (the `decorate_row` → `cache_get` → `compute` → `cache_put`
pattern in `render/decoration.lua`). Never call tree-sitter inside `on_line`.

Crucially, diff-body syntax does **not** come from parsing the diff buffer. Tree-sitter
parses the *reconstructed source files* once per revision (`syntax_engine` +
`syntax_context`, hosted in hidden scratch buffers), and the diff-body highlight spans
are derived from those real source parses. The decoration cache is keyed
`file_key:revision:line`; `cache_put` drops other revisions so a file never paints
against stale syntax (`decoration.cache_put`, and `syntax_context` drops its tree when
the snapshot changes).

## Native folds: don't reimplement them

Collapse/expand uses **native manual folds**, never virtual rows. Reimplementing folding
with hidden/virtual lines re-introduces the text-virtualization problems above.

```lua
-- views/status/fold_state.lua: set options on the WINDOW, then build ranges.
vim.api.nvim_win_call(win, function()
  vim.cmd("normal! zE")                          -- clear existing folds
  pcall(vim.cmd, ("%d,%dfold"):format(range.start_line, range.end_line))
end)
```

Set `foldmethod = "manual"` and `foldenable = true` with `vim.wo[win]` (explicit window,
never `vim.wo[0]`) before creating folds. Save/restore the view around fold edits to
avoid scroll jumps. Folding is then instant and native — search/marks/undo still see the
real lines underneath. Fold pitfalls (state keyed by node id, re-applying fold level
after refresh) live in async-and-git.md / common-bugs.md; do not duplicate them here.

## Scratch & source buffers: hide, don't wipe

For reusable scratch buffers (a `nofile` preview, a per-revision Tree-sitter *source*
buffer you re-parse) set **`bufhidden = "hide"`**, not `"wipe"`. `"wipe"` deletes the
buffer the moment it is hidden, so anything that still references it (a cache, a pending
async callback) errors later. Delete them explicitly when you are truly done:

```lua
vim.bo[buf].bufhidden = "hide"   -- survives being hidden; reuse it
-- …later, on teardown:
if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
```

Key a source-buffer cache by full path (not basename — basenames collide, see
common-bugs.md #9) and validate `nvim_buf_is_valid` before reuse, since another path may
have wiped it.

**Putting a buffer in a window: `nvim_win_set_buf` vs `vim.cmd.edit`.** They are not
interchangeable. `nvim_win_set_buf(win, buf)` is low-level — it fires **no**
`BufRead`/`BufEnter`/`FileType` autocmds, so LSP, gitsigns, mini.diff, treesitter
filetype attach, etc. will **not** attach. Use it for a preview / scratch display where
you don't want plugins touching the buffer. `vim.cmd.edit(path)` fires all normal
autocmds — use it when "jump to the real file" should bring the editing plugins with it.
Picking the wrong one is a classic "why didn't gitsigns attach?" bug.

## Size gate: stop before you overshoot a row budget

Even with real lines, do not eagerly render tens of thousands of body rows into janky
syntax work on first open. The size gate (`views/status/size_gate.lua`) estimates each
hunk's rendered row count and **stops rendering before a hunk that would overshoot the
budget**, emitting a `load_more` row instead of the rest of the body.

```lua
-- size_gate._status_size_gate_should_defer: always render hunk 1 + forced hunks,
-- then stop once the budget is hit or the next hunk would overshoot it.
function M._status_size_gate_should_defer(rendered_rows, next_estimate, hunk_index, forced_hunks, budget)
  if not budget then return false end
  if hunk_index <= 1 or hunk_index <= forced_hunks then return false end
  return rendered_rows >= budget or (rendered_rows + (next_estimate or 0)) > budget
end
```

The budget comes from `status_diff_viewport_threshold` (default 1200 rows per file
body). Activating the `load_more` row renders the next chunk on demand (via
`hunk_index`'s chunked sections). Always render the first hunk and any force-loaded hunks
so a single giant hunk can never freeze the render and `load_more` always makes progress.

## Prewarm caps: cap cursor-driven tree-sitter fan-out

Cursor-driven prewarm (warming per-hunk syntax for the file under the cursor) MUST be
capped, or hovering a many-hunk file fans out into hundreds of scratch-buffer parses and
freezes the editor. Concretely: hovering `init.lua`'s own ~273-hunk diff fanned out to
~512 tree-sitter parses and froze the editor ~4.8s.

```lua
-- render/syntax_engine.lua
local hunk_budget = status_prewarm_hunk_budget(#hunks)  -- min(#hunks, max_hunks)
local warmed = 0
for _, hunk in ipairs(hunks) do
  if warmed >= hunk_budget then break end
  if hunk.diff and hunk.diff ~= "" then
    M.prewarm_diff_syntax(file.filename, hunk.diff, { hunk.staged }, key, on_update, opts)
    warmed = warmed + 1
  end
end
```

The cap is `status_cursor_prewarm_max_hunks` (default **12**; `0` disables file prewarm
entirely; `status_cursor_prewarm = false` disables cursor prewarm altogether). Small
files still warm every hunk; only large files are clamped. Off-screen rows get their
syntax lazily through the provider anyway, so the cap costs nothing visible.

## Highlight mechanics that bite

- **Background-only highlights.** Set `bg` without `fg` so syntax foreground survives:
  `nvim_set_hl(0, "DiffReviewAddBg", { bg = "#102010" })`. Do **not** `link` a background
  band to `DiffAdd` if that group carries an `fg` — the link's `fg` overrides syntax
  coloring on every changed line.
- **`hl_mode = "replace"` overlays win.** Inline virtual-text overlays (the gutter, any
  Snacks-style virt_text code) with `hl_mode = "replace"` completely OVERRIDE
  `line_hl_group` and `hl_group` extmarks underneath. To recolor such an overlay you must
  change the **virtual-text's own hl groups**, not add an extmark on the line — the
  extmark is invisible under the overlay.
- **Extmark presence ≠ visible highlight.** A span being in the buffer does not prove it
  won the cell. When a row "looks uncolored," inspect the rendered cell stack with
  `nvim__inspect_cell` (row/col) to see which hl group actually applied — markdown
  `@markup.*` / `Special` or a `replace` overlay may be on top. Judge by the cell, not by
  `nvim_buf_get_extmarks`.
- **Gutter-width math for cursor mapping.** The gutter is inline virt_text, so the
  cursor's screen column is offset from the source column by the gutter width (which
  varies per hunk with the max line-number width). Locate the code substring in the
  buffer line to recover the offset when mapping a cursor column back to a source column.

## Mixed-content markdown pitfall

Never register a whole synthetic filetype (e.g. `GitStatus`) as markdown just to render
one description subsection. Attaching the markdown tree-sitter parser to the entire
buffer paints `@markup.*` and `Special` captures over unrelated diff/code rows, which
override the language-derived syntax extmarks and make DiffReview's own highlights *look*
missing even though the extmarks are present (a fold toggle re-runs activation and flips
the row's color on/off). Keep markdown / otter / render-markdown activation **opt-in for
the real markdown region only** — `pr_edit` does this by sandboxing the markdown parser
to the description range so the title and reviewer rows are not re-styled. If a row looks
mysteriously decolored after a render-markdown change, suspect whole-buffer markdown
activation first and confirm with `nvim__inspect_cell`.
