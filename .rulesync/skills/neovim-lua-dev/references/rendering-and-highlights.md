# Rendering, Highlights, Virtual Text, and Folds for Large Buffers

This reference defines buffer management, decoration providers, extmark lifecycles, and syntax optimization strategies for rendering large datasets in Neovim plugins.

## Concrete Buffer Lines vs Viewport Text Virtualization

Do not virtualize buffer text by paging rows in and out of a small physical buffer. Virtualizing text breaks core editor capabilities:
- Native pattern search (`/`)
- Marks and jumps
- Independent split windows
- Native folding
- Text-object selection and line yanking

Write all logical rows into the buffer using `nvim_buf_set_lines`. Neovim comfortably manages large buffers (over 30,000 lines). Performance degradation in large buffers stems from eager syntax highlighting and extensive extmark allocation across off-screen lines, not raw buffer size.

### Viewport-Scoped Decoration Providers

Retain complete text in the buffer while scoping expensive per-row decorations (background colors, syntax highlights, and intraline highlights) to the visible viewport using `vim.api.nvim_set_decoration_provider`.

```lua
vim.api.nvim_set_decoration_provider(ns, {
  on_win = function(_, _, buf, toprow, botrow)
    if buf ~= target_buf then
      return false
    end
    return true
  end,
  on_line = function(_, _, buf, row)
    local spans = status.diff_row_spans and status.diff_row_spans[row + 1]
    if spans then
      status_emit_row_spans(buf, ns, row, spans, true)
    end
  end,
})
```

`on_line` executes only for visible rows during each screen redraw.

## Persistent vs Ephemeral Extmark Classification

Partition decorations by lifetime into separate namespaces:

- **Structural Layout (Persistent):** Headers, section labels, hunk boundaries, and line numbers (`virt_text`) belong in a persistent namespace. They survive until the next explicit redraw pass.
- **Diff Body Styling (Ephemeral):** Line backgrounds, Tree-sitter syntax spans, and word diff highlights are emitted within `on_line` with `ephemeral = true`. Neovim discards ephemeral marks automatically after redraw, preventing mark accumulation.

### Layout-Changing Virtual Text Invariant

`on_line` executes after line layout has been computed. Virtual text that modifies row geometry (such as `virt_text_pos = "inline"`) must remain a **persistent** extmark. Emitting inline virtual text ephemerally from `on_line` causes the virtual columns to be discarded without rendering.

## Syntax Parsing Architecture

Do not parse raw diff buffer text directly with Tree-sitter. Instead:
1. Reconstruct underlying source files in hidden scratch buffers per commit revision.
2. Run Tree-sitter parses against the reconstructed source buffers.
3. Map resulting syntax tokens back to the corresponding diff lines and cache spans by `file_key:revision:line`.

Keep `on_line` synchronous and cache-backed. If a syntax span is not yet cached, render fallback coloring and trigger an asynchronous background parse to update the cache.

## Native Folding Implementation

Implement folding using Neovim's native manual fold commands rather than virtual line filtering:

```lua
vim.api.nvim_win_call(win, function()
  vim.cmd("normal! zE")
  pcall(vim.cmd, ("%d,%dfold"):format(range.start_line, range.end_line))
end)
```

Configure `foldmethod = "manual"` and `foldenable = true` explicitly on the target window (`vim.wo[win]`).

## Scratch Buffer Lifecycle

For reusable scratch buffers (such as diff previews and syntax parsing hosts), configure `bufhidden = "hide"` rather than `"wipe"`:

```lua
vim.bo[buf].bufhidden = "hide"
```

Explicitly delete scratch buffers during plugin shutdown when `nvim_buf_is_valid(buf)` returns true.

### Buffer Display Functions

- **`nvim_win_set_buf(win, buf)`**: Low-level assignment that bypasses `BufRead`, `BufEnter`, and `FileType` autocommands. Use for scratch overlays and read-only previews.
- **`vim.cmd.edit(path)`**: Standard buffer navigation that fires full autocommands, attaching LSP clients, Tree-sitter grammars, and editor plugins.

## Row Budgets and Size Gating

To prevent UI lockup when loading massive diffs, enforce a row rendering budget. When the estimated row count exceeds the configured threshold, suspend full expansion and insert an interactive pagination placeholder:

```lua
function M._status_size_gate_should_defer(rendered_rows, next_estimate, hunk_index, forced_hunks, budget)
  if not budget then
    return false
  end
  if hunk_index <= 1 or hunk_index <= forced_hunks then
    return false
  end
  return rendered_rows >= budget or (rendered_rows + (next_estimate or 0)) > budget
end
```

Always render the initial hunk and any user-selected hunks to ensure consistent forward progress.

## Highlight Precedence and Inspection Rules

- **Background Highlights:** Specify `bg` without `fg` (`{ bg = "#102010" }`) so underlying syntax foreground colors remain visible.
- **Virtual Text Precedence:** Inline virtual text configured with `hl_mode = "replace"` overrides underlying `line_hl_group` and `hl_group` extmarks.
- **Highlight Inspection:** Use `nvim__inspect_cell(0, row, col)` to inspect the winning highlight group on a specific screen cell when debugging decoration conflicts.
- **Scoped Filetype Grammars:** Do not assign generic filetypes like `markdown` to entire composite buffers. Confine markdown parsers strictly to dedicated description regions to prevent syntax conflicts across code hunks.
