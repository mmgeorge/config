local M = {}

local row_emitter = require("diff_review.render.row_emitter")

local function semantic_key(row)
  if not row then return nil end
  local interaction = row.interaction or {}
  local thought = row.thought or {}
  local tool = row.tool or {}
  return table.concat({
    tostring(row.kind or ""),
    tostring(row.expand_key or ""),
    tostring(interaction.id or ""),
    tostring(thought.id or ""),
    tostring(tool.id or ""),
    tostring(row.file_path or ""),
    tostring(row.old_line or ""),
    tostring(row.new_line or ""),
  }, "\31")
end

local function changed_range(previous, next_lines, reset)
  if reset then return 0, #previous, #next_lines end
  local prefix = 0
  local common = math.min(#previous, #next_lines)
  while prefix < common and previous[prefix + 1] == next_lines[prefix + 1] do prefix = prefix + 1 end
  local suffix = 0
  while suffix < common - prefix
    and previous[#previous - suffix] == next_lines[#next_lines - suffix]
  do
    suffix = suffix + 1
  end
  return prefix, #previous - suffix, #next_lines - suffix
end

local function install_folds(win, fold_list, installed, reset)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    if reset then vim.cmd("silent! normal! zE") end
    table.sort(fold_list, function(left, right)
      if left.first ~= right.first then return left.first < right.first end
      return left.last > right.last
    end)
    local new_fold_list = {}
    for _, fold in ipairs(fold_list) do
      if not installed[fold.key] and fold.last > fold.first then
        pcall(vim.cmd, ("%d,%dfold"):format(fold.first, fold.last))
        installed[fold.key] = true
        new_fold_list[#new_fold_list + 1] = fold
      end
    end
    for fold_index = #new_fold_list, 1, -1 do
      local fold = new_fold_list[fold_index]
      if fold.folded then
        vim.api.nvim_win_set_cursor(win, { fold.first, 0 })
        vim.cmd("silent! foldclose")
      end
    end
    vim.fn.winrestview(view)
  end)
end

---@param state DiffReviewHarnessPresentationState
---@param render table
---@param options? { reset?: boolean, follow_tail?: boolean }
---@return table
function M.apply(state, render, options)
  options = options or {}
  local buf = state.transcript_buf
  local win = state.transcript_win
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return { replaced = 0 } end
  local previous = state.render_initialized
      and state.rendered_lines
      or vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local cursor_key = nil
  local saved_view = nil
  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_call(win, function()
      saved_view = vim.fn.winsaveview()
      local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
      cursor_key = semantic_key(state.render_rows and state.render_rows[cursor_row])
    end)
  end
  local first0, old_after0, new_after0 = changed_range(previous, render.lines, options.reset == true)
  if options.reset then
    vim.api.nvim_buf_clear_namespace(buf, state.render_namespace, 0, -1)
  elseif old_after0 ~= first0 or new_after0 ~= first0 then
    vim.api.nvim_buf_clear_namespace(buf, state.render_namespace, first0, old_after0)
  end
  if old_after0 ~= first0 or new_after0 ~= first0 then
    local replacement = {}
    for line_index = first0 + 1, new_after0 do
      replacement[#replacement + 1] = render.lines[line_index]
    end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, first0, old_after0, false, replacement)
    vim.bo[buf].modifiable = false
  end
  for _, highlight in ipairs(render.highlights or {}) do
    if options.reset or (highlight.line - 1 >= first0 and highlight.line - 1 < new_after0) then
      local first = highlight.first or 0
      local last = highlight.last or -1
      if last < 0 then last = #(render.lines[highlight.line] or "") end
      if last > first then
        vim.api.nvim_buf_set_extmark(buf, state.render_namespace, highlight.line - 1, first, {
          end_col = last,
          hl_group = highlight.group,
          priority = highlight.priority or 90,
        })
      end
    end
  end
  for _, extmark in ipairs(render.extmarks or {}) do
    if options.reset or (extmark.line - 1 >= first0 and extmark.line - 1 < new_after0) then
      pcall(
        vim.api.nvim_buf_set_extmark,
        buf,
        state.render_namespace,
        extmark.line - 1,
        extmark.col or 0,
        extmark.options or {}
      )
    end
  end
  for line, spans in pairs(render.diff_row_spans or {}) do
    if options.reset or (line - 1 >= first0 and line - 1 < new_after0) then
      row_emitter.emit(buf, state.render_namespace, line - 1, spans, false)
    end
  end
  state.rendered_lines = vim.deepcopy(render.lines)
  state.render_initialized = true
  state.render_rows = render.rows or {}
  if options.reset then state.fold_installed = {} end
  install_folds(win, render.folds or {}, state.fold_installed, options.reset == true)
  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_call(win, function()
      if options.follow_tail then
        vim.api.nvim_win_set_cursor(win, { math.max(1, #render.lines), 0 })
        vim.cmd("silent! normal! zb")
        return
      end
      if cursor_key then
        for line, row in pairs(render.rows or {}) do
          if semantic_key(row) == cursor_key then
            vim.api.nvim_win_set_cursor(win, { line, 0 })
            if saved_view then
              saved_view.lnum = line
              vim.fn.winrestview(saved_view)
            end
            return
          end
        end
      end
      if saved_view then vim.fn.winrestview(saved_view) end
    end)
  end
  return { replaced = math.max(old_after0 - first0, new_after0 - first0), first0 = first0 }
end

return M
