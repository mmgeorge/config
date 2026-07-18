local M = {}

local row_emitter = require("diff_review.render.row_emitter")

local function same_lines(left, right)
  if #left ~= #right then return false end
  for index = 1, #left do
    if left[index] ~= right[index] then return false end
  end
  return true
end

local function block_operation_list(previous, next_blocks, reset, previous_line_count)
  if reset or not previous or #previous == 0 then
    local replacement = {}
    for _, block in ipairs(next_blocks or {}) do
      vim.list_extend(replacement, block.lines)
    end
    return { { first0 = 0, old_after0 = previous_line_count, replacement = replacement } }
  end
  local old_count, new_count = #previous, #next_blocks
  local common = {}
  for old_index = old_count + 1, 1, -1 do
    common[old_index] = {}
    for new_index = new_count + 1, 1, -1 do common[old_index][new_index] = 0 end
  end
  for old_index = old_count, 1, -1 do
    for new_index = new_count, 1, -1 do
      if previous[old_index].id == next_blocks[new_index].id then
        common[old_index][new_index] = common[old_index + 1][new_index + 1] + 1
      else
        common[old_index][new_index] = math.max(
          common[old_index + 1][new_index],
          common[old_index][new_index + 1]
        )
      end
    end
  end

  local operation_list = {}
  local old_index, new_index = 1, 1
  while old_index <= old_count or new_index <= new_count do
    local old_block = previous[old_index]
    local new_block = next_blocks[new_index]
    if old_block and new_block and old_block.id == new_block.id then
      if not same_lines(old_block.lines, new_block.lines) then
        operation_list[#operation_list + 1] = {
          first0 = old_block.first - 1,
          old_after0 = old_block.last,
          replacement = vim.deepcopy(new_block.lines),
        }
      end
      old_index = old_index + 1
      new_index = new_index + 1
    else
      local first_old_index, first_new_index = old_index, new_index
      while old_index <= old_count or new_index <= new_count do
        old_block = previous[old_index]
        new_block = next_blocks[new_index]
        if old_block and new_block and old_block.id == new_block.id then break end
        if new_index > new_count
          or (old_index <= old_count
            and common[old_index + 1][new_index] >= common[old_index][new_index + 1])
        then
          old_index = old_index + 1
        else
          new_index = new_index + 1
        end
      end
      local first0 = previous[first_old_index] and previous[first_old_index].first - 1
        or previous_line_count
      local old_after0 = old_index > first_old_index and previous[old_index - 1].last or first0
      local replacement = {}
      for index = first_new_index, new_index - 1 do
        vim.list_extend(replacement, next_blocks[index].lines)
      end
      operation_list[#operation_list + 1] = {
        first0 = first0,
        old_after0 = old_after0,
        replacement = replacement,
      }
    end
  end
  table.sort(operation_list, function(left, right) return left.first0 > right.first0 end)
  return operation_list
end

local function cursor_anchor(rows, cursor_row, cursor_col)
  local row = rows and rows[cursor_row]
  if not row then return nil end
  local node_id = row.node_id
  if not node_id then return nil end
  local first = cursor_row
  while first > 1 and rows[first - 1] and rows[first - 1].node_id == node_id do first = first - 1 end
  return { node_id = node_id, offset = cursor_row - first, col = cursor_col }
end

local function restore_cursor(win, rows, anchor, saved_view)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_win_call(win, function()
    local line_count = math.max(1, vim.api.nvim_buf_line_count(vim.api.nvim_win_get_buf(win)))
    if anchor then
      local first, last = nil, nil
      for line = 1, line_count do
        if rows[line] and rows[line].node_id == anchor.node_id then
          first = first or line
          last = line
        elseif first then
          break
        end
      end
      if first then
        local target = math.min(last, first + anchor.offset)
        pcall(vim.api.nvim_win_set_cursor, win, { target, math.max(0, anchor.col) })
        if saved_view then
          saved_view.lnum = target
          saved_view.topline = math.min(math.max(1, saved_view.topline), line_count)
          vim.fn.winrestview(saved_view)
        end
        return
      end
    end
    if saved_view then
      saved_view.lnum = math.min(math.max(1, saved_view.lnum), line_count)
      saved_view.topline = math.min(math.max(1, saved_view.topline), line_count)
      vim.fn.winrestview(saved_view)
    end
  end)
end

local function install_folds(win, fold_list)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    vim.cmd("silent! normal! zE")
    table.sort(fold_list, function(left, right)
      if left.first ~= right.first then return left.first < right.first end
      return left.last > right.last
    end)
    local line_count = math.max(1, vim.api.nvim_buf_line_count(vim.api.nvim_win_get_buf(win)))
    for _, fold in ipairs(fold_list) do
      local first = math.min(math.max(1, fold.first), line_count)
      local last = math.min(math.max(first, fold.last), line_count)
      if last > first then pcall(vim.cmd, ("%d,%dfold"):format(first, last)) end
    end
    for fold_index = #fold_list, 1, -1 do
      local fold = fold_list[fold_index]
      if fold.folded and fold.first <= line_count then
        pcall(vim.api.nvim_win_set_cursor, win, { math.max(1, fold.first), 0 })
        vim.cmd("silent! foldclose")
      end
    end
    vim.fn.winrestview(view)
  end)
end

local function fold_signature(fold_list)
  local part_list = {}
  for _, fold in ipairs(fold_list or {}) do
    part_list[#part_list + 1] = table.concat({
      fold.id or "",
      fold.first or 0,
      fold.last or 0,
      fold.folded and 1 or 0,
    }, ":")
  end
  return table.concat(part_list, "|")
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
  local transcript_visible = win
    and vim.api.nvim_win_is_valid(win)
    and vim.api.nvim_win_get_buf(win) == buf
  local previous_lines = state.render_initialized and state.rendered_lines
    or vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local saved_view, anchor = nil, nil
  if transcript_visible then
    vim.api.nvim_win_call(win, function()
      saved_view = vim.fn.winsaveview()
      local cursor = vim.api.nvim_win_get_cursor(win)
      anchor = cursor_anchor(state.render_rows, cursor[1], cursor[2])
    end)
  end
  local operation_list = block_operation_list(
    state.render_blocks,
    render.blocks or {},
    options.reset == true,
    #previous_lines
  )
  vim.api.nvim_buf_clear_namespace(buf, state.render_namespace, 0, -1)
  vim.bo[buf].modifiable = true
  for _, operation in ipairs(operation_list) do
    vim.api.nvim_buf_set_lines(
      buf,
      operation.first0,
      operation.old_after0,
      false,
      operation.replacement
    )
  end
  vim.bo[buf].modifiable = false

  local applied_line_list = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  if not same_lines(applied_line_list, render.lines) then
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, render.lines)
    vim.bo[buf].modifiable = false
    operation_list = { { first0 = 0, old_after0 = #applied_line_list, replacement = render.lines } }
  end

  for _, highlight in ipairs(render.highlights or {}) do
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
  for _, extmark in ipairs(render.extmarks or {}) do
    pcall(
      vim.api.nvim_buf_set_extmark,
      buf,
      state.render_namespace,
      extmark.line - 1,
      extmark.col or 0,
      extmark.options or {}
    )
  end
  for line, spans in pairs(render.diff_row_spans or {}) do
    row_emitter.emit(buf, state.render_namespace, line - 1, spans, false)
  end
  state.rendered_lines = vim.deepcopy(render.lines)
  state.render_blocks = vim.deepcopy(render.blocks or {})
  state.render_initialized = true
  state.render_rows = render.rows or {}
  local next_fold_signature = fold_signature(render.folds)
  if transcript_visible and (options.reset == true or state.render_fold_signature ~= next_fold_signature) then
    install_folds(win, render.folds or {})
    state.render_fold_signature = next_fold_signature
  elseif not transcript_visible then
    state.render_fold_signature = nil
  end
  if transcript_visible then
    if options.follow_tail then
      vim.api.nvim_win_call(win, function()
        local line_count = math.max(1, vim.api.nvim_buf_line_count(buf))
        vim.api.nvim_win_set_cursor(win, { line_count, 0 })
        vim.cmd("silent! normal! zb")
      end)
    else
      restore_cursor(win, render.rows or {}, anchor, saved_view)
    end
  end
  local replaced = 0
  local first0 = nil
  for _, operation in ipairs(operation_list) do
    replaced = replaced + math.max(operation.old_after0 - operation.first0, #operation.replacement)
    first0 = first0 and math.min(first0, operation.first0) or operation.first0
  end
  return { replaced = replaced, first0 = first0 or #render.lines, block_count = #operation_list }
end

return M
