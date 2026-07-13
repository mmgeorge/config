local M = {}

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessPlanQuestion")
local view_group = vim.api.nvim_create_augroup("DiffReviewHarnessQuestionView", { clear = false })

local function clamp_view(win, buf)
  if not (vim.api.nvim_win_is_valid(win) and vim.api.nvim_buf_is_valid(buf)) then return end
  local line_count = vim.api.nvim_buf_line_count(buf)
  local height = vim.api.nvim_win_get_height(win)
  local maximum_topline = math.max(1, line_count - height + 1)
  local view = vim.api.nvim_win_call(win, vim.fn.winsaveview)
  if view.topline <= maximum_topline then return end
  view.topline = maximum_topline
  vim.api.nvim_win_call(win, function() vim.fn.winrestview(view) end)
end

---@param transcript_win integer
---@param width integer
---@param height integer
---@param title string
---@return integer, integer
function M.open(transcript_win, width, height, title)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "DiffReviewPlanQuestion"
  local parent_width = vim.api.nvim_win_get_width(transcript_win)
  local parent_height = vim.api.nvim_win_get_height(transcript_win)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "win",
    win = transcript_win,
    row = math.max(0, math.floor((parent_height - height) / 2)),
    col = math.max(0, math.floor((parent_width - width) / 2)),
    width = width,
    height = height,
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " ",
    title_pos = "center",
    zindex = 80,
  })
  vim.wo[win].cursorline = false
  vim.wo[win].wrap = false
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].scrolloff = 0
  vim.wo[win].sidescrolloff = 0
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled" }, {
    group = view_group,
    buffer = buf,
    callback = function() clamp_view(win, buf) end,
    desc = "Keep Harness questions inside their rendered content",
  })
  return buf, win
end

---@param win integer
---@param transcript_win integer
---@param width integer
---@param height integer
function M.resize(win, transcript_win, width, height)
  local parent_width = vim.api.nvim_win_get_width(transcript_win)
  local parent_height = vim.api.nvim_win_get_height(transcript_win)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.row = math.max(0, math.floor((parent_height - height) / 2))
  window_config.col = math.max(0, math.floor((parent_width - width) / 2))
  window_config.width = width
  window_config.height = height
  vim.api.nvim_win_set_config(win, window_config)
end

---@param buf integer
---@param frame DiffReviewPlanQuestionFrame
---@param selected_index integer
---@param entry_list DiffReviewPlanQuestionEntry[]
function M.render(buf, frame, selected_index, entry_list)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, frame.lines)
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  for index, line in ipairs(frame.option_line) do
    local entry = entry_list[index]
    if entry and entry.key then
      vim.api.nvim_buf_add_highlight(
        buf,
        namespace,
        "DiffReviewHarnessQuestionKey",
        line - 1,
        2,
        2 + #entry.key
      )
    end
  end
  local selected_line = frame.option_line[selected_index]
  if selected_line then
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionSelected", selected_line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_question_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewQuestion", line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_answer_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewAnswer", line - 1, 0, -1)
  end
  for _, line in ipairs(frame.review_input_line) do
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewHarnessQuestionReviewInput", line - 1, 0, -1)
  end
  vim.api.nvim_buf_add_highlight(
    buf,
    namespace,
    "DiffReviewHarnessQuestionHint",
    frame.footer_line - 1,
    0,
    -1
  )
  vim.bo[buf].modifiable = false
  clamp_view(vim.fn.bufwinid(buf), buf)
end

---@param win integer
---@param hidden boolean
function M.set_cursor_hidden(win, hidden)
  vim.wo[win].winhighlight = hidden and "Cursor:DiffReviewHarnessHiddenCursor" or ""
end

---@param win integer
---@param focusable boolean
function M.set_focusable(win, focusable)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.focusable = focusable
  vim.api.nvim_win_set_config(win, window_config)
end

---@param win integer
---@param title string
function M.set_title(win, title)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.title = " " .. title .. " "
  vim.api.nvim_win_set_config(win, window_config)
end

return M
