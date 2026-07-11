--- Provides a keyboard-driven popup for small, explicit choices.

---@class DiffReviewChoicePopupOption
---@field key string
---@field value any
---@field label string
---@field desc? string

---@class DiffReviewChoicePopupOptions
---@field title string
---@field options DiffReviewChoicePopupOption[]
---@field on_choice fun(value: any?)
---@field relative? "cursor"|"editor"
---@field cancel_label? string
---@field cancel_keys? string[]
---@field min_width? integer

local M = {}

--- Open a choice popup and resolve one option or nil when cancelled.
---@param opts DiffReviewChoicePopupOptions
---@return integer buf
function M.open(opts)
  local lines = { "" }
  for _, option in ipairs(opts.options or {}) do
    lines[#lines + 1] = ("  [%s]  %s"):format(option.key, option.label)
  end
  lines[#lines + 1] = ("  [q]  %s"):format(opts.cancel_label or "cancel")
  lines[#lines + 1] = ""

  local width = tonumber(opts.min_width) or 0
  for _, line in ipairs(lines) do
    width = math.max(width, vim.fn.strdisplaywidth(line))
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local relative = opts.relative or "cursor"
  local window_opts = {
    relative = relative,
    width = width + 2,
    height = #lines,
    style = "minimal",
    border = "rounded",
    title = (" %s "):format(opts.title),
    title_pos = "center",
  }
  if relative == "editor" then
    window_opts.row = math.max(0, math.floor((vim.o.lines - #lines) / 2))
    window_opts.col = math.max(0, math.floor((vim.o.columns - width - 2) / 2))
  else
    window_opts.row = 1
    window_opts.col = 0
  end

  local win = vim.api.nvim_open_win(buf, true, window_opts)
  local chosen = false
  local function choose(value)
    if chosen then return end
    chosen = true
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
    opts.on_choice(value)
  end

  for _, candidate in ipairs(opts.options or {}) do
    local option = candidate
    vim.keymap.set("n", option.key, function() choose(option.value) end, {
      buffer = buf,
      nowait = true,
      silent = true,
      desc = option.desc or option.label,
    })
  end
  for _, key in ipairs(opts.cancel_keys or { "q", "<Esc>" }) do
    vim.keymap.set("n", key, function() choose(nil) end, {
      buffer = buf,
      nowait = true,
      silent = true,
      desc = "Cancel " .. opts.title:lower(),
    })
  end
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    once = true,
    callback = function() choose(nil) end,
  })
  return buf
end

return M
