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

local popup_window = require("diff_review.infra.popup_window")

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

  local relative = opts.relative or "cursor"
  local buf, win = popup_window.open({
    relative = relative,
    width = width + 2,
    height = #lines,
    title = opts.title,
    filetype = "DiffReviewChoicePopup",
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  if relative == "editor" then
    local row = math.max(0, math.floor((vim.o.lines - #lines) / 2))
    local col = math.max(0, math.floor((vim.o.columns - width - 2) / 2))
    local window_config = vim.api.nvim_win_get_config(win)
    window_config.row = row
    window_config.col = col
    vim.api.nvim_win_set_config(win, window_config)
  end
  local chosen = false
  local function choose(value)
    if chosen then return end
    chosen = true
    popup_window.close(win)
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
