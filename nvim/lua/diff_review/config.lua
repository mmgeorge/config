---@class DiffReviewConfig
---@field status_buffer_name string
---@field pr_buffer_name string
---@field debug_notifications boolean
---@field pr_lookup_mode? "real"|"mock-delay"
---@field pr_mock_delay_ms? integer
---@field status_cursor_prewarm boolean
---@field about_auto_generate boolean
---@field about_auto_generate_delay_ms integer
---@field keymaps DiffReviewKeymapConfig

---@alias DiffReviewKeymap string|string[]|false

---@class DiffReviewStatusKeymapConfig
---@field close DiffReviewKeymap
---@field refresh DiffReviewKeymap
---@field toggle DiffReviewKeymap
---@field collapse_parent DiffReviewKeymap
---@field stage DiffReviewKeymap
---@field unstage DiffReviewKeymap
---@field discard DiffReviewKeymap
---@field open DiffReviewKeymap
---@field commit DiffReviewKeymap
---@field push DiffReviewKeymap
---@field pull DiffReviewKeymap
---@field pr DiffReviewKeymap
---@field browse DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewKeymapConfig
---@field status DiffReviewStatusKeymapConfig

---@class DiffReviewConfigModule
---@field defaults DiffReviewConfig
---@field options DiffReviewConfig
---@field setup fun(opts?: DiffReviewConfig): DiffReviewConfig

---@type DiffReviewConfigModule
local M = {}

---@type DiffReviewConfig
M.defaults = {
  status_buffer_name = "DiffReviewStatus",
  pr_buffer_name = "DiffReviewPR",
  debug_notifications = false,
  pr_lookup_mode = "real",
  pr_mock_delay_ms = 5000,
  status_cursor_prewarm = true,
  about_auto_generate = true,
  about_auto_generate_delay_ms = 1000,
  keymaps = {
    status = {
      close = "q",
      refresh = { "r", "or" },
      toggle = "<Tab>",
      collapse_parent = "N",
      stage = "S",
      unstage = "U",
      discard = "j",
      open = { "o", "<CR>" },
      commit = "cc",
      push = "opp",
      pull = "opP",
      pr = "ogp",
      browse = "b",
      help = "?",
    },
  },
}

---@type DiffReviewConfig
M.options = vim.deepcopy(M.defaults)

---@param opts? DiffReviewConfig
---@return DiffReviewConfig
function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
  return M.options
end

return M
