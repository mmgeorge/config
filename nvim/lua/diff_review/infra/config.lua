---@class DiffReviewConfig
---@field status_buffer_name string
---@field pr_buffer_name string
---@field debug_notifications boolean
---@field perf_logging boolean
---@field diff_profile_log_path string?
---@field diff_profile_slow_threshold_ms number
---@field diff_profile_sample_rate number
---@field pr_lookup_mode? "real"|"mock-delay"
---@field pr_mock_delay_ms? integer
---@field status_cursor_prewarm boolean
---@field status_cursor_prewarm_max_hunks integer max hunks warmed per cursor-driven file prewarm; 0 disables file prewarm
---@field status_diff_viewport_enabled boolean
---@field status_diff_viewport_threshold integer
---@field status_diff_viewport_overscan integer
---@field about_auto_generate boolean
---@field about_auto_generate_delay_ms integer
---@field branch_prefix string default prefix for branches created with `bc`; a repo's .diffreview.json may override it
---@field keymaps DiffReviewKeymapConfig

--- Per-repository config read from `<repo root>/.diffreview.json`.
---@class DiffReviewRepoConfig
---@field branch_prefix? string

---@alias DiffReviewKeymap string|string[]|false

---@class DiffReviewStatusKeymapConfig
---@field close DiffReviewKeymap
---@field refresh DiffReviewKeymap
---@field toggle DiffReviewKeymap
---@field collapse_parent DiffReviewKeymap
---@field visual_line_with_gutter DiffReviewKeymap
---@field stage DiffReviewKeymap
---@field unstage DiffReviewKeymap
---@field discard DiffReviewKeymap
---@field open DiffReviewKeymap
---@field commit DiffReviewKeymap
---@field push DiffReviewKeymap
---@field pull DiffReviewKeymap
---@field pr DiffReviewKeymap
---@field branch_create DiffReviewKeymap
---@field browse DiffReviewKeymap
---@field walkthrough DiffReviewKeymap
---@field review DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewReviewKeymapConfig
---@field viewed DiffReviewKeymap mark the file under the cursor as viewed
---@field unviewed DiffReviewKeymap move the file under the cursor back to unviewed
---@field comment DiffReviewKeymap add a comment on the selection/line, or edit the comment under the cursor
---@field delete DiffReviewKeymap delete the comment under the cursor
---@field next_comment DiffReviewKeymap jump to the next comment
---@field prev_comment DiffReviewKeymap jump to the previous comment
---@field sync DiffReviewKeymap sync dirty review comments to GitHub
---@field submit DiffReviewKeymap submit the review to GitHub

---@class DiffReviewKeymapConfig
---@field status DiffReviewStatusKeymapConfig
---@field review DiffReviewReviewKeymapConfig

---@class DiffReviewConfigModule
---@field defaults DiffReviewConfig
---@field options DiffReviewConfig
---@field setup fun(opts?: DiffReviewConfig): DiffReviewConfig
local M = {}

---@type DiffReviewConfig
M.defaults = {
  status_buffer_name = "GitStatus",
  pr_buffer_name = "DiffReviewPR",
  debug_notifications = false,
  -- Master perf switch: gates both the structured profiler (infra.perf →
  -- diff-review-perf.log) and the GitStatus render timer (status_debug →
  -- gitstatus-debug.log). The diff_profile_* keys below only tune the profiler.
  perf_logging = true,
  diff_profile_log_path = nil,
  diff_profile_slow_threshold_ms = 8,
  diff_profile_sample_rate = 0,
  pr_lookup_mode = "real",
  pr_mock_delay_ms = 5000,
  status_cursor_prewarm = true,
  status_cursor_prewarm_max_hunks = 12,
  status_diff_viewport_enabled = false,
  status_diff_viewport_threshold = 1200,
  status_diff_viewport_overscan = 80,
  about_auto_generate = true,
  about_auto_generate_delay_ms = 1000,
  branch_prefix = "matt9222/",
  keymaps = {
    status = {
      close = "q",
      refresh = "R",
      toggle = "<Tab>",
      collapse_parent = "N",
      visual_line_with_gutter = "W",
      stage = "S",
      unstage = "U",
      discard = "j",
      open = { "o", "<CR>", "." },
      commit = "cc",
      push = "opp",
      pull = "opP",
      pr = "ogp",
      branch_create = "bc",
      browse = "b",
      walkthrough = "ow",
      review = "or",
      help = "?",
    },
    review = {
      viewed = "S",
      unviewed = "U",
      comment = "C",
      delete = "J",
      next_comment = "y",
      prev_comment = "z",
      sync = "<C-s>",
      submit = "cc",
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
