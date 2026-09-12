---@class ForgeConfig
---@field status_buffer_name string
---@field pr_buffer_name string
---@field debug_notifications boolean
---@field diff_logging boolean
---@field harness_logging boolean
---@field diff_log_path string?
---@field harness_log_path string?
---@field perf_slow_threshold_ms number
---@field perf_sample_rate number
---@field pr_lookup_mode? "real"|"mock-delay"
---@field pr_mock_delay_ms? integer
---@field status_cursor_prewarm boolean
---@field status_cursor_prewarm_max_hunks integer max hunks warmed per cursor-driven file prewarm; 0 disables file prewarm
---@field status_diff_viewport_enabled boolean
---@field status_diff_viewport_threshold integer
---@field status_diff_viewport_overscan integer
---@field status_file_preview_line_limit integer
---@field about_auto_generate boolean
---@field walkthrough_inventory "sem"|false compute inventory with Sem or disable it
---@field branch_prefix string default prefix for branches created with `bc`; a repo's .forge.json may override it
---@field harness ForgeHarnessConfig
---@field picker ForgePickerConfig
---@field keymaps ForgeKeymapConfig

--- Per-repository config read from `<repo root>/.forge.json`.
---@class ForgeRepoConfig
---@field branch_prefix? string

---@alias ForgeKeymap string|string[]|false

---@class ForgeStatusKeymapConfig
---@field close ForgeKeymap
---@field refresh ForgeKeymap
---@field toggle ForgeKeymap
---@field collapse_parent ForgeKeymap
---@field visual_line_with_gutter ForgeKeymap
---@field stage ForgeKeymap
---@field unstage ForgeKeymap
---@field ignore ForgeKeymap
---@field discard ForgeKeymap
---@field open ForgeKeymap
---@field commit ForgeKeymap
---@field push ForgeKeymap
---@field pull ForgeKeymap
---@field pr ForgeKeymap
---@field branch_create ForgeKeymap
---@field browse ForgeKeymap
---@field walkthrough ForgeKeymap
---@field review ForgeKeymap
---@field help ForgeKeymap

---@class ForgeReviewKeymapConfig
---@field viewed ForgeKeymap mark the file under the cursor as viewed
---@field unviewed ForgeKeymap move the file under the cursor back to unviewed
---@field comment ForgeKeymap add a comment on the selection/line, or edit the comment under the cursor
---@field delete ForgeKeymap delete the comment under the cursor
---@field next_comment ForgeKeymap jump to the next comment
---@field prev_comment ForgeKeymap jump to the previous comment
---@field sync ForgeKeymap sync dirty review comments to GitHub
---@field submit ForgeKeymap submit the review to GitHub

---@class ForgeHarnessKeymapConfig
---@field submit ForgeKeymap
---@field steer ForgeKeymap
---@field cancel ForgeKeymap
---@field edit_queued ForgeKeymap
---@field toggle_mode ForgeKeymap
---@field previous_prompt ForgeKeymap
---@field next_prompt ForgeKeymap
---@field history_previous ForgeKeymap
---@field history_next ForgeKeymap
---@field toggle_activity ForgeKeymap
---@field open_artifact ForgeKeymap
---@field agent ForgeKeymap
---@field sessions ForgeKeymap
---@field open_timeline ForgeKeymap
---@field reopen_question ForgeKeymap
---@field model ForgeKeymap
---@field effort_down ForgeKeymap
---@field effort_up ForgeKeymap
---@field close ForgeKeymap
---@field help ForgeKeymap

---@class ForgePlanReviewKeymapConfig
---@field open ForgeKeymap
---@field jump_entity ForgeKeymap
---@field entity_info ForgeKeymap
---@field rename_entity ForgeKeymap
---@field schema ForgeKeymap
---@field comment ForgeKeymap
---@field accept ForgeKeymap
---@field request_changes ForgeKeymap
---@field close ForgeKeymap
---@field help ForgeKeymap

---@class ForgePickerKeymapConfig
---@field previous ForgeKeymap
---@field next ForgeKeymap
---@field select ForgeKeymap
---@field feedback ForgeKeymap
---@field page_previous ForgeKeymap
---@field page_next ForgeKeymap
---@field focus_input ForgeKeymap
---@field submit_input ForgeKeymap
---@field close ForgeKeymap

---@class ForgeKeymapConfig
---@field status ForgeStatusKeymapConfig
---@field review ForgeReviewKeymapConfig
---@field harness ForgeHarnessKeymapConfig
---@field plan_review ForgePlanReviewKeymapConfig
---@field picker ForgePickerKeymapConfig

---@class ForgeHarnessBackendConfig
---@field command string[]
---@field label string
---@field detail string
---@field selectable? boolean

---@class ForgeHarnessConfig
---@field backend "codex"|"copilot"|"mock"
---@field model string
---@field effort string
---@field buffer_name string
---@field composer_name string
---@field composer_min_height integer
---@field composer_max_height integer
---@field goal_max_turns integer
---@field non_git_write_confirm boolean
---@field plan { scope_deviation_review: "auto"|"prompt" }
---@field backends table<string, ForgeHarnessBackendConfig>

---@class ForgePickerConfig
---@field choice_keys string[]
---@field session_keys string[]
---@field max_height integer
---@field input_height integer

---@class ForgeConfigModule
---@field defaults ForgeConfig
---@field options ForgeConfig
---@field setup fun(opts?: ForgeConfig): ForgeConfig
local M = {}

---@type ForgeConfig
M.defaults = {
  status_buffer_name = "ForgeStatus",
  pr_buffer_name = "ForgePR",
  debug_notifications = false,
  -- Gate non-Harness tracing (ForgeStatus, diffs, PRs, and shared UI work).
  diff_logging = false,
  -- Gate Harness lifecycle and provider-performance tracing.
  harness_logging = false,
  diff_log_path = nil,
  harness_log_path = nil,
  perf_slow_threshold_ms = 8,
  perf_sample_rate = 0,
  pr_lookup_mode = "real",
  pr_mock_delay_ms = 5000,
  status_cursor_prewarm = true,
  status_cursor_prewarm_max_hunks = 12,
  status_diff_viewport_enabled = false,
  status_diff_viewport_threshold = 1200,
  status_diff_viewport_overscan = 80,
  status_file_preview_line_limit = 1000,
  about_auto_generate = true,
  walkthrough_inventory = "sem",
  branch_prefix = "matt9222/",
  harness = {
    backend = "codex",
    model = "default",
    effort = "medium",
    buffer_name = "ForgeHarness",
    composer_name = "HarnessInput",
    composer_min_height = 3,
    composer_max_height = 12,
    goal_max_turns = 20,
    non_git_write_confirm = true,
    plan = {
      scope_deviation_review = "auto",
    },
    backends = {
      codex = {
        command = { "codex", "app-server" },
        label = "Codex CLI",
        detail = "OpenAI Codex app-server",
      },
      copilot = {
        command = {},
        label = "Copilot CLI",
        detail = "GitHub Copilot SDK",
      },
      mock = {
        command = { "mock" },
        label = "Mock",
        detail = "Harness test backend",
        selectable = false,
      },
    },
  },
  picker = {
    choice_keys = { "n", "e", "i", "l", "u", "y" },
    session_keys = { "n", "e", "a", "i", "l", "u", "o", "y" },
    max_height = 24,
    input_height = 3,
  },
  keymaps = {
    status = {
      close = "q",
      refresh = "R",
      reply = "R",
      toggle = "<Tab>",
      collapse_parent = "N",
      next_hunk = "]c",
      previous_hunk = "[c",
      visual_line_with_gutter = "W",
      stage = "S",
      unstage = "U",
      ignore = "I",
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
    harness = {
      submit = "<C-s>",
      steer = "<C-q>",
      cancel = "<C-c>",
      edit_queued = "<M-s>",
      toggle_mode = "<S-Tab>",
      previous_prompt = "<C-y>",
      next_prompt = "<C-z>",
      history_previous = "<Up>",
      history_next = "<Down>",
      toggle_activity = { "oa", "<Tab>" },
      open_artifact = "op",
      agent = "og",
      sessions = "os",
      open_timeline = { "<CR>", "." },
      reopen_question = "oe",
      model = "oM",
      effort_down = "<M-,>",
      effort_up = "<M-.>",
      close = "q",
      help = "?",
    },
    plan_review = {
      toggle = "<Tab>",
      open = "<CR>",
      jump_entity = ".",
      entity_info = "ol",
      rename_entity = "<Space>f",
      schema = "os",
      comment = "C",
      accept = "oY",
      request_changes = "oN",
      close = "q",
      help = "?",
    },
    picker = {
      previous = { "<Up>", "s" },
      next = { "<Down>", "t" },
      select = "<CR>",
      feedback = "<Tab>",
      page_previous = "<Left>",
      page_next = "<Right>",
      focus_input = "go",
      submit_input = "<C-s>",
      clear_input = "<C-c>",
      close = "q",
    },
  },
}

---@type ForgeConfig
M.options = vim.deepcopy(M.defaults)

---@param opts? ForgeConfig
---@return ForgeConfig
function M.setup(opts)
  local function merge(default, override)
    if override == nil then return vim.deepcopy(default) end
    if type(default) ~= "table" or type(override) ~= "table" then return vim.deepcopy(override) end
    if vim.islist(default) or vim.islist(override) then return vim.deepcopy(override) end
    local result = vim.deepcopy(default)
    for key, value in pairs(override) do result[key] = merge(default[key], value) end
    return result
  end
  local options = (opts == nil or vim.tbl_isempty(opts)) and vim.deepcopy(M.defaults) or merge(M.defaults, opts)
  if options.walkthrough_inventory ~= "sem" and options.walkthrough_inventory ~= false then
    error('walkthrough_inventory must be "sem" or false')
  end
  if not options.harness.backends[options.harness.backend] then
    error("harness.backend must name a configured harness backend")
  end
  local question_key_set = {}
  for _, key in ipairs(options.picker.choice_keys or {}) do
    if key == "a" or key == "o" then
      error(('picker.choice_keys cannot contain reserved key "%s"'):format(key))
    end
    if question_key_set[key] then error("picker.choice_keys must be unique") end
    question_key_set[key] = true
  end
  if vim.tbl_isempty(question_key_set) then error("picker.choice_keys cannot be empty") end
  M.options = options
  M.harness_backend_explicit = opts ~= nil and opts.harness ~= nil and opts.harness.backend ~= nil
  return M.options
end

return M
