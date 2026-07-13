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
---@field walkthrough_inventory "sem"|false compute inventory with Sem or disable it
---@field branch_prefix string default prefix for branches created with `bc`; a repo's .diffreview.json may override it
---@field harness DiffReviewHarnessConfig
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

---@class DiffReviewHarnessKeymapConfig
---@field submit DiffReviewKeymap
---@field steer DiffReviewKeymap
---@field cancel DiffReviewKeymap
---@field edit_queued DiffReviewKeymap
---@field toggle_mode DiffReviewKeymap
---@field previous_prompt DiffReviewKeymap
---@field next_prompt DiffReviewKeymap
---@field history_previous DiffReviewKeymap
---@field history_next DiffReviewKeymap
---@field toggle_activity DiffReviewKeymap
---@field open_artifact DiffReviewKeymap
---@field reopen_question DiffReviewKeymap
---@field model DiffReviewKeymap
---@field effort_down DiffReviewKeymap
---@field effort_up DiffReviewKeymap
---@field close DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewPlanReviewKeymapConfig
---@field comment DiffReviewKeymap
---@field accept DiffReviewKeymap
---@field request_changes DiffReviewKeymap
---@field close DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewPlanQuestionKeymapConfig
---@field previous DiffReviewKeymap
---@field next DiffReviewKeymap
---@field select DiffReviewKeymap
---@field feedback DiffReviewKeymap
---@field question_previous DiffReviewKeymap
---@field question_next DiffReviewKeymap
---@field focus_input DiffReviewKeymap
---@field submit_input DiffReviewKeymap
---@field confirm DiffReviewKeymap
---@field revise DiffReviewKeymap
---@field close DiffReviewKeymap

---@class DiffReviewInteractionKeymapConfig
---@field toggle DiffReviewKeymap
---@field comment DiffReviewKeymap
---@field request_changes DiffReviewKeymap
---@field rollback DiffReviewKeymap
---@field refresh DiffReviewKeymap
---@field close DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewSessionKeymapConfig
---@field open DiffReviewKeymap
---@field tab_next DiffReviewKeymap
---@field fork DiffReviewKeymap
---@field rename DiffReviewKeymap
---@field delete DiffReviewKeymap
---@field refresh DiffReviewKeymap
---@field close DiffReviewKeymap
---@field help DiffReviewKeymap

---@class DiffReviewKeymapConfig
---@field status DiffReviewStatusKeymapConfig
---@field review DiffReviewReviewKeymapConfig
---@field harness DiffReviewHarnessKeymapConfig
---@field plan_review DiffReviewPlanReviewKeymapConfig
---@field plan_question DiffReviewPlanQuestionKeymapConfig
---@field interactions DiffReviewInteractionKeymapConfig
---@field sessions DiffReviewSessionKeymapConfig

---@class DiffReviewHarnessBackendConfig
---@field command string[]

---@class DiffReviewHarnessTrustProfile
---@field allow_workspace_write boolean
---@field allow_command boolean
---@field allow_network boolean
---@field allow_outside_workspace boolean
---@field allow_git_index boolean
---@field allow_git_history boolean
---@field allow_elevation boolean

---@class DiffReviewHarnessConfig
---@field backend "acp"|"codex"|"mock"
---@field model string
---@field effort string
---@field buffer_name string
---@field composer_name string
---@field interactions_buffer_name string
---@field sessions_buffer_name string
---@field composer_min_height integer
---@field composer_max_height integer
---@field goal_max_turns integer
---@field question_choice_keys string[]
---@field non_git_write_confirm boolean
---@field backends table<string, DiffReviewHarnessBackendConfig>
---@field trust_profile string
---@field trust_profiles table<string, DiffReviewHarnessTrustProfile>

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
  walkthrough_inventory = "sem",
  branch_prefix = "matt9222/",
  harness = {
    backend = "codex",
    model = "default",
    effort = "medium",
    buffer_name = "Harness",
    composer_name = "HarnessInput",
    interactions_buffer_name = "Interactions",
    sessions_buffer_name = "Sessions",
    composer_min_height = 3,
    composer_max_height = 12,
    goal_max_turns = 20,
    question_choice_keys = { "n", "e", "i", "l", "u", "y" },
    non_git_write_confirm = true,
    backends = {
      acp = { command = { "copilot", "--acp" } },
      codex = { command = { "codex", "app-server" } },
      mock = { command = { "mock" } },
    },
    trust_profile = "workspace",
    trust_profiles = {
      workspace = {
        allow_workspace_write = true,
        allow_command = true,
        allow_network = false,
        allow_outside_workspace = false,
        allow_git_index = false,
        allow_git_history = false,
        allow_elevation = false,
      },
    },
  },
  keymaps = {
    status = {
      close = "q",
      refresh = "R",
      reply = "R",
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
      reopen_question = "oe",
      model = "oM",
      effort_down = "<M-,>",
      effort_up = "<M-.>",
      close = "q",
      help = "?",
    },
    plan_review = {
      comment = "C",
      accept = "oY",
      request_changes = "oN",
      close = "q",
      help = "?",
    },
    plan_question = {
      previous = { "<Up>", "s" },
      next = { "<Down>", "t" },
      select = "<CR>",
      feedback = "<Tab>",
      question_previous = "<Left>",
      question_next = "<Right>",
      focus_input = "go",
      submit_input = "<C-s>",
      confirm = "y",
      revise = "n",
      close = "q",
    },
    interactions = {
      toggle = "<Tab>",
      comment = "C",
      request_changes = "oN",
      rollback = "R",
      refresh = "r",
      close = "q",
      help = "?",
    },
    sessions = {
      open = { "<CR>", "o" },
      tab_next = "<Tab>",
      fork = "F",
      rename = "rn",
      delete = "d",
      refresh = "r",
      close = "q",
      help = "?",
    },
  },
}

---@type DiffReviewConfig
M.options = vim.deepcopy(M.defaults)

---@param opts? DiffReviewConfig
---@return DiffReviewConfig
function M.setup(opts)
  local function merge(default, override)
    if override == nil then return vim.deepcopy(default) end
    if type(default) ~= "table" or type(override) ~= "table" then return vim.deepcopy(override) end
    if vim.islist(default) or vim.islist(override) then return vim.deepcopy(override) end
    local result = vim.deepcopy(default)
    for key, value in pairs(override) do result[key] = merge(default[key], value) end
    return result
  end
  local options = merge(M.defaults, opts or {})
  if options.walkthrough_inventory ~= "sem" and options.walkthrough_inventory ~= false then
    error('walkthrough_inventory must be "sem" or false')
  end
  if not options.harness.backends[options.harness.backend] then
    error("harness.backend must name a configured harness backend")
  end
  if not options.harness.trust_profiles[options.harness.trust_profile] then
    error("harness.trust_profile must name a configured trust profile")
  end
  local question_key_set = {}
  for _, key in ipairs(options.harness.question_choice_keys or {}) do
    if key == "a" or key == "o" then
      error(('harness.question_choice_keys cannot contain reserved key "%s"'):format(key))
    end
    if question_key_set[key] then error("harness.question_choice_keys must be unique") end
    question_key_set[key] = true
  end
  if vim.tbl_isempty(question_key_set) then error("harness.question_choice_keys cannot be empty") end
  M.options = options
  return M.options
end

return M
