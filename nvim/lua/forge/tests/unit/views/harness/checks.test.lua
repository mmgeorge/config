local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

local function open(config, extra)
  local captured
  package.loaded["forge.views.picker"] = {
    is_open = function() return captured ~= nil end,
    open = function(spec) captured = spec end,
    update = function(spec) captured = spec end,
  }
  stub(require("forge.client"), "session_events_subscribe", function() return function() end end)
  mock_client.respond({ ["session.check.list"] = vim.tbl_extend("force", {
    configured = true, revision = "fixture", snapshot = { config = config },
    overrides = { enabled = {}, auto = {}, scopes = {} }, history = {},
  }, extra or {}) })
  require("forge.views.harness.checks").open({ session = { id = "checks" } }, {})
  return function() return captured end
end

T["adapter and custom categories retain their details and execution order"] = function()
  local notices = notify.capture({ notifications = true })
  local current = open({
    custom = { action = { category = "custom", command = { "rulesync", "generate" } } },
    lua = { action = { category = "test", adapter = { script = { command = { "nu", "--stdin", "tests.nu" } } } } },
    compiler = { action = { category = "typecheck", command = { "compiler" } } },
  })
  local rows = current().page_list[1].option_list
  eq(vim.tbl_map(function(row) return row.id end, rows), { "compiler", "lua", "custom" })
  current().action_list[1].callback({ option = rows[2] })
  local details = current().page_list[1].option_list[2].child_field_list
  eq(details, {
    { label = "adapter", value = "script" },
    { label = "  command", value = "nu --stdin tests.nu" },
  })
  notices:assert_none()
end

T["details retain nextest configuration and omit execution information"] = function()
  local current = open({ rust = {
    action = { category = "test", directory = "rust", adapter = { rust_nextest = {
      cargo_profile = "dev", locked = false, features = { "feature_one", "feature_two" },
      cargo_args = {}, all_features = false, nextest_profile = "default",
    } } },
    trigger = { auto = true, paths = { "rust/**" }, exclude = { "**/target/**", "**/*.md" } },
  } }, {
    applicability = { { id = "rust", selected = false, reason = "no changes" } },
    preview = { { id = "rust", selected = false, command = "cargo nextest", reason = "no changes" } },
    coverage = { { id = "rust", evidence = { state = "passed", run_id = "old" } } },
    history = { { id = "old", commands = { { context = { definition_id = "rust", selection = { kind = "all" } },
      state = "failed", command = "cargo nextest", error = "old failure", output = "old.output" } } } },
  })
  current().action_list[1].callback({ option = current().page_list[1].option_list[1] })
  local row = current().page_list[1].option_list[1]
  eq(row.child_field_list, {
    { label = "adapter", value = "rust_nextest" },
    { label = "  cargo_profile", value = "dev" },
    { label = "  features", value = "feature_one" },
    { label = "", value = "feature_two" },
    { label = "  locked", value = "false" },
    { label = "paths", value = "rust/**" },
    { label = "exclude", value = "**/target/**" },
    { label = "", value = "**/*.md" },
  })
  eq(row.child_line_list, nil)
  current().action_list[1].callback({ option = row })
  eq(current().page_list[1].option_list[1].child_field_list, nil)
end

T["script details preserve arguments prerequisites and trigger lists"] = function()
  local current = open({
    build = { action = { category = "build", adapter = { script = { command = { "build" } } } } },
    lua = { action = { category = "test", after = { "build" }, adapter = { script = {
      command = { "nu", "--stdin", "checks with spaces.nu", "", 'quoted"argument' },
    } } }, trigger = { paths = { "lua/**", "tests/**" }, exclude = { "tests/fixtures/**" } } },
  })
  current().action_list[1].callback({ option = current().page_list[1].option_list[2] })
  eq(current().page_list[1].option_list[2].child_field_list, {
    { label = "adapter", value = "script" },
    { label = "  command", value = 'nu --stdin "checks with spaces.nu" "" "quoted\\"argument"' },
    { label = "after", value = "build" },
    { label = "paths", value = "lua/**" },
    { label = "", value = "tests/**" },
    { label = "exclude", value = "tests/fixtures/**" },
  })
end

T["custom details retain declared inputs and explicit false values"] = function()
  local current = open({ custom = { action = {
    category = "custom", command = "run custom", prepare = false,
    inputs = { verbose = { type = "boolean", default = false, required = false, description = "Verbose output" } },
  } } })
  current().action_list[1].callback({ option = current().page_list[1].option_list[1] })
  eq(current().page_list[1].option_list[1].child_field_list, {
    { label = "command", value = "run custom" },
    { label = "prepare", value = "false" },
    { label = "inputs", value = "" },
    { label = "  verbose", value = "" },
    { label = "    default", value = "false" },
    { label = "    description", value = "Verbose output" },
    { label = "    required", value = "false" },
    { label = "    type", value = "boolean" },
  })
end

T["removed category is reported instead of rendering default validation"] = function()
  local notices = notify.capture({ notifications = true })
  local current = open({ compiler = { action = { category = "check", command = { "compiler" } } } })
  eq(current().page_list[1].option_list[1].id, "error")
  assert(table.concat(notices:messages(), "\n"):find("expected typecheck", 1, true))
end

return T
