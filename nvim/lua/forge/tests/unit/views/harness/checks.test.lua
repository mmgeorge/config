local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

local function open(config)
  local captured
  package.loaded["forge.views.picker"] = {
    is_open = function() return captured ~= nil end,
    open = function(spec) captured = spec end,
    update = function(spec) captured = spec end,
  }
  stub(require("forge.client"), "session_events_subscribe", function() return function() end end)
  mock_client.respond({ ["session.check.list"] = {
    configured = true, revision = "fixture", snapshot = { config = config },
    overrides = { enabled = {}, auto = {}, scopes = {} }, history = {},
  } })
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
  local details = current().page_list[1].option_list[2].child_line_list
  assert(details[2]:find("Adapter: ", 1, true) and details[2]:find("--stdin", 1, true), vim.inspect(details))
  notices:assert_none()
end

T["removed category is reported instead of rendering default validation"] = function()
  local notices = notify.capture({ notifications = true })
  local current = open({ compiler = { action = { category = "check", command = { "compiler" } } } })
  eq(current().page_list[1].option_list[1].id, "error")
  assert(table.concat(notices:messages(), "\n"):find("expected typecheck", 1, true))
end

return T
