vim.loader.enable(false)
require("forge").setup()

local client = require("forge.client")
local picker = require("forge.views.picker")
local picker_state = require("forge.views.picker.state")
local checks = require("forge.views.harness.checks")
local session = require("forge.session")
local original = client.request_for
local catalog = {
  configured = true, revision = "fixture", overrides = { enabled = vim.empty_dict(), scopes = vim.empty_dict() }, history = {},
  snapshot = { config = { definitions = {
    { id = "unit", name = "Unit tests", area = "Core", trigger = {kind = "paths", include = {"src/**"}, exclude = {}}, category = "test", enabled = true, scope = "all", directory = ".", all = { command = "runner all" }, files = { command = "runner files" }, cases = { command = "runner cases" }, depends_on = {} },
    { id = "screenshots", name = "Screenshot tests", area = "UI", trigger = {kind = "paths", include = {"ui/**"}, exclude = {}}, category = "test", enabled = false, scope = "all", directory = ".", all = { command = "visual tests" }, depends_on = { "unit" } },
  } } },
  applicability = {
    {id = "unit", selected = true, reason = "Changed: src/core.rs", matched_files = {"src/core.rs"}},
    {id = "screenshots", selected = false, reason = "No matching task changes", matched_files = {}},
  },
}
local calls = {}
local load_failure
local output_path = string.rep("path/", 40) .. "result.log"
catalog.history = {{ id = "pending-run", state = "running", commands = {{ command = "runner all", context = { definition_id = "unit" }, state = "pending", started_at_ms = vim.NIL, completed_at_ms = vim.NIL, output = output_path }} }}
client.request_for = function(_, method, params, callback)
  calls[#calls + 1] = { method = method, params = params }
  if method == "checks.list" and load_failure then callback(nil, load_failure) return end
  if method == "checks.override" then catalog.overrides = params end
  if method == "checks.run" then callback({ run_id = "fixture-run" }) else callback(vim.deepcopy(catalog)) end
end
session.harness.session = { id = "checks-fixture", workspace = "fixture" }
local window = vim.api.nvim_get_current_win()
local host = { window_list = { window }, control_win = window }
local setup_id
checks.open(session.harness, host, function(id) setup_id = id or "all" end)

local function row(id)
  local current = picker._state_for_test()
  for _, option in ipairs(picker_state.page(current.state, current.spec).option_list) do
    if option.id == id then return option end
  end
  error("missing check row " .. id)
end
local function action(id, context)
  local current = picker._state_for_test()
  local operations = vim.list_extend(vim.deepcopy(current.spec.action_list),
    vim.deepcopy(picker_state.page(current.state, current.spec).action_list or {}))
  for _, operation in ipairs(operations) do
    if operation.id == id then operation.callback(context or {}) return end
  end
  error("missing picker action " .. id)
end
local function select(id)
  local current = picker._state_for_test()
  local page = picker_state.page(current.state, current.spec)
  for option_index, option in ipairs(page.option_list) do
    if option.id == id then
      current.state.selected_index_by_page[page.id] = option_index
      current.spec.on_change({ option = option })
      vim.wait(20, function() return false end)
      return
    end
  end
  error("missing check row " .. id)
end

assert(row("screenshots").columns[2] == "test", "screenshot suites must remain dynamic named test checks")
assert(row("unit").columns[7] == "Core", "functional area must be visible")
local current = picker._state_for_test()
current.spec.on_confirm({option = row("run-all")})
assert(calls[#calls - 1].method == "checks.run" and calls[#calls - 1].params.all == true, "full-suite execution must be explicit")
select("unit")
assert(picker_state.page(picker._state_for_test().state, picker._state_for_test().spec).subtitle:find("Selected: Changed: src/core.rs", 1, true), "selected checks must explain applicability")
action("next")
assert(catalog.overrides.enabled.unit == false, "arrow keys must change task state through the host")
assert(catalog.snapshot.config.definitions[1].enabled, "temporary toggles must preserve project defaults")
action("field")
action("next")
assert(catalog.overrides.scopes.unit == "changed_files", "scope must cycle inline using the shared field control")
action("details", { option = row("unit") })
assert(row("unit").child_line_list[2] == "All: runner all", "expanded rows must show concrete commands")
action("details", { option = row("screenshots") })
assert(row("screenshots").child_line_list[3] == "Prerequisites: unit", "details must expose dependencies")
local frame = require("forge.views.picker.layout").build(picker_state.page(picker._state_for_test().state, picker._state_for_test().spec), 1, 80)
assert(table.concat(frame.lines, "\n"):find("Run all enabled checks (ignore triggers)", 1, true), "action labels must use the available row width")
for _, range in pairs(frame.child_range) do
  for line = range.first, range.last do
    assert(vim.fn.strdisplaywidth(frame.lines[line]) <= 76, "long output paths must remain inside picker borders")
  end
end
assert(table.concat(frame.lines):gsub("%s", ""):find(output_path, 1, true), "wrapping must preserve the full output path")
action("edit")
assert(setup_id == "unit", "editing must identify the selected check")
current = picker._state_for_test()
select("setup")
local call_count = #calls
action("next")
assert(#calls == call_count, "value keys on an action row must not mutate the previously selected check")
local function key(value)
  local mapping = vim.fn.maparg(value, "n", false, true)
  assert(type(mapping.callback) == "function", "missing mapping " .. value)
  mapping.callback()
  vim.wait(20, function() return false end)
end
key("t")
current = picker._state_for_test()
assert(picker_state.selected_option(current.state, current.spec).id == "run", "case selection must not replace the configured next-option alias")
key("]")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "history", "history must be reachable while arrows edit values")
key("R")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "history", "refresh must preserve the user's history page")
key("<Left>")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "checks", "history arrows must restore page navigation instead of stale value actions")
picker.close(false)
catalog.configured, catalog.snapshot, catalog.error = false, nil, "checks_not_configured: use /checks setup"
checks.open(session.harness, host, function() end)
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).title:find("Unconfigured", 1, true))
assert(row("run").disabled_reason, "missing configuration must disable execution actions")
for _, option in ipairs(picker_state.page(current.state, current.spec).option_list) do
  assert(option.id ~= "error", "an uninitialized project must not report malformed configuration")
end
picker.close(false)
load_failure = "invalid .forge.json: expected a JSON object"
checks.open(session.harness, host, function() end)
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).title:find("Unavailable", 1, true), "load failures must leave a visible repair surface")
assert(row("error").label:find(load_failure, 1, true), "repair surface must retain the actual failure")
assert(row("run-all").disabled_reason, "unavailable configuration must not admit execution")
picker.close(false)
client.request_for = original
io.write("harness_checks OK\n")
vim.cmd("qa!")
