vim.loader.enable(false)
require("forge").setup()

local client = require("forge.client")
local picker = require("forge.views.picker")
local picker_state = require("forge.views.picker.state")
local checks = require("forge.views.harness.checks")
local session = require("forge.session")
local original = client.request_for
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
local errors = {}
notifications.error = function(message) errors[#errors + 1] = message end
local catalog = {
  configured = true, revision = "fixture", overrides = { enabled = { screenshots = false }, scopes = vim.empty_dict() }, history = {},
  snapshot = { config = {
    unit = { trigger = {auto = true, paths = {"src/**"}}, action = {label = "Unit tests", category = "test", directory = "Core", command = "runner all"} },
    screenshots = { trigger = {paths = {"ui/**"}}, action = {label = "Screenshot tests", category = "test", directory = "UI", command = "visual tests", after = {"unit"}} },
  } },
  applicability = {
    {id = "unit", selected = true, reason = "Changed: src/core.rs", matched_files = {"src/core.rs"}},
    {id = "screenshots", selected = false, reason = "No matching task changes", matched_files = {}},
  },
}
local calls = {}
local load_failure
local output_path = string.rep("path/", 40) .. "result.log"
catalog.history = {{ id = "pending-run", state = "running", context = {trigger = "Agent requested"}, lifecycle = {{kind = "started", text = "Checks started · Agent requested · 1 selected"}}, commands = {{ command = "runner all", context = { definition_id = "unit" }, state = "pending", started_at_ms = vim.NIL, completed_at_ms = vim.NIL, output = output_path }} }}
client.request_for = function(_, method, params, callback)
  calls[#calls + 1] = { method = method, params = params }
  if method == "checks.list" and load_failure then callback(nil, load_failure) return end
  if method == "checks.override" then catalog.overrides = params end
  if method == "checks.run" then callback({ run_id = "fixture-run" }) else callback(vim.deepcopy(catalog)) end
end
session.harness.session = { id = "checks-fixture", workspace = "fixture" }
local window = vim.api.nvim_get_current_win()
local host = { window_list = { window }, control_win = window }
checks.open(session.harness, host)

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

local function key(value)
  local mapping = vim.fn.maparg(value, "n", false, true)
  assert(type(mapping.callback) == "function", "missing mapping " .. value)
  mapping.callback()
  vim.wait(20, function() return false end)
end

assert(row("screenshots").columns[2] == "test", "screenshot suites must remain dynamic named test checks")
assert(row("unit").columns[4] == "Core", "functional area must be visible")
assert(row("unit").columns[1] == "● Unit tests", "enabled checks need a filled circle without numbering")
assert(row("screenshots").columns[1] == "○ Screenshot tests", "disabled checks need an empty circle")
local current = picker._state_for_test()
local page = picker_state.page(current.state, current.spec)
assert(vim.deep_equal(page.column_headers, { "Check", "Category", "Auto", "Directory" }))
assert(#page.option_list == 2, "the definitions page must only contain check rows")
for _, option in ipairs(page.option_list) do
  assert(option.value and option.value.category, "setup, run, reset, and save actions must be absent")
end
for _, operation in ipairs(page.action_list) do
  assert(operation.id ~= "edit" and operation.id ~= "files" and operation.id ~= "cases",
    "setup and scope selection must not have dedicated hotkeys")
end
select("unit")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).subtitle == nil, "selection details must not appear above the table")
local call_count = #calls
assert(current.spec.on_confirm({ option = row("unit") }) == false)
assert(#calls == call_count, "Enter must not run a check")
assert(not page.footer:find("↵ run", 1, true), "the footer must not advertise Enter to run")

key("<Space>")
assert(catalog.overrides.enabled.unit == false, "Space must toggle the selected check through the host")
assert(row("unit").columns[1] == "○ Unit tests")
assert(catalog.snapshot.config.unit.trigger.auto, "temporary toggles must preserve project defaults")
key("<Right>")
assert(catalog.overrides.auto.unit == false, "arrows must toggle automatic execution")
key("<Space>")
assert(catalog.overrides.enabled.unit and catalog.overrides.auto.unit == false,
  "Space must toggle enabled state independently of automatic execution")
key("<Left>")
assert(catalog.overrides.auto.unit == true, "Left must toggle automatic execution")

action("details", { option = row("unit") })
assert(row("unit").child_line_list[2] == "Action: runner all", "expanded rows must show concrete commands")
action("details", { option = row("screenshots") })
assert(row("screenshots").child_line_list[3] == "Prerequisites: unit", "details must expose dependencies")
local frame = require("forge.views.picker.layout").build(picker_state.page(picker._state_for_test().state, picker._state_for_test().spec), 1, 80)
assert(not table.concat(frame.lines, "\n"):find("Run all enabled checks", 1, true), "removed actions must not render")
for _, range in pairs(frame.child_range) do
  for line = range.first, range.last do
    assert(vim.fn.strdisplaywidth(frame.lines[line]) <= 76, "long output paths must remain inside picker borders")
  end
end
assert(table.concat(frame.lines):gsub("%s", ""):find(output_path, 1, true), "wrapping must preserve the full output path")

select("unit")
key("t")
current = picker._state_for_test()
assert(picker_state.selected_option(current.state, current.spec).id == "screenshots", "scope actions must preserve the next-option alias")
key("]")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "history", "history must be reachable while arrows edit values")
assert(picker_state.page(current.state, current.spec).option_list[1].label == "Checks",
  "history must not label checks by who requested them")
action("details", {option = picker_state.page(current.state, current.spec).option_list[1]})
current = picker._state_for_test()
assert(not table.concat(picker_state.page(current.state, current.spec).option_list[1].child_line_list, "\n"):find("Agent requested", 1, true),
  "history details must omit the requester-bearing start event")
key("R")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "history", "refresh must preserve the history page")
assert(vim.fn.maparg("<Space>", "n") == "", "the history page must not retain the enabled toggle")
key("<Left>")
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).id == "checks", "history arrows must restore page navigation")

local valid_config = vim.deepcopy(catalog.snapshot.config)
catalog.snapshot.config = { version = 2, definitions = {} }
action("refresh")
current = picker._state_for_test()
assert(row("error").label:find("schema mismatch", 1, true), "old host schemas must show a repair error")
assert(row("unit").columns[1] == "● Unit tests", "a failed refresh must retain the last valid rows")
local failure_count = #errors
action("refresh")
assert(#errors == failure_count, "unchanged schema errors must not repeat notifications")
select("unit")
call_count = #calls
action("toggle")
action("next")
current.spec.on_confirm({ option = row("unit") })
assert(#calls == call_count, "retained rows must not mutate an incompatible host")
catalog.snapshot.config = valid_config
action("refresh")
assert(#picker_state.page(picker._state_for_test().state, picker._state_for_test().spec).option_list == 2,
  "a valid response must remove the error row")

catalog.snapshot.config = {
  a_build = { action = { command = "exit 0", category = "build" } },
  b_test = { action = { command = "exit 0", category = "test" } },
  c_check = { action = { command = "exit 0", category = "check" } },
  d_format = { action = { command = "exit 0", category = "format" } },
}
action("refresh")
current = picker._state_for_test()
local ordered = picker_state.page(current.state, current.spec).option_list
assert(vim.deep_equal(vim.tbl_map(function(option) return option.id end, ordered),
  { "d_format", "c_check", "b_test", "a_build" }), "check rows must follow category priority, not names")
catalog.snapshot.config = valid_config
action("refresh")

picker.close(false)
for _, malformed in ipairs({
  { unit = { trigger = {} } },
  { unit = { action = { command = {} } } },
  { unit = { action = { command = "exit 0", inputs = "invalid" } } },
  { unit = { action = { command = "exit 0", after = { "missing" } } } },
}) do
  catalog.snapshot.config = malformed
  checks.open(session.harness, host)
  assert(row("error").label:find("Invalid check", 1, true), "malformed checks must not crash the callback")
  assert(#picker_state.page(picker._state_for_test().state, picker._state_for_test().spec).option_list == 1,
    "an invalid initial response must not publish partial check rows")
  picker.close(false)
end
catalog.configured, catalog.snapshot, catalog.error = false, nil, "checks_not_configured: enable /checks edit on and request configuration in chat"
checks.open(session.harness, host)
current = picker._state_for_test()
page = picker_state.page(current.state, current.spec)
assert(page.title:find("Unconfigured", 1, true))
assert(#page.option_list == 0, "missing configuration must leave no execution actions")
assert(page.empty_text:find("/checks edit on", 1, true))
picker.close(false)
load_failure = "invalid .forge.json: expected a JSON object"
checks.open(session.harness, host)
current = picker._state_for_test()
assert(picker_state.page(current.state, current.spec).title:find("Unavailable", 1, true))
assert(row("error").label:find(load_failure, 1, true), "repair surface must retain the actual failure")
local call_count = #calls
current.spec.on_confirm({ option = row("error") })
action("toggle")
action("next")
assert(#calls == call_count, "error rows must not run or change a previously selected check")
picker.close(false)
client.request_for = original
notifications.error = original_error
io.write("harness_checks OK\n")
vim.cmd("qa!")
