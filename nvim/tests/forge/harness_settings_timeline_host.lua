vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local stage = "settings-timeline"
arg[2] = vim.fn.tempname()
local executable = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local data = assert(arg[2])
local workspace = vim.fs.joinpath(data, stage)
vim.fn.mkdir(workspace, "p")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
require("forge").setup({ harness_logging = true, harness = { backend = "mock",
  backends = { mock = { command = { "control-tools" }, selectable = true } } },
  background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function wait(predicate, message)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
require("forge").open_harness()
wait(function() return state.ready and state.session end, "Harness did not start")
local configured
require("forge.views.harness.controller").configure({ service_tier = "fast" }, true, function(applied)
  configured = applied
end)
wait(function() return configured ~= nil end, "Service tier validation did not finish")
assert(configured, "Service tier validation failed")
wait(function() return state.session.service_tier == "fast" end, "Service tier was not applied")
local snapshot
client.request("session.preview", { session_id = state.session.id }, function(result, failure)
  assert(not failure, failure)
  snapshot = result
end)
wait(function() return snapshot ~= nil end, "State did not refresh")
assert(type(snapshot.lines) == "table", "Session preview omitted rendered lines")
assert(not table.concat(snapshot.lines, "\n"):find("Settings completed", 1, true),
  "Completed settings appeared in the session preview")
local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
assert(not text:find("Settings completed", 1, true), "Completed settings appeared in the transcript")
local function request(method, params)
  local complete, value, failure
  client.request(method, params or {}, function(result, diagnostic)
    complete, value, failure = true, result, diagnostic
  end)
  wait(function() return complete end, method .. " did not complete")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
request("session.configure", { automatic_checks = false })
local function query(tool, arguments)
  local marker = vim.fn.nr2char(9679) .. " CONTROL "
  local before = 0
  for _, row in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if row:find(marker, 1, true) then before = before + 1 end
  end
  local input = vim.tbl_extend("force", arguments or {}, { tool = tool })
  request("prompt.submit", { text = "CONTROL " .. vim.json.encode(input) })
  local response
  wait(function()
    local rows = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
    local boundary, count = 0, 0
    for index, row in ipairs(rows) do
      if row:find(marker, 1, true) then boundary, count = index, count + 1 end
    end
    if count <= before then return false end
    for index = boundary + 1, #rows do
      local candidate = rows[index]:match("^%s*([%[{].*[%]}])%s*$")
      if candidate then response = candidate end
    end
    return response ~= nil
  end, tool .. " did not reach the rendered conversation")
  return vim.json.decode(response)
end
local config = query("harness_config").items[1]
assert(config.session_id == state.session.id)
assert(config.settings.session_tools == true)
assert(config.locations.forge_rust:find("/nvim/rust/forge", 1, true))
assert(config.locations.settings_database:find(data, 1, true))
local current = query("harness_session_status").items[1]
assert(current.session_id == state.session.id and current.approval_mode == "ask")
assert(current.observation == "authenticated_turn_start")
local names = query("__catalog")
local function contains(list, value)
  return vim.tbl_contains(list, value)
end
assert(contains(names, "harness_config") and contains(names, "harness_session_search"), vim.inspect(names))
local listed = query("harness_session_list").items
assert(#listed == 1 and listed[1].session_id == state.session.id)
local matches = query("harness_session_search", { query = "harness_config", session_id = state.session.id }).items
assert(#matches > 0)
local conversation = query("harness_session_exchange_list").items
assert(#conversation > 0)
local first = conversation[1]
assert(#first.messages >= 2, "native final answers were omitted from recall")
local detailed = query("harness_session_exchange", { exchange_id = first.exchange_id }).items
local tool
for _, item in ipairs(detailed) do if item.kind == "tool" then tool = item break end end
assert(tool, "exchange detail omitted its native tool occurrence")
local output = query("harness_session_tool", {
  exchange_id = first.exchange_id, tool_call_id = tool.tool_call_id, turn_id = tool.turn_id,
  fields = { "output", "input" },
}).items
assert(vim.iter(output):any(function(item) return item.field == "output" and item.text:find("items", 1, true) end))
assert(vim.iter(output):any(function(item) return item.field == "input" and item.unavailable end))
local log = query("harness_session_log")
assert(#log.items > 0 and not log.items[1].unavailable)
assert(query("harness_session_status", { session_id = "missing" }).error:find("does not exist", 1, true))
assert(query("harness_session_exchange", { exchange_id = "missing" }).error:find("does not belong", 1, true))
assert(query("harness_session_tool", { exchange_id = first.exchange_id, tool_call_id = "missing" }).error:find("missing or ambiguous", 1, true))

local picker = require("forge.views.picker")
require("forge.views.harness.settings").open(state, {
  window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win,
})
wait(function() return picker.is_open("harness-config") end, "configuration picker did not open")
local function history_field()
  local instance = picker._state_for_test()
  for index, option in ipairs(instance.spec.page_list[1].option_list) do
    for _, field in ipairs(option.fields or {}) do
      if field.id == "session-tools" then return instance, option, field, index end
    end
  end
  error("Session history field is missing")
end
local function toggle_history()
  local instance, option, field, index = history_field()
  instance.state.selected_index_by_page.config = index
  instance.state.field_by_page.config = { [option.id] = field.id }
  picker.update(instance.spec)
  local mapping = vim.api.nvim_buf_call(instance.buf, function() return vim.fn.maparg(" ", "n", false, true) end)
  assert(mapping.callback, "Session history has no Space mapping")
  mapping.callback()
end
toggle_history()
wait(function() local _, _, field = history_field() return state.session_tools == false and field.value == false end,
  "persisted off state did not reach the live picker")
local disabled = query("__catalog")
assert(contains(disabled, "harness_config"))
assert(not vim.iter(disabled):any(function(name) return name:find("^harness_session_") end))
assert(query("harness_session_list").error:find("disabled", 1, true))
assert(query("harness_config").items[1].settings.session_tools == false)
toggle_history()
wait(function() local _, _, field = history_field() return state.session_tools == true and field.value == true end,
  "persisted on state did not reach the live picker")
assert(contains(query("__catalog"), "harness_session_search"))
picker.close()

local original_access = vim.deepcopy(state.session.access)
local read_only = vim.deepcopy(original_access)
read_only.write_access = "off"
request("session.configure", { access = read_only })
assert(query("harness_session_status").items[1].access.write_access == "off")
request("session.configure", { access = original_access })

local large_prompt = "Large recall marker " .. string.rep("retained snow ", 12000)
request("prompt.submit", { text = large_prompt })
local pages = query("harness_session_exchange_list", { __collect_pages = true }).pages
assert(#pages > 1, "large exchange history did not use continuations")
local fragments, found_large = "", false
for _, page in ipairs(pages) do
  if page.fragment then
    fragments = fragments .. page.fragment
    if page.record_complete then
      local record = vim.json.decode(fragments)
      for _, message in ipairs(record.messages or {}) do
        if message.text == large_prompt then found_large = true end
      end
      fragments = ""
    end
  end
end
assert(found_large and fragments == "", "native paging lost the complete oversized question")
local expired = query("harness_session_exchange_list", { cursor = pages[1].next_cursor })
assert(expired.error:find("another interaction", 1, true), "expired inspection cursor was reused")
client.stop()
assert(vim.wait(5000, function() return not client._client.process end, 10), "Host did not stop")
print("harness_settings_timeline_host: validated speed, all eight inspection tools, live toggle and complete history paging")
vim.cmd("qa!")
