vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local state = session.harness
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local keymaps = require("forge.shared.keymaps")
local requests, notices, errors = {}, {}, {}
local header = ""
client.request = function(method, params, callback)
  assert(method == "session.configure", method)
  requests[#requests + 1] = { params = params, callback = callback }
end
notifications.info = function(message) notices[#notices + 1] = message end
notifications.error = function(message) errors[#errors + 1] = message end
keymaps.apply_view_winbar = function(_, _, _, _, segments)
  header = table.concat(vim.tbl_map(function(segment) return segment.text end, segments))
end
state.command_set = controller.command_set()
state.session = { id = "effort-test", backend = "codex", model = "test", resolved_model = "test", effort = "medium" }
state.capability = { effort_selection = true, fast_mode = true }
state.queue, state.pending_steer = {}, {}
local function shows(effort)
  assert(header:find(" " .. effort .. " •", 1, true), header)
end
local function settled()
  assert(vim.wait(1000, function() return not state.configuration_debounce end, 5), "selection timer did not settle")
end
local function acknowledge(index)
  local request = requests[index]
  request.callback(vim.tbl_extend("force", state.session, request.params))
end
local success, failure = xpcall(function()
  state.busy = true
  controller.change_effort(1)
  shows("high*")
  controller.change_effort(-1)
  shows("medium")
  assert(state.pending_config == nil, "returning to applied effort retained a request")
  controller.change_effort(-1)
  shows("low*")
  settled()
  assert(#requests == 0, "busy selection reached provider")
  state.busy = false
  controller.drain()
  assert(#requests == 1 and requests[1].params.effort == "low")
  shows("low*")
  acknowledge(1)
  shows("low")

  controller.change_effort(1)
  shows("medium*")
  controller.change_effort(1)
  shows("high*")
  controller.change_effort(-1)
  shows("medium*")
  controller.change_effort(-1)
  shows("low")
  settled()
  assert(#requests == 1, "cancelled idle selections were sent")

  controller.change_effort(1)
  settled()
  assert(#requests == 2 and requests[2].params.effort == "medium")
  shows("medium*")
  controller.change_effort(1)
  shows("high*")
  controller.change_effort(-1)
  shows("medium*")
  controller.change_effort(-1)
  shows("low")
  controller.change_effort(-1)
  shows("minimal*")
  settled()
  assert(#requests == 2, "concurrent configuration request")
  acknowledge(2)
  shows("minimal*")
  assert(vim.wait(1000, function() return #requests == 3 end, 5))
  assert(requests[3].params.effort == "minimal")
  acknowledge(3)
  shows("minimal")
  acknowledge(2)
  assert(state.session.effort == "minimal", "stale acknowledgement overwrote applied effort")

  state.busy = true
  for _, effort in ipairs({ "low*", "medium*", "high*", "xhigh*" }) do controller.change_effort(1) shows(effort) end
  for _, effort in ipairs({ "high*", "medium*", "low*", "minimal" }) do controller.change_effort(-1) shows(effort) end
  settled()
  assert(state.pending_config == nil and #requests == 3)
  state.busy = false
  state.status = { kind = "finalizing" }
  controller.change_effort(1)
  settled()
  assert(#requests == 3, "configuration sent during finalization")
  state.status = nil
  controller.drain()
  assert(#requests == 4)
  requests[4].callback(nil, "test rejection")
  shows("minimal")
  assert(#errors == 0 and #notices == 0, "effort selection emitted a notification")
  assert(header:find("Settings rejected: test rejection", 1, true), "rejection was not visible in the header")

  state.busy = true
  controller.configure_fast_mode(true)
  assert(header:find(" fast*", 1, true), header)
  require("forge.views.picker").open = function() error("fast toggle opened a picker") end
  controller.toggle_fast_mode()
  assert(not header:find("fast", 1, true) and not header:find("standard*", 1, true), header)
  assert(state.pending_config == nil, "cancelled fast change remained queued")
  controller.configure_fast_mode(true)
  controller.change_effort(1)
  assert(header:find("low* fast*", 1, true), header)
  settled()
  assert(#requests == 4)
  state.busy = false
  controller.drain()
  assert(#requests == 5 and requests[5].params.fast_mode == true and requests[5].params.effort == "low")
  controller.configure_fast_mode(false)
  assert(not header:find("fast", 1, true), header)
  settled()
  acknowledge(5)
  assert(header:find(" standard*", 1, true), header)
  assert(vim.wait(1000, function() return #requests == 6 end, 5))
  assert(requests[6].params.fast_mode == false, "false fast selection was discarded")
  acknowledge(6)
  assert(not header:find("fast", 1, true) and not header:find("standard*", 1, true), header)
  controller.configure_fast_mode(true)
  controller.configure_fast_mode(false)
  settled()
  assert(#requests == 6, "cancelled idle fast selection reached provider")
  assert(#notices == 0 and #errors == 0, "fast selection emitted a notification")
  state.busy = true
  state.composer_buf = vim.api.nvim_create_buf(false, true)
  require("forge.views.harness.prompt_history").record = function() end
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/fast" })
  controller.submit()
  assert(state.pending_config.fast_mode == true, "/fast did not toggle on")
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/fast" })
  controller.queue_submit()
  assert(state.pending_config == nil and #state.queue == 0, "queued /fast did not cancel pending toggle")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  settled()
  assert(#requests == 6 and #notices == 0 and #errors == 0)
  vim.api.nvim_buf_delete(state.composer_buf, { force = true })
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_effort_selection: passed")
vim.cmd("qa!")
