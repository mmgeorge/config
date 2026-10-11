vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local client = require("forge.client")
local pins = require("forge.views.harness.model_pins")
local model_picker = require("forge.views.harness.model_picker")
local picker = require("forge.views.picker")
local picker_state = require("forge.views.picker.state")
local errors, request_list, confirmed = {}, {}, nil
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
client.request = function(method, parameter, callback)
  request_list[#request_list + 1] = { method = method, parameter = parameter, callback = callback }
end
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback, "missing mapping " .. key)
  mapping.callback()
end
local function selected()
  local instance = picker._state_for_test()
  return picker_state.selected_option(instance.state, instance.spec)
end
local source = {
  { id = "older", reasoning = { "low", "high" }, default_reasoning = "low" },
  { id = "newer", reasoning = { "low", "high" }, default_reasoning = "low" },
  { id = "third" },
}
local function open(current, is_current)
  return model_picker.open({ host = { win = vim.api.nvim_get_current_win() }, model_list = source,
    backend = "codex", current_model = current, is_current = is_current,
    on_confirm = function(config) confirmed = config end })
end
local function first()
  return picker._state_for_test().spec.page_list[1].option_list[1]
end

local success, failure = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  pins.refresh("codex", function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({})
  open("newer")
  assert(selected().id == "newer" and first().id == "older")
  invoke("<Right>")
  assert(selected().value.picker_reasoning == "high")
  invoke("p")
  local write = request_list[#request_list]
  assert(write.method == "backend.model_pin" and write.parameter.model == "newer" and write.parameter.pinned)
  local count = #request_list
  invoke("p")
  assert(#request_list == count, "pin writes overlapped")
  assert(first().id == "older", "unacknowledged pin changed ordering")
  write.callback({ "newer" })
  assert(first().id == "newer" and first().label == "* newer")
  assert(selected().id == "newer" and selected().value.picker_reasoning == "high")
  assert(confirmed == nil, "pinning configured the model")
  invoke("p")
  write = request_list[#request_list]
  write.callback(nil, "database failure")
  assert(#errors == 1 and errors[1]:find("database failure", 1, true))
  assert(first().id == "newer" and pins.get("codex").newer)
  invoke("p")
  write = request_list[#request_list]
  write.callback({})
  assert(first().id == "older" and selected().id == "newer", "unpin lost the original order or selection")
  invoke("p")
  write = request_list[#request_list]
  picker.close()
  open("third")
  write.callback({ "newer" })
  assert(first().id == "older" and selected().id == "third", "late response updated a replacement picker")
  picker.close()
  local current = true
  open("third", function() return current end)
  invoke("p")
  write = request_list[#request_list]
  current = false
  write.callback({ "newer", "third" })
  assert(first().id == "newer", "backend-stale response changed the picker")
  assert(pins.get("codex").third, "successful stale write was not retained")
  picker.close()

  pins.refresh("codex", function(_, pin_error) assert(not pin_error) end)
  local read = request_list[#request_list]
  pins.set("codex", "older", true, function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({ "older" })
  read.callback({ "third" })
  assert(pins.get("codex").older and not pins.get("codex").third, "late read replaced a newer write")

  local state = require("forge.session").harness
  state.session = { id = "pin-completion", backend = "codex" }
  state.model_backend, state.model_list = "codex", source
  local completion = require("forge.views.harness.completion.command_source").new()
  local result
  completion:request_model_list(function(model_list) result = model_list end)
  assert(result[1].id == "older")
  pins.set("codex", "third", true, function(_, pin_error) assert(not pin_error) end)
  request_list[#request_list].callback({ "third" })
  completion:request_model_list(function(model_list) result = model_list end)
  assert(result[1].id == "third" and state.model_list[1].id == "older", "completion changed the raw catalog")
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { "/model " })
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { 1, 7 })
  completion:get_completions({}, function(value) result = value.items end)
  assert(result[1].label == "third" and result[1].sortText == "000001", "model completion lost pin ordering")
end, debug.traceback)
picker.close()
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_model_pins: passed")
vim.cmd("qa!")
