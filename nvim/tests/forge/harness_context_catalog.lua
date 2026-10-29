vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)

local client = require("forge.client")
local session = require("forge.session")
local catalog = require("forge.views.harness.provider_catalog")
local context_status = require("forge.views.harness.context_status")
local model_picker = require("forge.views.harness.model_picker")
local picker = require("forge.views.picker")

---@type ForgeHarnessModel[]
local source = {
  {
    id = "gpt-test",
    default_context_window = "default",
    context_window = {
      { id = "default", token_limit = 400000 },
      { id = "long_context" },
    },
  },
  {
    id = "other-model",
    default_context_window = "default",
    context_window = { { id = "default", token_limit = 128000 } },
  },
  { id = "auto", is_default = true, default_context_window = "default" },
}
local original_request = client.request
local request_count = 0
client.request = function(request_specification, callback)
  local method = request_specification.method

  assert(method == "session.provider.model.list", "context discovery requested an unrelated catalog")
  request_count = request_count + 1
  callback(vim.deepcopy(source), nil)
end
local state = session.harness
state.session = {
  id = "context-catalog",
  backend = "copilot",
  workspace = vim.fn.getcwd(),
  model = "gpt-test",
  context_usage = { used = 60000, size = 272000, remaining_percent = 78 },
}
state.model_list, state.model_backend = nil, nil
state.configuring_config, state.task_config = nil, nil
catalog.clear()

---@return ForgeHarnessModel[]
local function models()
  ---@type ForgeHarnessModel[]?
  local result
  catalog.models(false, function(value, failure)
    assert(not failure, failure)
    result = value
  end)
  assert(result, "model catalog did not resolve")
  return result
end

local delivered = models()
assert(delivered[1].context_window[1].token_limit == state.session.context_usage.size,
  "active picker capacity differs from the winbar's provider-reported limit")
assert(delivered[1].context_window[2].token_limit == nil, "live default capacity leaked into long context")
assert(delivered[2].context_window[1].token_limit == 128000, "live capacity leaked into another model")
assert(state.model_list[1].context_window[1].token_limit == 400000, "live capacity mutated the catalog cache")
assert(source[1].context_window[1].token_limit == 400000, "live capacity mutated provider metadata")

local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
model_picker.open({
  host = host,
  backend = "copilot",
  model_list = delivered,
  current_model = "gpt-test",
  on_confirm = function() end,
})
local picker_view = picker._state_for_test()
assert(vim.iter(picker_view.frame.lines):any(function(line)
  return line:find("gpt-test", 1, true) and line:find("272K", 1, true)
end), "rendered picker did not show the live capacity")
assert(context_status.segment(state.session.context_usage)[1].text == "78% left (272K)",
  "winbar did not show the same capacity as the picker")
picker.close(false)

state.session.context_usage.size = 264000
assert(models()[1].context_window[1].token_limit == 264000, "cache reuse ignored updated live capacity")
assert(request_count == 1, "usage changes unnecessarily reloaded the model catalog")

state.task_config = { model = "other-model", context_window = "default" }
delivered = models()
assert(delivered[1].context_window[1].token_limit == 264000, "pending selection hid the active model capacity")
assert(delivered[2].context_window[1].token_limit == 128000, "pending model inherited the active session budget")
state.task_config = { model = "gpt-test", context_window = "long_context" }
delivered = models()
assert(delivered[1].selected_context_window == "long_context", "pending context selection was lost")
assert(delivered[1].context_window[2].token_limit == nil, "pending tier inherited the active default budget")
state.task_config = nil

state.session.context_window = "long_context"
state.session.context_usage.size = 1000000
delivered = models()
assert(delivered[1].context_window[1].token_limit == 400000, "long-context budget replaced the default limit")
assert(delivered[1].context_window[2].token_limit == 1000000, "active long context did not use live usage")

state.session.model = "default"
state.session.resolved_model = "gpt-test"
assert(models()[1].context_window[2].token_limit == 1000000, "default model did not resolve to its runtime model")
assert(models()[3].context_window == nil, "resolved model usage leaked into the auto catalog entry")
state.session.resolved_model = nil
state.session.context_window = nil
delivered = models()
assert(delivered[3].context_window[1].id == "default"
    and delivered[3].context_window[1].token_limit == 1000000,
  "an active auto model without catalog limits did not acquire its live capacity")

state.session.model = "gpt-test"
for _, size in ipairs({ 0, -1 }) do
  state.session.context_usage.size = size
  assert(models()[1].context_window[1].token_limit == 400000, "invalid usage replaced the explicit model limit")
end
state.session.context_usage = nil
assert(models()[1].context_window[1].token_limit == 400000, "missing usage discarded the explicit model limit")

state.session.id = "another-session"
state.session.backend = "codex"
state.model_list, state.model_backend = nil, nil
delivered = models()
assert(request_count == 2, "switching sessions did not request an independent catalog")
assert(delivered[1].context_window[1].token_limit == 400000, "session switch retained another session's limit")

client.request = original_request
catalog.clear()
print("harness_context_catalog: passed")
vim.cmd("qa!")
