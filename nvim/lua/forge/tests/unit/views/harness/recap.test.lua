-- Unit tests for `forge.views.harness.recap` (recap.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "harness_recap", run = function()
vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local recap = require("forge.views.harness.recap")
local pending, errors, rendered = {}, {}, 0
client.host_get_generation = function() return 1 end
client.request = function(request_specification, callback)
  local id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  assert(id == "session" and method == "session.document" and params.operation == "recap")
  assert(params.model == "default", "recap must preserve auto/default selection instead of replaying a provider-resolved alias")
  pending[#pending + 1] = callback
end
notifications.error = function(message) errors[#errors + 1] = message end
local state = { session = { id = "session", model = "default", resolved_model = "provider-private-alias" }, busy = false, queue = {} }
local function render() rendered = rendered + 1 end
recap.request(state, render)
recap.request(state, render)
assert(#pending == 1 and state.recap.loading and not require("forge.views.harness.activity").busy(state))
pending[1]({ text = "We fixed status rendering." })
assert(state.recap.text == "We fixed status rendering." and not state.recap.loading)
state.queue[1] = "next prompt"
assert(state.recap.text, "queue admission cleared recap before model dispatch")
recap.request(state, render)
recap.clear(state)
pending[2]({ text = "stale recap" })
assert(state.recap == nil, "new prompt allowed old recap to reappear")
recap.request(state, render)
pending[3](nil, "provider failed")
assert(state.recap == nil and errors[1] == "Recap unavailable: provider failed")
recap.request(state, render)
pending[4]({ text = "" })
assert(state.recap == nil and #errors == 2)
recap.request(state, render)
state.session = { id = "different" }
pending[5]({ text = "wrong session" })
assert(state.recap.text == nil, "result crossed session identity")
assert(rendered > 5)
print("harness_recap: passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
