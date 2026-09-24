vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local recap = require("forge.views.harness.recap")
local pending, errors, rendered = {}, {}, 0
client.host_generation = function() return 1 end
client.request_for = function(id, method, params, callback)
  assert(id == "session" and method == "harness.document" and params.operation == "recap")
  assert(params.model == "default", "recap must preserve auto/default selection instead of replaying a provider-resolved alias")
  pending[#pending + 1] = callback
end
notifications.error = function(message) errors[#errors + 1] = message end
local state = { session = { id = "session", model = "default", resolved_model = "provider-private-alias" }, busy = false, queue = {} }
local function render() rendered = rendered + 1 end
recap.request(state, render)
recap.request(state, render)
assert(#pending == 1 and state.recap.loading and not state.busy)
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
assert(state.recap == nil and errors[1] == "provider failed")
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
