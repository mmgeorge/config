-- Unit tests for `forge.views.harness.recap` (recap.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harness_recap"] = function()
  local client = require("forge.client")
  local recap = require("forge.views.harness.recap")
  local rendered = 0
  client.host_get_generation = function() return 1 end
  local pending = mock_client.respond({
    ["session.document"] = function(spec)
      local params = spec.params or {}
      assert(spec.session_id == "session" and params.operation == "recap")
      assert(params.model == "default", "recap must preserve auto/default selection instead of replaying a provider-resolved alias")
    end,
  })
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local state = { session = { id = "session", model = "default", resolved_model = "provider-private-alias" }, busy = false, queue = {} }
  local function render() rendered = rendered + 1 end
  recap.request(state, render)
  recap.request(state, render)
  assert(#pending == 1 and state.recap.loading and not require("forge.views.harness.activity").busy(state))
  pending[1].callback({ text = "We fixed status rendering." })
  assert(state.recap.text == "We fixed status rendering." and not state.recap.loading)
  state.queue[1] = "next prompt"
  assert(state.recap.text, "queue admission cleared recap before model dispatch")
  recap.request(state, render)
  recap.clear(state)
  pending[2].callback({ text = "stale recap" })
  assert(state.recap == nil, "new prompt allowed old recap to reappear")
  recap.request(state, render)
  pending[3].callback(nil, "provider failed")
  assert(state.recap == nil and errors[1].message == "Recap unavailable: provider failed")
  recap.request(state, render)
  pending[4].callback({ text = "" })
  assert(state.recap == nil and #errors == 2)
  recap.request(state, render)
  state.session = { id = "different" }
  pending[5].callback({ text = "wrong session" })
  assert(state.recap.text == nil, "result crossed session identity")
  assert(rendered > 5)
end

return T
