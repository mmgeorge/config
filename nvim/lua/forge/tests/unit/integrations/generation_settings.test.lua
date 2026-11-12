local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["commit settings select isolated generation without background policy"] = function()
  local config = require("forge.infra.config")
  config.options.background_generation = { mock = { model = "fresh-model", reasoning_effort = "low" } }
  config.options.commit_generation = {}
  local client = require("forge.client")
  local requests = {}
  client.generation_config = function(callback) callback({ data_root = "fixture" }) end
  client.request = function(request, callback)
    requests[#requests + 1] = request
    callback({ revision = 3, settings = {
      provider = "mock", models = { mock = { model = "commit-model", thinking = "low", service_tier = "fast" } },
      auto_compaction = { enabled = true, threshold_tokens = 128000 },
    } })
  end
  local commit = require("forge.integrations.commit_settings")
  local snapshot
  commit.request(nil, nil, function(result) snapshot = result end)
  eq(requests[1].params.purpose, "commit")
  eq(requests[1].params.defaults.models.mock.model, "fresh-model")
  local selection, display = commit.selection(snapshot)
  eq(selection.provider, "mock")
  eq(selection.model, "commit-model")
  eq(selection.auto_compaction, nil)
  eq(selection.service_tier, "fast")
  eq(display.service_tier, "fast")
  commit.request(snapshot, snapshot.settings, function() end)
  eq(requests[2].params.expected_revision, 3)
  eq(requests[2].params.purpose, "commit")
  require("forge.integrations.background_settings").request(nil, nil, function() end)
  eq(requests[3].params.purpose, "background")
end

return T
