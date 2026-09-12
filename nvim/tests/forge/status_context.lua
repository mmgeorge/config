vim.loader.enable(false)
local client = require("forge.client")
local ai = require("forge.integrations.ai_commit")
local gh = require("forge.integrations.gh")
local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
local errors = {}
notifications.error = function(message) errors[#errors + 1] = message end
local original_auto = config.options.about_auto_generate
local original_lookup, original_lookup_delay = config.options.pr_lookup_mode, config.options.pr_mock_delay_ms
assert(config.options.about_auto_generate ~= false, "automatic About must be enabled by default")
local original_request, original_ai, original_pr = client.request_host, ai.ensure, gh.current_pr_async
local ok, failure = xpcall(function()
  local updates, generation, pull_request, presentation = {}, {}, {}, {}
  client.request_host = function(method, params, callback)
    assert(method == "status.context")
    updates[#updates + 1] = { params = params, callback = callback }
  end
  ai.ensure = function(_, options, callback)
    assert(options.ignored_paths[1] == "excluded.txt", "About omitted Status Ignore selection")
    generation[#generation + 1] = callback
  end
  gh.current_pr_async = function(_, callback) pull_request[#pull_request + 1] = callback end
  local applied, opened, current = 0, nil, true
  local owner = require("forge.views.status.status_context").attach({ document_id = "fixture", workspace = "fixture", window = vim.api.nvim_get_current_win(),
    capture_input = function(target) return { document = "fixture", target = target, revision = 1, sequence = 1, view = "view" } end,
    is_input_current = function() return current end,
    is_alive = function() return true end,
    get_info = function() return { workspace = "fixture" } end,
    ignored_paths = function() return { "excluded.txt" } end,
    present = function(value) applied = applied + 1; presentation[#presentation + 1] = value end,
    open_commit = function(action) opened = action.oid end })
  owner.refresh({ info_loaded = true })
  assert(#generation == 1, "default Status delayed generation behind a timer or PR response")
  owner.refresh({ info_loaded = true })
  assert(#updates == 0 and applied == 2, "presentation made a host round trip")
  assert(#generation == 1, "status refresh restarted the draft")
  pull_request[1]({ ok = true, pr = { title = "stale PR" } })
  assert(owner.pr == nil, "stale PR callback was adopted")
  generation[1]({ state = "ready", message = "current message" })
  pull_request[2]({ ok = true, pr = { title = "current PR" } })
  assert(#updates == 0 and applied == 4)
  assert(presentation[#presentation].about.text == "current message")
  assert(presentation[#presentation].pr.text == "current PR")
  owner.activate("status:context:head")
  updates[1].callback({ kind = "commit", oid = string.rep("a", 40) })
  assert(opened == string.rep("a", 40))
  owner.activate("status:context:head")
  current = false
  updates[2].callback({ kind = "commit", oid = string.rep("b", 40) })
  assert(opened == string.rep("a", 40), "stale action response opened another commit")
  config.options.about_auto_generate = false
  local generation_count = #generation
  owner.refresh({ info_loaded = true })
  assert(#generation == generation_count, "disabled automatic About started generation")
  owner.generate_about(true)
  assert(#generation == generation_count + 1, "explicit About did not generate when automatic mode was disabled")
  config.options.pr_lookup_mode, config.options.pr_mock_delay_ms = "mock-delay", 1000
  local lookup_count = #pull_request
  owner.refresh({ info_loaded = true })
  assert(#pull_request == lookup_count and owner.pr_timer, "mock PR lookup contacted GitHub")
  owner.generate_about(true)
  assert(owner.pr_timer, "About generation cancelled the independent mock PR timer")
  generation[#generation]({ state = "error", error = "provider authentication failed" })
  assert(#errors == 1 and errors[1]:find("authentication failed", 1, true), "current generation failure was hidden")
  local closed_about = owner.about
  owner.close()
  assert(not owner.pr_timer, "closed Status retained its mock PR timer")
  generation[2]({ state = "ready", message = "late message" })
  assert(owner.about == closed_about, "closed context adopted generation")
end, debug.traceback)
client.request_host, ai.ensure, gh.current_pr_async = original_request, original_ai, original_pr
notifications.error = original_error
config.options.about_auto_generate = original_auto
config.options.pr_lookup_mode, config.options.pr_mock_delay_ms = original_lookup, original_lookup_delay
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
