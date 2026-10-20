vim.loader.enable(false)
local source = require("forge.views.harness.completion.command_source")
assert(not source.accepts_prompt("/sess"))
assert(not source.accepts_prompt("  /sess extra text"))
assert(not source.accepts_prompt("/"))
assert(source.accepts_prompt("/sessions anything /unrecognized"))
assert(source.accepts_prompt("/plan arbitrary prompt"))
assert(source.accepts_prompt("/plan"))
assert(source.accepts_prompt("Explain /sess"))
assert(not source.queues_execution("/plan inspect screenshot.png"),
  "filename arguments must not override supported command routing")
assert(not source.accepts_prompt("/spawn\nscreenshot.png"),
  "an extension on a later line must not unlock an unknown slash command")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local warnings = {}
notifications.warn = function(message) warnings[#warnings + 1] = message end
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.queue = {}
for _, prompt in ipairs({ "/sess", "/spawn", "/unrecognized extra text", "/workspace/example",
  "/workspace/", "/workspace.name/README", "/sessions-extra" }) do
  for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
    local warning_count = #warnings
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { prompt })
    submit()
    assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == prompt)
    assert(#state.queue == 0)
    assert(#warnings == warning_count + 1 and warnings[#warnings]:find(prompt:match("^%S+"), 1, true),
      "rejected slash commands must explain why the draft was retained")
  end
end
warnings = {}
local client = require("forge.client")
require("forge.views.harness.prompt_history").record = function() end
local requests = {}
client.request_for = function(_, method, params, callback)
  requests[#requests + 1] = { method = method, params = params }
  callback({})
end
controller.refresh_winbar = function() end
state.session = { id = "plan-mode", mode = "write", execution_mode = "write" }
state.transcript_win, state.composer_win = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_win()
state.no_checkpoint = false
state.busy = false
for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "  /plan  " })
  submit()
  assert(state.session.execution_mode == "write")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  assert(#state.queue == 0)
end
assert(#requests == 2 and requests[1].method == "plan.list" and requests[2].method == "plan.list")
state.busy = true
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan" })
controller.submit()
assert(#requests == 3 and #warnings == 0 and #state.queue == 0)
state.busy = false
state.capability = { fast_mode = true, ultrafast_mode = true }
state.model_backend, state.model_list = state.session.backend, { { id = state.session.model or "default", is_default = true, service_tiers = { "fast", "ultrafast" } } }
controller.configure = function(params) state.session.service_tier = params.service_tier end
for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
  for _, selection in ipairs({ { "/fast", "fast" }, { "/ultrafast", "ultrafast" }, { "/ultrafast", "default" } }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { selection[1] })
    submit()
    assert(state.session.service_tier == selection[2], "slash command selected the wrong tier")
    assert(#state.queue == 0, "service-tier command was queued as a prompt")
  end
end
state.busy = true
controller.drain = function() end
for _, prompt in ipairs({ "/workspace/example.png", "  /workspace/example.PNG\n[Image #1]", "/plan/design.md",
  "/workspace/report.pdf", "/workspace/archive.tar.gz", "/workspace/source.rs explain this", "/sess.png",
  "/workspace/Screen shot 2026-10-19.png\n[Image #1]", "/workspace/example.custom",
  "C:\\workspace\\example.png", "\\\\server\\share\\example.png" }) do
  assert(source.accepts_prompt(prompt) and source.queues_execution(prompt), "ordinary text was treated as a command")
  for _, submit in ipairs({ controller.submit, controller.queue_submit }) do
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(prompt, "\n", { plain = true }))
    submit()
    assert(#state.queue == 1 and state.queue[1].text == vim.trim(prompt), "ordinary prompt did not enter the queue")
    assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
    state.queue = {}
  end
end
assert(#warnings == 0, "ordinary prompts emitted an admission warning")
vim.cmd("new")
vim.api.nvim_set_current_buf(state.composer_buf)
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/x" })
vim.api.nvim_win_set_cursor(0, { 1, 1 })
state.session.backend = "codex"
state.session.model = "default"
state.model_backend = "codex"
require("forge.views.harness.model_pins").get = function() return {} end
state.model_list = {
  { id = "luna", is_default = true, service_tiers = { "fast" } },
  { id = "sol", service_tiers = { "fast", "ultrafast" } },
  { id = "unknown" },
}
for _, case in ipairs({
  { model = "default", capable = true, visible = false },
  { model = "luna", capable = true, visible = false },
  { model = "sol", capable = true, visible = true },
  { model = "sol", capable = false, visible = false },
  { model = "unknown", capable = true, visible = false },
  { model = "missing", capable = true, visible = false },
  { model = "sol", pending = "luna", capable = true, visible = false },
  { model = "luna", pending = "sol", capable = true, visible = true },
}) do
  state.session.model = case.model
  state.pending_config = case.pending and { model = case.pending } or nil
  state.capability.ultrafast_mode = case.capable
  local completion
  source.new():get_completions({}, function(result) completion = result end)
  assert(completion, "command completion did not return")
  assert(vim.iter(completion.items):any(function(item) return item.label == "/ultrafast" end) == case.visible,
    "ultrafast completion ignored selected model or backend capability: " .. vim.inspect(case))
end
print("harness_command_admission: passed")
