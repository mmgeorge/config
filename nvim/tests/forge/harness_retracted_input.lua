vim.loader.enable(false)
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local submission = require("forge.views.harness.prompt_submission")
local requests = {}
client.request = function(spec, callback) requests[#requests + 1] = { spec = spec, callback = callback } end
local state = { session = { id = "cancelled-input" }, queue = {}, activity = { revision = 0, phase = "idle" } }
input.attach(state, function() end)
requests[1].callback({ revision = 0, suspended = false, messages = {} })
state.composer_buf = vim.api.nvim_create_buf(false, true)
local context = { document = "draft", buffer = state.composer_buf, is_current = function() return true end }
for _, scenario in ipairs({ { newer = false }, { newer = true }, { newer = false, late = true }, { newer = true, late = true } }) do
  local newer_draft = scenario.newer
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Cancel this prompt" })
  submission.start(state, { context = context }, function() end)
  local request = requests[#requests]
  local message = vim.deepcopy(request.spec.params.action.message)
  message.revision = 1
  local revision = state.input.revision + 1
  request.callback({ operation_id = request.spec.params.id, revision = revision,
    suspended = false, upsert = { message }, removed = {} })
  if newer_draft then vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Newer unsent draft" }) end
  local count = #requests
  local cancellation = { revision = revision + 1, suspended = true,
    upsert = {}, removed = { message.id }, retracted = message.id }
  if scenario.late then
    input.receive(state, { revision = revision + 2,
      snapshot = { revision = revision + 2, control_revision = 9, suspended = true, messages = {} } })
  end
  input.receive(state, cancellation)
  input.receive(state, cancellation)
  assert(#requests == count, "rendering a cancelled input issued another queue mutation")
  assert(#state.input.confirmed == 0, "cancelled input remained in the queue")
  if scenario.late then
    assert(state.input.revision == revision + 2 and state.input.control_revision == 9,
      "late retraction regressed the authoritative input projection")
  end
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1]
    == (newer_draft and "Newer unsent draft" or "Cancel this prompt"), "cancellation overwrote or lost the draft")
end
print("harness_retracted_input: passed")
vim.cmd("qa!")
