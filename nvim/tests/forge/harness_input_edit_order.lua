vim.loader.enable(false)
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local editor = require("forge.views.harness.queue_editor")
local requests = {}
client.request = function(spec, callback) requests[#requests + 1] = { spec = spec, callback = callback } end
local state = { session = { id = "edit-order" }, queue = {},
  composer_buf = vim.api.nvim_create_buf(false, true) }
input.attach(state, function() end)
local entry = { id = "queued", revision = 1, text = "original", attachments = {},
  purpose = { kind = "user" }, intent = "queue", delivery = "queued" }
requests[1].callback({ revision = 1, messages = { entry }, suspended = false })
local identity = state.queue[1]
local text, failure = editor.move(state, "unsent composer draft", -1)
assert(not failure and text == "original")
local reserve = requests[2]
assert(reserve.spec.params.action.kind == "reserve")
assert(identity.reservation == nil, "editor granted its own reservation")
local restored, finish_failure = editor.finish(state, "changed")
assert(restored == "unsent composer draft" and not finish_failure)
assert(#requests == 2, "commit raced ahead of its reservation acknowledgement")
entry = vim.deepcopy(entry)
entry.revision, entry.reservation = 2, reserve.spec.params.action.reservation
reserve.callback({ revision = 2, operation_id = reserve.spec.params.id, upsert = { entry }, removed = {} })
assert(#requests == 3 and requests[3].spec.params.action.kind == "commit")
assert(requests[3].spec.params.action.edits[1].revision == 2, "commit used a pre-reservation revision")
assert(identity.text == "original", "editor committed before its host result")
entry.text, entry.revision, entry.reservation = "changed", 3, nil
requests[3].callback({ revision = 3, operation_id = requests[3].spec.params.id, upsert = { entry }, removed = {} })
assert(state.queue[1] == identity and identity.text == "changed")
print("harness_input_edit_order: passed")
vim.cmd("qa!")
