-- Unit tests for `forge.views.harness.queue_editor` (queue_editor.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "input_reopen_edit", run = function()
vim.loader.enable(false)
-- Reopening a queued message before the host publishes the previous edit must reserve the
-- revision that edit produces, not the revision the editor displayed when it committed.
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local editor = require("forge.views.harness.queue_editor")
local requests = {}
client.request = function(spec, callback) requests[#requests + 1] = { spec = spec, callback = callback } end
local state = { session = { id = "reopen-edit" }, queue = {},
  composer_buf = vim.api.nvim_create_buf(false, true) }
input.attach(state, function() end)
local entry = { id = "queued", revision = 1, text = "original", attachments = {},
  purpose = { kind = "user" }, intent = "queue", delivery = "queued" }
requests[1].callback({ revision = 1, messages = { entry }, suspended = false })
local identity = state.queue[1]

editor.move(state, "", -1)
local reserve = requests[2]
assert(reserve.spec.params.action.kind == "reserve" and reserve.spec.params.action.revision == 1)
entry = vim.deepcopy(entry)
entry.revision, entry.reservation = 2, reserve.spec.params.action.reservation
reserve.callback({ revision = 2, operation_id = reserve.spec.params.id, upsert = { entry }, removed = {} })

editor.finish(state, "first edit")
local commit = requests[3]
assert(commit and commit.spec.params.action.kind == "commit", "first edit did not commit")
assert(commit.spec.params.action.edits[1].revision == 2)

-- The host has not published the commit result yet, so the displayed revision is stale.
local text, failure = editor.move(state, "", -1)
assert(not failure and text == "original", "reopened edit did not show the queued entry")
assert(#requests == 3, "reopened edit reserved before the previous edit was published: "
  .. vim.inspect(requests[4] and requests[4].spec.params.action))

entry = vim.deepcopy(entry)
entry.text, entry.revision, entry.reservation = "first edit", 3, nil
commit.callback({ revision = 3, operation_id = commit.spec.params.id, upsert = { entry }, removed = {} })
assert(#requests == 4 and requests[4].spec.params.action.kind == "reserve", "reopened edit never reserved")
assert(requests[4].spec.params.action.revision == 3, "reopened edit reserved a stale revision: "
  .. tostring(requests[4].spec.params.action.revision))
assert(state.queue[1] == identity and identity.text == "first edit")

-- The reopened edit commits against the revision its own reservation produced.
local reopened = requests[4]
entry = vim.deepcopy(entry)
entry.revision, entry.reservation = 4, reopened.spec.params.action.reservation
reopened.callback({ revision = 4, operation_id = reopened.spec.params.id, upsert = { entry }, removed = {} })
local restored, finish_failure = editor.finish(state, "second edit")
assert(restored == "" and not finish_failure)
assert(#requests == 5 and requests[5].spec.params.action.edits[1].revision == 4)
print("harness_input_reopen_edit: passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
