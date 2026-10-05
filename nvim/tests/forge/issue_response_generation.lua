vim.opt.runtimepath:append("nvim")
local adapter = require("github.issue_document")
local editable = require("forge.editable")
local pending, closed, saves, resolves, views, errors = {}, {}, 0, {}, {}, {}
local revision, text = 0, "initial"
local opening, captured
local browse_input, browsed_url
local original_open = vim.ui.open
vim.ui.open = function(url) browsed_url = url end
local function metadata(value, accepted)
  return { target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {},
    editable_region = { { id = "body", revision = accepted, sequence = 100,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #value } } } } }
end
adapter._set_runner_for_test(function(method, params, callback)
  assert(method == "issue.document")
  if params.operation == "open" then
    captured = vim.deepcopy(params)
    local result = { snapshot = { document = params.document, revision = revision,
      block = { { id = "body", text = { text }, metadata = metadata(text, revision) } } },
      fields = { { region = "body", revision = revision, sequence = 100 } } }
    if params.number == 8 then opening = function() callback(result) end else callback(result) end
  elseif params.operation == "edit" then error("typing must not send an edit request")
  elseif params.operation == "act" then
    browse_input = params.input
    local effect = vim.deepcopy(params.input)
    effect.id, effect.kind, effect.url = "issue-browse-test", "browser", "https://enterprise.example/owner/other/issues/7"
    callback({ effect = effect })
  elseif params.operation == "save" then
    saves = saves + 1
    pending[#pending + 1] = { capture = vim.deepcopy(params.capture), document = params.document, callback = callback }
  elseif params.operation == "snapshot" then
    callback({ document = params.document, revision = revision,
      block = { { id = "body", text = { text }, metadata = metadata(text, revision) } } })
  elseif params.operation == "resolve" then
    resolves[#resolves + 1] = vim.deepcopy(params)
    callback({ fields = {}, recovery = { capture = { operation_id = params.operation_id },
      state = { phase = "user_closed_unknown" } }, fresh_required = true })
  elseif params.operation == "refresh" then callback({ fields = {}, fresh_required = false })
  elseif params.operation == "view" then views[#views + 1] = vim.deepcopy(params) callback(vim.NIL)
  elseif params.operation == "close_view" then callback(vim.NIL)
  elseif params.operation == "close" then closed[#closed + 1] = params.document callback({ collected = true })
  else error("unexpected operation " .. params.operation) end
end)
local options = { repository = { hostname = "enterprise.example", owner = "owner", name = "other" },
  number = 7, on_error = function(message) errors[#errors + 1] = message end }
vim.wo[0].number = true
vim.o.columns = 120
vim.wo[0].statuscolumn = "%l %=%s"
vim.wo[0].winbar = "origin"
local state = adapter.open(options)
options.repository.name = "changed"
assert(vim.wait(1000, function() return state.shown end))

local client = require("forge.client")
local original_generation = client.host_generation
local generation = original_generation()
client.host_generation = function() return generation end
vim.bo[state.replica.buffer].modifiable = true
vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, 7, { "retained draft" })
adapter.save(state)
assert(#pending == 1)
vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, 14, { "newer retained draft" })
adapter.save(state)
local accepted = state.replica.editable.region.body.revision
generation = generation + 1
pending[1].callback({ fields = {}, outcome = "confirmed" })
assert(vim.wait(1000, function() return not state.saving end))
assert(state.replica.editable.region.body.revision == accepted)
assert(editable.capture_draft(state.replica.editable)[1].text == "newer retained draft")
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)[1] == "newer retained draft")
assert(errors[#errors]:find("host changed", 1, true))
client.host_generation = original_generation
print("issue_response_generation: stale save completion preserves newer local text and accepted revision")

assert(#pending == 1, "host loss dispatched queued issue text against a stale document")
assert(state.save_pending[1].text == "newer retained draft")
