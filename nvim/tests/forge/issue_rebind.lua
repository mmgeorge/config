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

state.fields = { { region = "body", baseline = "initial", revision = 0 } }
local original_document, original_buffer = state.document, state.replica.buffer
local original_origin = state.origin
adapter._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    opening = function()
      callback({ snapshot = { document = params.document, revision = 0, block = {
        { id = "body", text = { "initial" }, metadata = metadata("initial", 0) },
      } }, fields = { { region = "body", baseline = "initial", revision = 0 } } })
    end
  elseif params.operation == "view" then callback(vim.NIL)
  else error("rebind sent unexpected operation " .. params.operation) end
end)
local recovered
assert(adapter.rebind(state, function(value, failure) assert(not failure, failure) recovered = value end))
assert(vim.api.nvim_buf_get_lines(original_buffer, 0, -1, false)[1] == "newer retained draft")
vim.api.nvim_buf_set_text(original_buffer, 0, 0, 0, 20, { "latest rebind typing", "λ\r", "" })
opening()
assert(vim.wait(1000, function() return recovered ~= nil end))
assert(state.document ~= original_document and state.replica.buffer == original_buffer)
assert(state.origin == original_origin, "recovery replaced the original return buffer")
assert(state.active and not state.hidden and state.view[state.window], "recovery did not attach the retained view")
assert(editable.capture_draft(state.replica.editable)[1].text == "latest rebind typing\nλ\r\n")
assert(state.save_pending[1].text == "newer retained draft")
assert(state.save_pending[1].document == state.document)
assert(state.save_pending[1].sequence < editable.capture_draft(state.replica.editable)[1].sequence)
assert(vim.bo[original_buffer].modified, "rebind cleared retained dirty text")
print("issue_rebind: baseline validation, current raw text, buffer identity, and pending capture sequences passed")

local retained_text = vim.api.nvim_buf_get_lines(original_buffer, 0, -1, false)
local retained_document = state.document
adapter._set_runner_for_test(function(_, params, callback)
  if params.operation == "close" then
    closed[#closed + 1] = params.document
    callback({ collected = true })
    return
  end
  assert(params.operation == "open")
  callback({ snapshot = { document = params.document, revision = 0, block = {
    { id = "body", text = { "remote changed" }, metadata = metadata("remote changed", 0) },
  } }, fields = { { region = "body", baseline = "remote changed", revision = 0 } } })
end)
local conflict
assert(adapter.rebind(state, function(value, failure) assert(not value) conflict = failure end))
assert(vim.wait(1000, function() return conflict ~= nil end))
assert(state.document == retained_document)
assert(#closed == 1 and closed[1] ~= retained_document, "rejected replacement document leaked")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(original_buffer, 0, -1, false), retained_text))
assert(vim.bo[original_buffer].modified)
print("issue_rebind: changed remote baselines preserve draft text and retained identity")

state.save_pending = { { document = state.document, region = "missing", base = 0, sequence = 200, text = "queued" } }
state.fields[#state.fields + 1] = { region = "missing", baseline = "queued baseline", revision = 0 }
local before_close_count = #closed
adapter._set_runner_for_test(function(_, params, callback)
  if params.operation == "close" then
    closed[#closed + 1] = params.document
    callback({ collected = true })
    return
  end
  assert(params.operation == "open")
  callback({ snapshot = { document = params.document, revision = 0, block = {
    { id = "body", text = { "initial" }, metadata = metadata("initial", 0) },
  } }, fields = { { region = "body", baseline = "initial", revision = 0 } } })
end)
conflict = nil
assert(adapter.rebind(state, function(value, failure) assert(not value) conflict = failure end))
assert(vim.wait(1000, function() return conflict ~= nil end))
assert(conflict:find("missing", 1, true), "recovery did not validate the queued-only field")
assert(#closed == before_close_count + 1)
assert(state.document == retained_document and state.save_pending[1].text == "queued")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(original_buffer, 0, -1, false), retained_text))
print("issue_rebind: queued captures survive replacement rejection")
