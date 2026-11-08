-- Unit tests for `github.issue_document` (issue_document.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "issue_document", run = function()
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
  assert(method == "github.issue.document")
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
assert(vim.bo[state.replica.buffer].modifiable, "issue field remained read-only on entry")
assert(vim.api.nvim_buf_get_name(state.replica.buffer):find("forge://issue/", 1, true),
  "issue buffer lacks a name for native write commands")
assert(captured.repository.name == "other" and captured.repository.hostname == "enterprise.example")
assert(captured.database:gsub("\\", "/"):find("enterprise.example/repos/owner/other/issues/issues.redb", 1, true))
assert(vim.b[state.replica.buffer].github_user_completion == true, "native issue did not enable assignee completion")
assert(vim.b[state.replica.buffer].github_repo == "owner/other", "native issue completion lost repository identity")
assert(vim.bo[state.replica.buffer].filetype == "ForgeGithubIssue", "native issue lost the issue filetype")
assert(vim.wo[0].wrap and vim.wo[0].linebreak and vim.wo[0].breakindent, "native issue lost wrapped display options")
assert(vim.wo[0].number and vim.wo[0].statuscolumn == "%l %=%s", "native issue lost inherited editor columns")
assert(not vim.wo[0].winbar:find("GitHub Issue #7", 1, true),
  "native issue retained its title when the command hints consume the available width")
assert(vim.wo[0].winbar:find("<C-s> sync", 1, true) and vim.wo[0].winbar:find("q close", 1, true),
  "native issue command bar omitted its save and close actions")
assert(vim.fn.maparg("<C-s>", "i", false, true).desc == "Save issue", "native issue lost insert save")
assert(vim.fn.maparg("<CR>", "i", false, true).expr == 1, "native issue lost scalar newline guard")
assert(vim.fn.maparg("?", "n", false, true).desc == "Show issue commands", "native issue lost command help")
assert(vim.wait(1000, function() return #views >= 1 end), "native issue did not register its input view")
vim.fn.maparg("b", "n", false, true).callback()
assert(vim.wait(1000, function() return browsed_url ~= nil end))
assert(browse_input.action == "browse" and not browse_input.target, "issue browse required a row target")
assert(browsed_url == "https://enterprise.example/owner/other/issues/7")
vim.ui.open = original_open
vim.cmd("vsplit")
local split_window = vim.api.nvim_get_current_win()
assert(vim.wait(1000, function() return state.view[split_window] ~= nil and #views >= 2 end),
  "native issue did not register the split view")
assert(vim.wo[split_window].wrap and vim.wo[split_window].linebreak and vim.wo[split_window].breakindent,
  "native issue did not apply display options to the split view")
vim.api.nvim_win_close(split_window, true)
state.recovery = { capture = { operation_id = "uncertain-save" }, state = { phase = "outcome_unknown" } }
assert(adapter.resolve(state, { resolution = "close_unknown" }), "issue recovery did not submit an explicit decision")
assert(#resolves == 1 and resolves[1].operation_id == "uncertain-save")
assert(resolves[1].resolution.resolution == "close_unknown", "issue recovery changed the selected decision")
assert(vim.wait(1000, function() return state.fresh_required == true end), "closed unknown outcome did not require a fresh observation")
assert(adapter.save(state) == false and saves == 0, "fresh-source gate replayed an uncertain save")
assert(adapter.refresh(state), "issue refresh did not start after CloseUnknown")
assert(vim.wait(1000, function() return state.fresh_required == false end), "fresh issue observation did not clear the save gate")
local function type_text(value)
  vim.bo[state.replica.buffer].modifiable = true
  local previous = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1]
  vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, #previous, { value })
  editable.capture(state.replica.editable, "body")
  assert(#editable.capture_draft(state.replica.editable) > 0, "native typing must remain locally captured")
end
local function confirm(index)
  local selected = assert(pending[index])
  local edit = assert(selected.capture[1])
  text, revision = edit.text, revision + 1
  selected.callback({ fields = { { region = "body", revision = revision, sequence = edit.sequence } } })
end
vim.cmd("normal! gg0c$captured")
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "captured",
  "normal issue editing required a forced modifiable option")
assert(#pending == 0, "native typing sent an edit request")
vim.cmd("write")
assert(pending[1].capture[1].sequence > 100, "restored draft reused an earlier edit sequence")
assert(pending[1].capture[1].text == "captured")
type_text("newer\r")
adapter.save(state)
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "newer\r",
  "issue save changed raw carriage return text")
assert(saves == 1)
type_text("unsaved after second save")
confirm(1)
assert(vim.wait(1000, function() return #pending == 2 end))
assert(pending[2].capture[1].text == "newer\r", "queued issue save recaptured newer typing")
assert(pending[2].capture[1].base == 1, "queued capture did not use the accepted revision")
confirm(2)
assert(vim.wait(1000, function() return not state.saving end))
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "unsaved after second save")
assert(vim.bo[state.replica.buffer].modified, "save cleared newer typing")
adapter.close(state)
assert(#closed == 0 and state.active)
assert(state.hidden and vim.api.nvim_get_current_buf() == state.origin, "close did not hide the unsaved issue")
local reopened = adapter.open({ repository = state.repository, number = 7, on_error = error })
assert(reopened == state and vim.api.nvim_get_current_buf() == state.replica.buffer, "reopening discarded the native draft")
adapter.save(state)
confirm(3)
assert(vim.wait(1000, function() return not state.saving and not vim.bo[state.replica.buffer].modified end))
adapter.close(state)
assert(vim.wait(1000, function() return not state.active end))
assert(closed[1] == state.document)
assert(vim.wo[0].winbar == "origin", "native issue close did not restore the invoking winbar")
assert(vim.wo[0].statuscolumn == "%l %=%s", "native issue close did not restore the status column")
local late = adapter.open({ repository = options.repository, number = 8, on_error = error })
adapter.close(late)
opening()
assert(vim.wait(1000, function() return not late.active end))
assert(closed[2] == late.document and not late.replica)
local is_current = true
local stale = adapter.open({ repository = options.repository, number = 8,
  is_current = function() return is_current end, on_error = error })
is_current = false
opening()
assert(vim.wait(1000, function() return not stale.active end))
assert(closed[3] == stale.document and not stale.replica)
assert(#errors == 2 and errors[1]:find("closed without confirmation", 1, true)
  and errors[2]:find("fresh remote observation", 1, true), table.concat(errors, "\n"))
adapter._set_runner_for_test(nil)
print("issue_document: explicit captures, queued saves, newer typing, draft close/reopen, recovery, and stale open cleanup passed")
  end },
  { name = "issue_rebind", run = function()
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
  assert(method == "github.issue.document")
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
local original_generation = client.host_get_generation
local generation = original_generation()
client.host_get_generation = function() return generation end
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
client.host_get_generation = original_generation

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
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
