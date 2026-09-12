vim.opt.runtimepath:append("nvim")
local adapter = require("github.issue_document")
local editable = require("forge.editable")
local pending, closed, saves, resolves, views, errors = {}, {}, 0, {}, {}, {}
local revision, text = 0, "initial"
local opening, captured
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
  elseif params.operation == "edit" then pending[#pending + 1] = { edit = params.edit, callback = callback }
  elseif params.operation == "save" then saves = saves + 1 callback({ fields = {} })
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
  editable.flush(state.replica.editable)
end
local function confirm(index)
  local selected = assert(pending[index])
  local edit = selected.edit
  local previous = revision
  text, revision = edit.text, revision + 1
  selected.callback({ document = edit.document, region = edit.region, sequence = edit.sequence,
    revision = edit.base + 1, patch = { document = edit.document, base = previous, next = revision,
      base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
      removed_block = {}, block_edit = {},
      text_edit = { { start_row = 0, removed_rows = 1, text = { text } } },
      metadata_edit = { { block = "body", row_count = 1, metadata = metadata(text, revision) } } } })
end
type_text("captured")
assert(pending[1].edit.sequence > 100, "restored draft reused an earlier edit sequence")
type_text("newer")
adapter.save(state)
assert(saves == 0)
confirm(1)
assert(vim.wait(1000, function() return #pending == 2 end))
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, 1, false)[1] == "newer")
confirm(2)
assert(vim.wait(1000, function() return saves == 1 end))
type_text("durable before close")
adapter.close(state)
assert(#closed == 0 and state.active)
confirm(3)
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
print("issue_document: captured host/repository, recovery gating, newer typing, save/close acknowledgement, and stale open cleanup passed")
