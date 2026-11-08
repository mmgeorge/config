-- Unit tests for `github.issue_document` (issue_document.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["issue_document"] = function()
  local adapter = require("github.issue_document")
  local editable = require("forge.editable")
  local pending, closed, saves, resolves, views, errors = {}, {}, 0, {}, {}, {}
  local revision, text = 0, "initial"
  local opening, captured
  local browse_input, browsed_url
  vim.ui.open = function(url) browsed_url = url end
  local function metadata(value, accepted)
    return { target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {},
      editable_region = { { id = "body", revision = accepted, sequence = 100,
        range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #value } } } } }
  end
  adapter._set_runner_for_test(function(method, params, callback)
    eq(method, "github.issue.document")
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
  wait_until(function() return state.shown end, "native issue was not shown")
  assert(vim.bo[state.replica.buffer].modifiable, "issue field remained read-only on entry")
  assert(vim.api.nvim_buf_get_name(state.replica.buffer):find("forge://issue/", 1, true),
    "issue buffer lacks a name for native write commands")
  assert(captured.repository.name == "other" and captured.repository.hostname == "enterprise.example")
  assert(captured.database:gsub("\\", "/"):find("enterprise.example/repos/owner/other/issues/issues.redb", 1, true))
  eq(vim.b[state.replica.buffer].github_user_completion, true, { fail_reason = "native issue did not enable assignee completion" })
  eq(vim.b[state.replica.buffer].github_repo, "owner/other", { fail_reason = "native issue completion lost repository identity" })
  eq(vim.bo[state.replica.buffer].filetype, "ForgeGithubIssue", { fail_reason = "native issue lost the issue filetype" })
  assert(vim.wo[0].wrap and vim.wo[0].linebreak and vim.wo[0].breakindent, "native issue lost wrapped display options")
  assert(vim.wo[0].number and vim.wo[0].statuscolumn == "%l %=%s", "native issue lost inherited editor columns")
  assert(not vim.wo[0].winbar:find("GitHub Issue #7", 1, true),
    "native issue retained its title when the command hints consume the available width")
  assert(vim.wo[0].winbar:find("<C-s> sync", 1, true) and vim.wo[0].winbar:find("q close", 1, true),
    "native issue command bar omitted its save and close actions")
  eq(keymap.get("<C-s>", { mode = "i" }).desc, "Save issue", { fail_reason = "native issue lost insert save" })
  eq(keymap.get("<CR>", { mode = "i" }).expr, 1, { fail_reason = "native issue lost scalar newline guard" })
  eq(keymap.get("?").desc, "Show issue commands", { fail_reason = "native issue lost command help" })
  wait_until(function() return #views >= 1 end, "native issue did not register its input view")
  keymap.invoke("b")
  wait_until(function() return browsed_url ~= nil end, "issue browse did not open a URL")
  assert(browse_input.action == "browse" and not browse_input.target, "issue browse required a row target")
  eq(browsed_url, "https://enterprise.example/owner/other/issues/7")
  vim.cmd("vsplit")
  local split_window = vim.api.nvim_get_current_win()
  wait_until(function() return state.view[split_window] ~= nil and #views >= 2 end, "native issue did not register the split view")
  assert(vim.wo[split_window].wrap and vim.wo[split_window].linebreak and vim.wo[split_window].breakindent,
    "native issue did not apply display options to the split view")
  vim.api.nvim_win_close(split_window, true)
  state.recovery = { capture = { operation_id = "uncertain-save" }, state = { phase = "outcome_unknown" } }
  assert(adapter.resolve(state, { resolution = "close_unknown" }), "issue recovery did not submit an explicit decision")
  assert(#resolves == 1 and resolves[1].operation_id == "uncertain-save")
  eq(resolves[1].resolution.resolution, "close_unknown", { fail_reason = "issue recovery changed the selected decision" })
  wait_until(function() return state.fresh_required == true end, "closed unknown outcome did not require a fresh observation")
  assert(adapter.save(state) == false and saves == 0, "fresh-source gate replayed an uncertain save")
  assert(adapter.refresh(state), "issue refresh did not start after CloseUnknown")
  wait_until(function() return state.fresh_required == false end, "fresh issue observation did not clear the save gate")
  local function type_text(value)
    vim.bo[state.replica.buffer].modifiable = true
    local previous = buffer.line(state.replica.buffer, 1)
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
  assert(buffer.line(state.replica.buffer, 1) == "captured",
    "normal issue editing required a forced modifiable option")
  eq(#pending, 0, { fail_reason = "native typing sent an edit request" })
  vim.cmd("write")
  assert(pending[1].capture[1].sequence > 100, "restored draft reused an earlier edit sequence")
  eq(pending[1].capture[1].text, "captured")
  type_text("newer\r")
  adapter.save(state)
  assert(buffer.line(state.replica.buffer, 1) == "newer\r",
    "issue save changed raw carriage return text")
  eq(saves, 1)
  type_text("unsaved after second save")
  confirm(1)
  wait_until(function() return #pending == 2 end, "queued issue save was not submitted after the first confirmation")
  eq(pending[2].capture[1].text, "newer\r", { fail_reason = "queued issue save recaptured newer typing" })
  eq(pending[2].capture[1].base, 1, { fail_reason = "queued capture did not use the accepted revision" })
  confirm(2)
  wait_until(function() return not state.saving end, "second issue save did not settle")
  eq(buffer.line(state.replica.buffer, 1), "unsaved after second save")
  assert(vim.bo[state.replica.buffer].modified, "save cleared newer typing")
  adapter.close(state)
  assert(#closed == 0 and state.active)
  assert(state.hidden and vim.api.nvim_get_current_buf() == state.origin, "close did not hide the unsaved issue")
  local reopened = adapter.open({ repository = state.repository, number = 7, on_error = error })
  assert(reopened == state and vim.api.nvim_get_current_buf() == state.replica.buffer, "reopening discarded the native draft")
  adapter.save(state)
  confirm(3)
  wait_until(function() return not state.saving and not vim.bo[state.replica.buffer].modified end, "confirmed issue save did not leave the buffer clean")
  adapter.close(state)
  wait_until(function() return not state.active end, "closing a saved issue did not release it")
  assert(closed[1] == state.document)
  eq(vim.wo[0].winbar, "origin", { fail_reason = "native issue close did not restore the invoking winbar" })
  eq(vim.wo[0].statuscolumn, "%l %=%s", { fail_reason = "native issue close did not restore the status column" })
  local late = adapter.open({ repository = options.repository, number = 8, on_error = error })
  adapter.close(late)
  opening()
  wait_until(function() return not late.active end, "closing a still-opening issue did not release it")
  assert(closed[2] == late.document and not late.replica)
  local is_current = true
  local stale = adapter.open({ repository = options.repository, number = 8,
    is_current = function() return is_current end, on_error = error })
  is_current = false
  opening()
  wait_until(function() return not stale.active end, "stale issue open did not release it")
  assert(closed[3] == stale.document and not stale.replica)
  assert(#errors == 2 and errors[1]:find("closed without confirmation", 1, true)
    and errors[2]:find("fresh remote observation", 1, true), table.concat(errors, "\n"))
  adapter._set_runner_for_test(nil)
end

T["issue_rebind"] = function()
  local adapter = require("github.issue_document")
  local editable = require("forge.editable")
  local pending, closed, saves, resolves, views, errors = {}, {}, 0, {}, {}, {}
  local revision, text = 0, "initial"
  local opening, captured
  local browse_input, browsed_url
  vim.ui.open = function(url) browsed_url = url end
  local function metadata(value, accepted)
    return { target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {},
      editable_region = { { id = "body", revision = accepted, sequence = 100,
        range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #value } } } } }
  end
  adapter._set_runner_for_test(function(method, params, callback)
    eq(method, "github.issue.document")
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
  wait_until(function() return state.shown end, "native issue was not shown")

  local client = require("forge.client")
  local original_generation = client.host_get_generation
  local generation = original_generation()
  client.host_get_generation = function() return generation end
  vim.bo[state.replica.buffer].modifiable = true
  vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, 7, { "retained draft" })
  adapter.save(state)
  eq(#pending, 1)
  vim.api.nvim_buf_set_text(state.replica.buffer, 0, 0, 0, 14, { "newer retained draft" })
  adapter.save(state)
  local accepted = state.replica.editable.region.body.revision
  generation = generation + 1
  pending[1].callback({ fields = {}, outcome = "confirmed" })
  wait_until(function() return not state.saving end, "retained issue save did not settle after the host changed")
  assert(state.replica.editable.region.body.revision == accepted)
  eq(editable.capture_draft(state.replica.editable)[1].text, "newer retained draft")
  eq(buffer.line(state.replica.buffer, 1), "newer retained draft")
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
  eq(buffer.line(original_buffer, 1), "newer retained draft")
  vim.api.nvim_buf_set_text(original_buffer, 0, 0, 0, 20, { "latest rebind typing", "λ\r", "" })
  opening()
  wait_until(function() return recovered ~= nil end, "issue rebind did not recover")
  assert(state.document ~= original_document and state.replica.buffer == original_buffer)
  assert(state.origin == original_origin, "recovery replaced the original return buffer")
  assert(state.active and not state.hidden and state.view[state.window], "recovery did not attach the retained view")
  eq(editable.capture_draft(state.replica.editable)[1].text, "latest rebind typing\nλ\r\n")
  eq(state.save_pending[1].text, "newer retained draft")
  assert(state.save_pending[1].document == state.document)
  assert(state.save_pending[1].sequence < editable.capture_draft(state.replica.editable)[1].sequence)
  assert(vim.bo[original_buffer].modified, "rebind cleared retained dirty text")

  local retained_text = buffer.lines(original_buffer)
  local retained_document = state.document
  adapter._set_runner_for_test(function(_, params, callback)
    if params.operation == "close" then
      closed[#closed + 1] = params.document
      callback({ collected = true })
      return
    end
    eq(params.operation, "open")
    callback({ snapshot = { document = params.document, revision = 0, block = {
      { id = "body", text = { "remote changed" }, metadata = metadata("remote changed", 0) },
    } }, fields = { { region = "body", baseline = "remote changed", revision = 0 } } })
  end)
  local conflict
  assert(adapter.rebind(state, function(value, failure) assert(not value) conflict = failure end))
  wait_until(function() return conflict ~= nil end, "rebind against a changed remote did not report a conflict")
  assert(state.document == retained_document)
  assert(#closed == 1 and closed[1] ~= retained_document, "rejected replacement document leaked")
  eq(buffer.lines(original_buffer), retained_text)
  assert(vim.bo[original_buffer].modified)

  state.save_pending = { { document = state.document, region = "missing", base = 0, sequence = 200, text = "queued" } }
  state.fields[#state.fields + 1] = { region = "missing", baseline = "queued baseline", revision = 0 }
  local before_close_count = #closed
  adapter._set_runner_for_test(function(_, params, callback)
    if params.operation == "close" then
      closed[#closed + 1] = params.document
      callback({ collected = true })
      return
    end
    eq(params.operation, "open")
    callback({ snapshot = { document = params.document, revision = 0, block = {
      { id = "body", text = { "initial" }, metadata = metadata("initial", 0) },
    } }, fields = { { region = "body", baseline = "initial", revision = 0 } } })
  end)
  conflict = nil
  assert(adapter.rebind(state, function(value, failure) assert(not value) conflict = failure end))
  wait_until(function() return conflict ~= nil end, "rebind with a missing queued field did not report a conflict")
  assert(conflict:find("missing", 1, true), "recovery did not validate the queued-only field")
  assert(#closed == before_close_count + 1)
  assert(state.document == retained_document and state.save_pending[1].text == "queued")
  eq(buffer.lines(original_buffer), retained_text)
end

return T
