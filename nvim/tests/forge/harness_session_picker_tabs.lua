vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
-- The session picker acts on Harness views in other tabs: deleting a session closes every view
-- that shows it, and opening a retained session in the current tab keeps that tab in place.
require("forge.infra.config").setup({})
require("forge.infra.highlights").setup()

local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local navigation = require("forge.views.harness.session_navigation")
local picker = require("forge.views.picker")
local session = require("forge.session")

local workspace_path = vim.fn.getcwd()
local deleted_by_id = {}
local document_request_by_session = {}
local failures = {}
client.host_get_generation = function() return 1 end
client.host_is_accepting = function() return true end
client.session_events_subscribe = function() return function() end end
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end

---@param session_id string
---@return table
local function session_record(session_id)
  return { id = session_id, name = session_id .. " name", provider = "mock", model = "mock",
    approval_mode = "ask", workspace = workspace_path, created_at_ms = 1 }
end

---Build a native document response for the production presentation adapter.
local function document_snapshot(document, text)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = {} } } } }
end

local listed_id_list = {}
client.request = function(request, callback)
  local params = request.params or {}
  local method = request.method
  local result, failure, detail
  if method == "session.input.get" then
    result = { revision = 0, messages = {}, suspended = true, execution_busy = false }
  elseif method == "session.health.get" then result = { responsive = true }
  elseif method == "session.provider.model.list" then result = {}
  elseif method == "session.list" then
    result = {}
    for _, session_id in ipairs(listed_id_list) do
      if not deleted_by_id[session_id] then result[#result + 1] = session_record(session_id) end
    end
  elseif method == "session.preview" then result = { lines = { "preview " .. params.session_id } }
  elseif method == "session.delete" then
    deleted_by_id[params.session_id] = true
    result = { deleted = params.session_id }
  elseif method == "session.resume" then result = { session = session_record(params.session_id) }
  elseif method == "session.state.get" then result = { session = session_record(request.session_id) }
  elseif method == "session.document" then
    if params.operation ~= "close" then
      document_request_by_session[request.session_id] = (document_request_by_session[request.session_id] or 0) + 1
    end
    if deleted_by_id[request.session_id] then
      -- The host reports a deleted session's documents with its generic request failure code.
      failure, detail = "session not found", { code = "request_failed" }
    elseif params.operation == "open" then
      result = { transcript = document_snapshot(params.document, request.session_id .. " transcript"),
        status = document_snapshot(params.document .. ":status", "Waiting") }
    elseif params.operation == "background_terminals" then result = { supported = false }
    elseif params.operation == "sync" then
      result = { patch = {}, status = document_snapshot(params.document .. ":status", "Waiting") }
    elseif params.operation == "staging" then result = { updated = true }
    elseif params.operation == "close" then result = { closed = true }
    else result = { patch = {} } end
  else error("Unexpected native request: " .. method) end
  vim.schedule(function() callback(result, failure, detail) end)
end

---@param state ForgeHarnessPresentationState
local function settled(state)
  assert(vim.wait(2000, function() return state.presentation and state.presentation.ready end, 1),
    "presentation did not become ready")
end

---Construct a session in its own tab with a real presentation.
---@param session_id string
---@return ForgeHarnessPresentationState
local function create_session(session_id)
  local pending = navigation.begin_new(session_id)
  local state = pending.state
  state.ready = true
  navigation.complete_pending(pending, { session = session_record(session_id) })
  settled(state)
  listed_id_list[#listed_id_list + 1] = session_id
  return state
end

---Open /sessions from `state`, filter to `session_id`, and run the insert-mode `key` action.
---@param state ForgeHarnessPresentationState
---@param session_id string
---@param key string
local function picker_action(state, session_id, key)
  vim.api.nvim_set_current_win(state.composer_win)
  controller.open_session_picker()
  assert(vim.wait(1000, function() return picker.is_open("sessions") end, 1), "session picker did not open")
  vim.fn.maparg("/", "n", false, true).callback()
  local active_picker = picker._state_for_test()
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { session_id })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  vim.fn.maparg(key, "i", false, true).callback()
end

local ok, failure = xpcall(function()
  local registry = navigation._registry_for_test()

  -- Deleting a session shown in another tab closes that view instead of leaving it retrying.
  local alpha = create_session("alpha")
  local bravo = create_session("bravo")
  local bravo_tab, bravo_presentation = bravo.timeline_tab, bravo.presentation
  local bravo_buffer_list = { bravo.transcript_buf, bravo.composer_buf }
  picker_action(alpha, "bravo", "<C-j>")
  assert(vim.wait(1000, function() return deleted_by_id.bravo end, 1), "picker did not delete bravo")
  local requests_at_deletion = document_request_by_session.bravo
  assert(vim.wait(1000, function() return not vim.api.nvim_tabpage_is_valid(bravo_tab) end, 1),
    "the tab showing the deleted session stayed open")
  assert(bravo_presentation.closed, "the deleted session's presentation kept syncing")
  assert(registry.bravo == nil and session.harness_by_id.bravo == nil, "the deleted session stayed registered")
  for _, buffer in ipairs(bravo_buffer_list) do
    assert(not vim.api.nvim_buf_is_valid(buffer), "the deleted session retained its buffer")
  end
  assert(document_request_by_session.bravo == requests_at_deletion,
    "the deleted session's presentation issued document reads after deletion")
  assert(not vim.api.nvim_eval_statusline(vim.o.tabline, { use_tabline = true, maxwidth = 300 }).str:find("bravo", 1, true),
    "a tab label still names the deleted session")
  picker.close(true)

  -- Opening a session retained without a tab replaces the current tab in place.
  local charlie = create_session("charlie")
  local delta = create_session("delta")
  vim.api.nvim_buf_set_lines(delta.composer_buf, 0, -1, false, { "delta draft" })
  assert(require("forge.shared.view_command_set").dispatch(controller.command_set(), "close", delta))
  assert(registry.delta == delta and vim.api.nvim_buf_is_valid(delta.composer_buf), "closing delta did not retain it")
  local charlie_tab = charlie.timeline_tab
  local charlie_tab_number = vim.api.nvim_tabpage_get_number(charlie_tab)
  local tab_count = #vim.api.nvim_list_tabpages()
  picker_action(charlie, "delta", "<CR>")
  assert(vim.wait(1000, function() return delta.presentation and delta.presentation.ready end, 1),
    "delta did not open")
  assert(#vim.api.nvim_list_tabpages() == tab_count, "opening in the current tab changed the tab count")
  assert(delta.timeline_tab == charlie_tab and vim.api.nvim_tabpage_is_valid(charlie_tab),
    "opening in the current tab moved the session to a new tab")
  assert(vim.api.nvim_get_current_tabpage() == charlie_tab
    and vim.api.nvim_tabpage_get_number(charlie_tab) == charlie_tab_number, "the current tab changed position")
  assert(vim.api.nvim_win_get_buf(delta.composer_win) == delta.composer_buf
    and vim.api.nvim_buf_get_lines(delta.composer_buf, 0, -1, false)[1] == "delta draft",
    "the retained session lost its draft")
  assert(session.harness == delta, "the current tab did not select the chosen session")

  -- A session hidden by a plain `:tabclose` of its tab also opens in place of the current tab.
  local echo = create_session("echo")
  local foxtrot = create_session("foxtrot")
  vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(echo.timeline_tab))
  local foxtrot_tab = foxtrot.timeline_tab
  tab_count = #vim.api.nvim_list_tabpages()
  picker_action(foxtrot, "echo", "<CR>")
  assert(vim.wait(1000, function() return session.harness.session and session.harness.session.id == "echo" end, 1),
    "echo did not open")
  assert(#vim.api.nvim_list_tabpages() == tab_count,
    ("opening a tab-closed session in the current tab changed the tab count from %d to %d")
      :format(tab_count, #vim.api.nvim_list_tabpages()))
  assert(vim.api.nvim_get_current_tabpage() == foxtrot_tab, "the tab-closed session opened outside the current tab")
  assert(#failures == 0, table.concat(failures, "\n"))
end, debug.traceback)
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("harness_session_picker_tabs: passed")
vim.cmd("qa!")
