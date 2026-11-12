-- Unit tests for `forge.views.harness.session_picker` (session_picker.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["session_picker"] = function()
  local forge = require("forge")
  local controller = require("forge.views.harness.controller")
  local layout = require("forge.views.harness.layout")
  local picker = require("forge.views.picker")
  local session = require("forge.session")
  local session_picker = require("forge.views.harness.session_picker")
  local datetime = require("forge.integrations.datetime")

  local function interaction(session_id, prompt)
    local interaction_id = session_id .. ":interaction"
    local turn_id = interaction_id .. ":turn"
    local message_id = turn_id .. ":message"
    local record = {
      id = interaction_id,
      session_id = session_id,
      ordinal = 1,
      prompt = prompt,
      state = "complete",
      attributed_diff_text = vim.NIL,
      checkpoint_diff_text = vim.NIL,
      created_at_ms = 1,
      completed_at_ms = 2,
      duration_ms = 1,
      token_count = 10,
      node_list = {
        {
          kind = "turn_content",
          id = message_id .. ":content",
          turn_id = turn_id,
          item = { kind = "message", id = message_id },
        },
      },
      turn = { {
        id = turn_id,
        state = { kind = "finished", outcome = "completed" },
        started_at_ms = 1,
        completed_at_ms = 2,
        token_count = 10,
        tool = { order = {}, item = {} },
        message = { {
          id = message_id,
          kind = "assistant",
          delivery = "final",
          text = "Preview response for " .. prompt,
        } },
        item = { { kind = "message", id = message_id } },
      } },
    }
    return record
  end

  local function snapshot(entry)
    local record = interaction(entry.id, "Preview " .. (entry.name == "" and "unnamed" or entry.name))
    return {
      session = entry,
      lines = { record.prompt, record.turn[1].message[1].text },
      status = { kind = "idle" },
      artifact = {},
      approval = {},
      capability = {},
      prompt_history = {},
      agent = { definition = {}, run = {}, turn = {} },
    }
  end

  forge.setup({ walkthrough_inventory = "sem", text_correction = { enabled = false }, harness = { provider = "mock" } })
  local state = session.harness
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()

  local workspace = vim.fn.getcwd()
  local now = 1700000000
  datetime.now_override = function() return now end
  local entry_list = {
    { id = "beta", name = "Beta cleanup", workspace = workspace, provider = "mock", approval_mode = "ask", created_at_ms = (now - 2 * 3600) * 1000 },
    { id = "active", name = "Architecture review", workspace = workspace, provider = "mock", approval_mode = "ask", created_at_ms = (now - 2 * 86400) * 1000, active = true },
    { id = "gamma", name = "Gamma migration", workspace = workspace, provider = "mock", approval_mode = "ask", active = true },
    { id = "remote", name = "Remote review", workspace = workspace .. "-other", provider = "mock", approval_mode = "ask" },
  }
  state.session = entry_list[2]
  state.interaction = {}
  state.timeline = {}
  state.agent = { definition = {}, run = {}, turn = {} }
  local original_transcript_buf = state.transcript_buf
  local input_host = dofile("nvim/lua/forge/tests/support/input.lua").new(state)
  local request_list
  local deleted_session_id = nil
  local defer_preview = false
  local preview_callback_by_session = {}
  local function last_request(method)
    for request_index = #request_list, 1, -1 do
      if request_list[request_index].spec.method == method then return request_list[request_index].spec end
    end
    return nil
  end
  local function find_entry(session_id)
    return vim.iter(entry_list):find(function(candidate) return candidate.id == session_id end)
  end
  request_list = mock_client.respond({
    ["session.list"] = function(request_specification, callback)
      local params = request_specification.params or {}
      local result = vim.tbl_filter(function(entry)
        return not deleted_session_id or entry.id ~= deleted_session_id
      end, entry_list)
      if params.scope == "repo" then
        result = vim.tbl_filter(function(entry) return entry.workspace == workspace end, result)
      end
      callback(vim.deepcopy(result))
    end,
    ["session.preview"] = function(request_specification, callback)
      local params = request_specification.params or {}
      eq(request_specification.session_id, "active",
        { fail_reason = "preview transport must use the captured picker owner, not the candidate or current view" })
      local entry = find_entry(params.session_id)
      if defer_preview then
        preview_callback_by_session[params.session_id] = function()
          callback(snapshot(vim.deepcopy(entry)))
        end
      else
        callback(snapshot(vim.deepcopy(entry)))
      end
    end,
    ["session.delete"] = function(request_specification, callback)
      deleted_session_id = request_specification.params.session_id
      callback({ deleted = request_specification.params.session_id })
    end,
    ["session.resume"] = function(request_specification, callback)
      callback(snapshot(vim.deepcopy(find_entry(request_specification.params.session_id))))
    end,
    -- Activation reconciles through the reconnect primitive after applying the resume snapshot.
    ["session.state.get"] = function(request_specification, callback)
      callback(snapshot(vim.deepcopy(find_entry(request_specification.session_id))))
    end,
    ["session.provider.model.list"] = function(_, callback) callback({}) end,
    ["session.input.get"] = function(request_specification, callback)
      input_host.request(request_specification.method, request_specification.params, callback)
    end,
  })

  session_picker.open({
    transcript_win = state.transcript_win,
    composer_win = state.composer_win,
    window_list = { state.transcript_win, state.composer_win },
    control_win = state.composer_win,
  })
  assert(picker.is_open("sessions"), "session search picker should open")
  eq(request_list[1].spec.params.scope, "repo", { fail_reason = "session search should default to the current workspace" })
  local active_picker = picker._state_for_test()
  local picker_bounds = require("forge.views.picker.layout").host_bounds(active_picker.spec.host.window_list)
  local expected_height = math.max(1, math.floor(picker_bounds.height * 0.35) - 2)
  eq(vim.api.nvim_win_get_height(active_picker.win), expected_height)
  eq(active_picker.spec.page_list[1].wrap_selection, false,
    { fail_reason = "session selection should stop at either end" })
  eq(vim.api.nvim_win_get_config(active_picker.win).title_pos, "left")
  eq(active_picker.spec.page_navigation, "tabs")
  eq(vim.tbl_map(function(page) return page.title end, active_picker.spec.page_list), { "Workspace", "All" })
  assert(buffer.line(active_picker.buf, 1):find("[Workspace]  All", 1, true))
  eq(active_picker.spec.page_list[1].header_right, nil)
  assert(active_picker.spec.page_list[1].option_list[1].label:find("2 days ago", 1, true) == 1)
  assert(active_picker.spec.page_list[1].option_list[3].label:find("2 hours ago", 1, true) == 1)
  eq(vim.tbl_map(function(entry) return { entry.id, entry.section } end, active_picker.spec.page_list[1].option_list), {
    { "active", "Active" }, { "gamma", "Active" }, { "beta", "Inactive" },
  }, { fail_reason = "loaded sessions, including hidden ones, precede saved-only history" })
  local picker_state = require("forge.views.picker.state")
  picker_state.set_query(active_picker.state, active_picker.spec, "a")
  local matched = picker_state.page(active_picker.state, active_picker.spec).option_list
  eq(vim.tbl_map(function(entry) return entry.section end, matched), { "Active", "Active", "Inactive" },
    { fail_reason = "fuzzy scores must not move inactive matches above active matches" })
  picker_state.set_query(active_picker.state, active_picker.spec, "")
  eq(vim.api.nvim_get_current_win(), active_picker.win, { fail_reason = "session picker should start on options" })
  eq(vim.api.nvim_get_mode().mode, "n", { fail_reason = "session picker should start in normal mode" })
  eq(active_picker.frame.search_start, nil, { fail_reason = "normal-mode session picker should hide the search row" })
  eq(active_picker.search_win, nil, { fail_reason = "normal-mode session picker should not open search input" })
  local search_mapping = keymap.get("/")
  assert(search_mapping.callback ~= nil, "session picker should retain search access")
  search_mapping.callback()
  assert(active_picker.frame.search_start ~= nil, "entering search should reveal its row")
  eq(vim.api.nvim_win_get_height(active_picker.win), expected_height)
  eq(vim.api.nvim_get_current_win(), active_picker.search_win, { fail_reason = "/ should focus session search" })
  vim.cmd("stopinsert")
  vim.api.nvim_exec_autocmds("InsertLeave", { buffer = active_picker.search_buf })
  wait_until(function() return active_picker.search_win == nil end, "leaving an empty search should close its input")
  eq(active_picker.frame.search_start, nil, { fail_reason = "empty search row should disappear in normal mode" })
  eq(vim.api.nvim_get_current_win(), active_picker.win, { fail_reason = "empty search should return focus to options" })
  search_mapping.callback()
  assert(active_picker.search_win ~= nil, "search should reopen after the empty row disappears")
  assert(vim.api.nvim_win_get_buf(state.transcript_win) ~= original_transcript_buf,
    "session search should display a separate preview buffer")
  local preview_text = buffer.text(vim.api.nvim_win_get_buf(state.transcript_win))
  assert(preview_text:find("Preview Architecture review", 1, true) ~= nil,
    "initial selection should preview the active session timeline")
  local live_record = interaction("active", "Live update while preview is open")
  state.interaction = { live_record }
  state.timeline = { { kind = "exchange", id = live_record.id, created_at_ms = 1, exchange = live_record } }
  controller.render(true)
  assert(vim.api.nvim_buf_is_valid(original_transcript_buf),
    "background Harness state updates should retain the hidden native transcript")
  preview_text = buffer.text(vim.api.nvim_win_get_buf(state.transcript_win))
  assert(preview_text:find("Preview Architecture review", 1, true) ~= nil,
    "background Harness renders must not overwrite the visible session preview")
  keymap.invoke("<C-j>", { mode = "i" })
  eq(deleted_session_id, nil, { fail_reason = "Ctrl-j should refuse to delete the active session" })
  assert(picker.is_open("sessions"), "refusing active-session deletion should keep search open")

  buffer.set(active_picker.search_buf, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  eq(vim.api.nvim_win_get_height(active_picker.win), expected_height,
    { fail_reason = "filtering sessions should preserve the fixed picker height" })
  preview_text = buffer.text(vim.api.nvim_win_get_buf(state.transcript_win))
  assert(preview_text:find("Preview Gamma migration", 1, true) ~= nil,
    "fuzzy filtering should preview the selected matching session")
  eq(last_request("session.preview").params.session_id, "gamma",
    { fail_reason = "same-provider preview must keep the candidate identity in params" })
  eq(state.session.id, "active", { fail_reason = "preview must not resume its candidate" })
  vim.api.nvim_exec_autocmds("InsertLeave", { buffer = active_picker.search_buf })
  vim.wait(20, function() return false end, 5)
  assert(active_picker.search_win ~= nil and active_picker.frame.search_start ~= nil,
    "leaving a nonempty search should preserve the filter and input")

  defer_preview = true
  local preview_owner = state.session
  state.session = vim.tbl_extend("force", preview_owner, { id = "another-active-view" })
  buffer.set(active_picker.search_buf, { "beta" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  state.session = preview_owner
  buffer.set(active_picker.search_buf, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  preview_callback_by_session.gamma()
  preview_callback_by_session.beta()
  preview_text = buffer.text(vim.api.nvim_win_get_buf(state.transcript_win))
  assert(preview_text:find("Preview Gamma migration", 1, true) ~= nil,
    "an older asynchronous preview must not replace the latest selection")
  defer_preview = false

  keymap.invoke("<Tab>", { mode = "i" })
  eq(last_request("session.list").params.scope, "all", { fail_reason = "Tab should switch to all workspaces" })
  active_picker = picker._state_for_test()
  eq(active_picker.state.page_index, 2)
  assert(buffer.line(active_picker.buf, 1):find("Workspace  [All]", 1, true))
  eq(active_picker.spec.page_list[2].column_headers[5], "Workspace")
  eq(#active_picker.spec.page_list[2].option_list, 4)
  eq(active_picker.state.query_by_page.all, "",
    { fail_reason = "All should have its own search query" })
  vim.api.nvim_set_current_win(active_picker.win)
  keymap.invoke("<Tab>")
  eq(last_request("session.list").params.scope, "repo")
  eq(active_picker.state.page_index, 1)
  eq(active_picker.state.query_by_page.repo, "gamma",
    { fail_reason = "returning to Workspace should restore its query" })
  keymap.invoke("<Right>")
  eq(active_picker.state.page_index, 2)
  keymap.invoke("/")
  buffer.set(active_picker.search_buf, { "remote" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  keymap.invoke("<CR>", { mode = "i" })
  assert(picker.is_open("sessions"), "cross-repository history should preview without resuming")
  eq(state.session.id, "active", { fail_reason = "cross-repository Enter should preserve the active session" })
  buffer.set(active_picker.search_buf, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  local picker_owner = state.session
  state.session = vim.tbl_extend("force", picker_owner, { id = "another-active-view" })
  keymap.invoke("<C-j>", { mode = "i" })
  state.session = picker_owner
  eq(deleted_session_id, "gamma", { fail_reason = "Ctrl-j should delete the filtered session" })
  eq(last_request("session.delete").session_id, "active",
    { fail_reason = "deletion must retain the picker owner's context instead of the deletion target" })
  assert(picker.is_open("sessions"), "deletion should keep the session search open")

  active_picker = picker._state_for_test()
  buffer.set(active_picker.search_buf, { "beta" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  local original_tab = vim.api.nvim_get_current_tabpage()
  local original_tab_count = #vim.api.nvim_list_tabpages()
  keymap.invoke("<CR>", { mode = "i" })
  assert(not picker.is_open("sessions"), "resuming should close the session search")
  eq(state.session.id, "beta", { fail_reason = "Enter should resume the selected same-repository session" })
  eq(vim.api.nvim_get_current_tabpage(), original_tab)
  eq(#vim.api.nvim_list_tabpages(), original_tab_count,
    { fail_reason = "resuming a session should not create a tab" })
  eq(vim.api.nvim_win_get_buf(state.transcript_win), original_transcript_buf,
    { fail_reason = "closing session search should restore the real Harness timeline buffer" })
end

return T
