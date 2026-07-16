vim.loader.enable(false)

local diff_review = require("diff_review")
local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local layout = require("diff_review.views.harness.layout")
local picker = require("diff_review.views.picker")
local session = require("diff_review.session")
local session_picker = require("diff_review.views.harness.session_picker")

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function interaction(session_id, prompt)
  local interaction_id = session_id .. ":interaction"
  local record = {
    id = interaction_id,
    session_id = session_id,
    ordinal = 1,
    prompt = prompt,
    state = "complete",
    created_at_ms = 1,
    completed_at_ms = 2,
    duration_ms = 1,
    token_count = 10,
    node_list = {
      {
        kind = "main_segment",
        segment = {
          id = interaction_id .. ":segment",
          state = "complete",
          started_at_ms = 1,
          completed_at_ms = 2,
          duration_ms = 1,
          token_count = 10,
          spawned_agent_count = 0,
          thought = {},
          response = "Preview response for " .. prompt,
        },
      },
    },
  }
  return record
end

local function snapshot(entry)
  local record = interaction(entry.id, "Preview " .. (entry.name == "" and "unnamed" or entry.name))
  return {
    session = entry,
    interaction = { record },
    timeline = { { kind = "interaction", id = record.id, created_at_ms = 1, interaction = record } },
    artifact = {},
    approval = {},
    capability = {},
    prompt_history = {},
    agent = { definition = {}, run = {}, turn = {} },
  }
end

local ok, failure = pcall(function()
  diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
  local state = session.harness
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  controller.attach()

  local workspace = vim.fn.getcwd()
  local entry_list = {
    { id = "active", name = "Architecture review", workspace = workspace, backend = "mock", execution_mode = "read" },
    { id = "beta", name = "Beta cleanup", workspace = workspace, backend = "mock", execution_mode = "write" },
    { id = "gamma", name = "Gamma migration", workspace = workspace, backend = "mock", execution_mode = "read" },
    { id = "remote", name = "Remote review", workspace = workspace .. "-other", backend = "mock", execution_mode = "read" },
  }
  state.session = entry_list[1]
  state.interaction = {}
  state.timeline = {}
  state.agent = { definition = {}, run = {}, turn = {} }
  local original_transcript_buf = state.transcript_buf
  local request_list = {}
  local deleted_session_id = nil
  local defer_preview = false
  local preview_callback_by_session = {}
  local original_request = client.request
  local function last_request(method)
    for request_index = #request_list, 1, -1 do
      if request_list[request_index].method == method then return request_list[request_index] end
    end
    return nil
  end
  client.request = function(method, params, callback)
    request_list[#request_list + 1] = { method = method, params = vim.deepcopy(params) }
    if method == "session.list" then
      local result = vim.tbl_filter(function(entry)
        return not deleted_session_id or entry.id ~= deleted_session_id
      end, entry_list)
      if params.scope == "repo" then
        result = vim.tbl_filter(function(entry) return entry.workspace == workspace end, result)
      end
      callback(vim.deepcopy(result))
    elseif method == "session.preview" then
      local entry = vim.iter(entry_list):find(function(candidate) return candidate.id == params.session_id end)
      if defer_preview then
        preview_callback_by_session[params.session_id] = function()
          callback(snapshot(vim.deepcopy(entry)))
        end
      else
        callback(snapshot(vim.deepcopy(entry)))
      end
    elseif method == "session.delete" then
      deleted_session_id = params.session_id
      callback({ deleted = params.session_id })
    elseif method == "session.resume" then
      local entry = vim.iter(entry_list):find(function(candidate) return candidate.id == params.session_id end)
      callback(snapshot(vim.deepcopy(entry)))
    elseif method == "backend.models" then
      callback({})
    else
      error("unexpected request: " .. method)
    end
  end

  session_picker.open({
    transcript_win = state.transcript_win,
    composer_win = state.composer_win,
    window_list = { state.transcript_win, state.composer_win },
    control_win = state.composer_win,
  })
  assert_true(picker.is_open("sessions"), "session search picker should open")
  assert_equals(request_list[1].params.scope, "repo", "session search should default to the current repository")
  local active_picker = picker._state_for_test()
  assert_equals(vim.api.nvim_get_current_win(), active_picker.search_win, "session search should focus its input")
  assert_true(vim.api.nvim_win_get_buf(state.transcript_win) ~= original_transcript_buf,
    "session search should display a separate preview buffer")
  local preview_text = table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(state.transcript_win), 0, -1, false), "\n")
  assert_true(preview_text:find("Preview Architecture review", 1, true) ~= nil,
    "initial selection should preview the active session timeline")
  local live_record = interaction("active", "Live update while preview is open")
  state.interaction = { live_record }
  state.timeline = { { kind = "interaction", id = live_record.id, created_at_ms = 1, interaction = live_record } }
  controller.render(true)
  assert_true(table.concat(vim.api.nvim_buf_get_lines(original_transcript_buf, 0, -1, false), "\n")
      :find("Live update while preview is open", 1, true) ~= nil,
    "background Harness renders should continue updating the hidden real transcript")
  preview_text = table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(state.transcript_win), 0, -1, false), "\n")
  assert_true(preview_text:find("Preview Architecture review", 1, true) ~= nil,
    "background Harness renders must not overwrite the visible session preview")
  vim.fn.maparg("<C-j>", "i", false, true).callback()
  assert_equals(deleted_session_id, nil, "Ctrl-j should refuse to delete the active session")
  assert_true(picker.is_open("sessions"), "refusing active-session deletion should keep search open")

  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  preview_text = table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(state.transcript_win), 0, -1, false), "\n")
  assert_true(preview_text:find("Preview Gamma migration", 1, true) ~= nil,
    "fuzzy filtering should preview the selected matching session")

  defer_preview = true
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "beta" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  preview_callback_by_session.gamma()
  preview_callback_by_session.beta()
  preview_text = table.concat(vim.api.nvim_buf_get_lines(vim.api.nvim_win_get_buf(state.transcript_win), 0, -1, false), "\n")
  assert_true(preview_text:find("Preview Gamma migration", 1, true) ~= nil,
    "an older asynchronous preview must not replace the latest selection")
  defer_preview = false

  vim.fn.maparg("<C-o>", "i", false, true).callback()
  assert_equals(last_request("session.list").params.scope, "all", "Ctrl-o should reload all repositories")
  assert_equals(vim.api.nvim_buf_get_lines(picker._state_for_test().search_buf, 0, 1, false)[1], "gamma",
    "scope changes should preserve the active search query")
  active_picker = picker._state_for_test()
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "remote" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  vim.fn.maparg("<CR>", "i", false, true).callback()
  assert_true(picker.is_open("sessions"), "cross-repository history should preview without resuming")
  assert_equals(state.session.id, "active", "cross-repository Enter should preserve the active session")
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "gamma" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  vim.fn.maparg("<C-j>", "i", false, true).callback()
  assert_equals(deleted_session_id, "gamma", "Ctrl-j should delete the filtered session")
  assert_true(picker.is_open("sessions"), "deletion should keep the session search open")

  active_picker = picker._state_for_test()
  vim.api.nvim_buf_set_lines(active_picker.search_buf, 0, -1, false, { "beta" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active_picker.search_buf })
  vim.fn.maparg("<CR>", "i", false, true).callback()
  assert_true(not picker.is_open("sessions"), "resuming should close the session search")
  assert_equals(state.session.id, "beta", "Enter should resume the selected same-repository session")
  assert_equals(vim.api.nvim_win_get_buf(state.transcript_win), original_transcript_buf,
    "closing session search should restore the real Harness timeline buffer")

  client.request = original_request
  vim.cmd("qa!")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
