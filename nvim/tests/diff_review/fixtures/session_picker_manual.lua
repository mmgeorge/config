vim.loader.enable(false)

local diff_review = require("diff_review")
local client = require("diff_review.harness.client")
local controller = require("diff_review.views.harness.controller")
local layout = require("diff_review.views.harness.layout")
local session = require("diff_review.session")
local session_picker = require("diff_review.views.harness.session_picker")

local function interaction(session_id, prompt)
  local interaction_id = session_id .. ":interaction"
  return {
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
          response = "Timeline preview for " .. prompt,
        },
      },
    },
  }
end

local function snapshot(entry)
  local record = interaction(entry.id, entry.name == "" and "[unnamed]" or entry.name)
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

diff_review.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
local state = session.harness
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
controller.attach()

local workspace = vim.fn.getcwd()
local entry_list = {
  { id = "active", name = "Architecture review", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "streaming", name = "Streaming renderer", workspace = workspace, backend = "mock", execution_mode = "write" },
  { id = "picker", name = "Picker polish", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "gamma", name = "Gamma migration", workspace = workspace, backend = "mock", execution_mode = "write" },
  { id = "plans", name = "Plan review", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "agents", name = "Agent timeline", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "approvals", name = "Approval flow", workspace = workspace, backend = "mock", execution_mode = "write" },
  { id = "rollback", name = "Rollback checks", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "unnamed", name = "", workspace = workspace, backend = "mock", execution_mode = "read" },
  { id = "remote", name = "Remote repository", workspace = workspace .. "-remote", backend = "mock", execution_mode = "read" },
}

state.session = entry_list[1]
state.interaction = {}
state.timeline = {}
state.agent = { definition = {}, run = {}, turn = {} }

local deleted_session_id = nil
client.request = function(method, params, callback)
  if method == "session.list" then
    local result = vim.tbl_filter(function(entry)
      return entry.id ~= deleted_session_id and (params.scope == "all" or entry.workspace == workspace)
    end, entry_list)
    callback(vim.deepcopy(result))
    return
  end

  if method == "session.preview" or method == "session.resume" then
    local entry = vim.iter(entry_list):find(function(candidate) return candidate.id == params.session_id end)
    callback(snapshot(vim.deepcopy(entry)))
    return
  end

  if method == "session.delete" then
    deleted_session_id = params.session_id
    callback({ deleted = params.session_id })
    return
  end

  if method == "backend.models" then
    callback({})
    return
  end

  error("unexpected manual picker request: " .. method)
end

session_picker.open({
  transcript_win = state.transcript_win,
  composer_win = state.composer_win,
  window_list = { state.transcript_win, state.composer_win },
  control_win = state.composer_win,
})
