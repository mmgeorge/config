vim.loader.enable(false)

local forge = require("forge")
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local layout = require("forge.views.harness.layout")
local session = require("forge.session")
local session_picker = require("forge.views.harness.session_picker")

local function interaction(session_id, prompt)
  local interaction_id = session_id .. ":interaction"
  local turn_id = interaction_id .. ":turn"
  local message_id = turn_id .. ":message"
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
      message = { { id = message_id, kind = "assistant", delivery = "final", text = "Timeline preview for " .. prompt } },
      item = { { kind = "message", id = message_id } },
    } },
  }
end

local function snapshot(entry)
  local record = interaction(entry.id, entry.name == "" and "[unnamed]" or entry.name)
  return {
    session = entry,
    exchange = { record },
    timeline = { { kind = "exchange", id = record.id, created_at_ms = 1, exchange = record } },
    artifact = {},
    approval = {},
    capability = {},
    prompt_history = {},
    agent = { definition = {}, run = {}, turn = {} },
  }
end

forge.setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
local state = session.harness
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win = layout.open()
layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
controller.attach()

local workspace = vim.fn.getcwd()
local entry_list = {
  { id = "active", name = "Architecture review", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "streaming", name = "Streaming renderer", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "picker", name = "Picker polish", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "gamma", name = "Gamma migration", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "plans", name = "Plan review", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "agents", name = "Agent timeline", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "approvals", name = "Approval flow", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "rollback", name = "Rollback checks", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "unnamed", name = "", workspace = workspace, backend = "mock", approval_mode = "ask" },
  { id = "remote", name = "Remote repository", workspace = workspace .. "-remote", backend = "mock", approval_mode = "ask" },
}

state.session = entry_list[1]
state.interaction = {}
state.timeline = {}
state.agent = { definition = {}, run = {}, turn = {} }

local deleted_session_id = nil
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

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

  if method == "session.provider.model.list" then
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
