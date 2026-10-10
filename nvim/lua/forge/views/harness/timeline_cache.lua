local M = {}
local output_metatable = {
  __index = function(tool, field)
    if field ~= "output" then return nil end
    local cached = rawget(tool, "output_materialized")
    if cached then return cached end
    local chunk, text = rawget(tool, "output_chunk"), {}
    while chunk do text[#text + 1], chunk = chunk.text, chunk.previous end
    local ordered = {}
    for index = #text, 1, -1 do ordered[#ordered + 1] = text[index] end
    cached = table.concat(ordered)
    rawset(tool, "output_materialized", cached)
    return cached
  end,
}

---@class ForgeHarnessTimelinePatch
---@field session_id string
---@field base_revision integer
---@field revision integer
---@field operation ForgeHarnessTimelineOperation[]

---@class ForgeHarnessTimelineOperation
---@field kind "insert"|"replace"|"remove"|"tool_output"|"message"
---@field index integer
---@field id string?
---@field entry table?
---@field entry_id string?
---@field call_id string?
---@field delta string?

local function transform_entry(entry, transform)
  if entry.kind == "exchange" then
    local exchange = transform(entry.exchange)
    if exchange then return vim.tbl_extend("force", {}, entry, { exchange = exchange }) end
  elseif entry.kind == "agent_lifecycle" then
    for index, exchange in ipairs(entry.exchange or {}) do
      local replacement = transform(exchange)
      if replacement then
        local exchange_list = vim.list_extend({}, entry.exchange)
        exchange_list[index] = replacement
        return vim.tbl_extend("force", {}, entry, { exchange = exchange_list })
      end
    end
  end
  for _, field in ipairs({ "agent", "agent_by_id" }) do
    for id, agent in pairs(entry[field] or {}) do
      local replacement = transform_entry(agent, transform)
      if replacement then
        local agent_map = vim.tbl_extend("force", {}, entry[field])
        agent_map[id] = replacement
        return vim.tbl_extend("force", {}, entry, { [field] = agent_map })
      end
    end
  end
end

local function transform_turn(exchange, transform)
  for index, turn in ipairs(exchange.turn or {}) do
    local replacement = transform(turn)
    if replacement then
      local next_exchange = vim.tbl_extend("force", {}, exchange)
      next_exchange.turn = vim.list_extend({}, exchange.turn)
      next_exchange.turn[index] = replacement
      return next_exchange
    end
  end
end

local function append_output(entry, call_id, delta)
  return transform_entry(entry, function(exchange)
    return transform_turn(exchange, function(turn)
      for id, tool in pairs(turn.tool and turn.tool.item or {}) do
        if turn.id .. ":" .. id == call_id then
          local next_turn = vim.tbl_extend("force", {}, turn)
          next_turn.tool = vim.tbl_extend("force", {}, turn.tool)
          next_turn.tool.item = vim.tbl_extend("force", {}, turn.tool.item)
          local replacement = vim.tbl_extend("force", {}, tool)
          local previous = rawget(tool, "output_chunk") or { text = rawget(tool, "output") or "" }
          replacement.output, replacement.output_materialized = nil, nil
          replacement.output_chunk = { text = delta, previous = previous }
          next_turn.tool.item[id] = setmetatable(replacement, output_metatable)
          return next_turn
        end
      end
    end)
  end)
end

local function replace_message(entry, operation)
  return transform_entry(entry, function(exchange)
    if exchange.id ~= operation.exchange_id then return end
    return transform_turn(exchange, function(turn)
      if turn.id ~= operation.turn_id then return end
      for index, message in ipairs(turn.message or {}) do
        if message.id == operation.message.id then
          local replacement = vim.tbl_extend("force", {}, turn)
          replacement.message = vim.list_extend({}, turn.message)
          replacement.message[index] = vim.deepcopy(operation.message)
          return replacement
        end
      end
    end)
  end)
end

local function trace(state, event, detail)
  local record = vim.tbl_extend("force", {
    event = event,
    timestamp_ms = vim.uv.now(),
    session_id = state.session and state.session.id or nil,
    revision = state.timeline_revision,
  }, detail or {})
  require("forge.infra.perf").event("harness", "ui." .. event, record)
end

local function synchronize_status(state)
  local final_entry = state.timeline[#state.timeline]
  if final_entry and final_entry.kind == "status" then
    state.status = vim.deepcopy(final_entry.status)
  else
    state.status = { kind = "idle" }
  end
end

---@param state ForgeHarnessPresentationState
---@param entry_list table[]
---@param revision integer
function M.replace(state, entry_list, revision)
  state.timeline = vim.list_extend({}, entry_list)
  state.timeline_revision = revision
  synchronize_status(state)
  trace(state, "timeline_snapshot_applied", {
    entry_count = #state.timeline,
  })
end

---@param state ForgeHarnessPresentationState
---@param patch ForgeHarnessTimelinePatch
---@return boolean applied
---@return string? error
function M.apply(state, patch)
  local session_id = state.session and state.session.id
  if patch.session_id ~= session_id then
    return false, ("timeline patch belongs to session %s"):format(tostring(patch.session_id))
  end
  if patch.base_revision ~= state.timeline_revision then
    trace(state, "timeline_revision_gap", {
      base_revision = patch.base_revision,
      received_revision = patch.revision,
    })
    return false, ("timeline revision gap: have %s, received base %s")
      :format(tostring(state.timeline_revision), tostring(patch.base_revision))
  end

  local structural = false
  for _, operation in ipairs(patch.operation or {}) do
    if operation.kind == "insert" or operation.kind == "remove" then structural = true break end
  end
  local next_timeline = structural and vim.list_extend({}, state.timeline or {}) or state.timeline
  local replacement_by_index = {}
  for _, operation in ipairs(patch.operation or {}) do
    local lua_index = operation.index + 1
    if operation.kind == "message" then
      local existing = replacement_by_index[lua_index] or next_timeline[lua_index]
      if not existing or existing.id ~= operation.entry_id or type(operation.message) ~= "table" then
        return false, "invalid message operation"
      end
      local replacement = replace_message(existing, operation)
      if not replacement then return false, "message identity is missing" end
      if structural then next_timeline[lua_index] = replacement
      else replacement_by_index[lua_index] = replacement end
    elseif operation.kind == "tool_output" then
      local existing = replacement_by_index[lua_index] or next_timeline[lua_index]
      if not existing or existing.id ~= operation.entry_id or type(operation.delta) ~= "string" then
        return false, "invalid tool output operation"
      end
      local replacement = append_output(existing, operation.call_id, operation.delta)
      if not replacement then return false, "tool output identity is missing" end
      if structural then next_timeline[lua_index] = replacement
      else replacement_by_index[lua_index] = replacement end
    elseif operation.kind == "insert" then
      if lua_index < 1 or lua_index > #next_timeline + 1 or not operation.entry then
        return false, "invalid timeline insert operation"
      end
      table.insert(next_timeline, lua_index, vim.deepcopy(operation.entry))
    elseif operation.kind == "replace" then
      if lua_index < 1 or lua_index > #next_timeline or not operation.entry then
        return false, "invalid timeline replace operation"
      end
      local replacement = vim.deepcopy(operation.entry)
      if structural then next_timeline[lua_index] = replacement
      else replacement_by_index[lua_index] = replacement end
    elseif operation.kind == "remove" then
      local existing = next_timeline[lua_index]
      if not existing or existing.id ~= operation.id then
        return false, "timeline remove identity mismatch"
      end
      table.remove(next_timeline, lua_index)
    else
      return false, ("unknown timeline operation: %s"):format(tostring(operation.kind))
    end
  end

  for index, replacement in pairs(replacement_by_index) do next_timeline[index] = replacement end
  state.timeline = next_timeline
  state.timeline_revision = patch.revision
  synchronize_status(state)
  trace(state, "timeline_patch_applied", {
    base_revision = patch.base_revision,
    operation_count = #(patch.operation or {}),
  })
  return true
end

---@param state { timeline: table[] }
---@return table[]
function M.history(state)
  local entry_list = state.timeline or {}
  if entry_list[#entry_list] and entry_list[#entry_list].kind == "status" then
    return vim.list_slice(entry_list, 1, #entry_list - 1)
  end
  return entry_list
end

local function find_agent(entry_list, run_id)
  for _, entry in ipairs(entry_list or {}) do
    if entry.kind == "agent_lifecycle" then
      if entry.run and entry.run.id == run_id then return entry end
      local nested = find_agent(entry.agent, run_id)
      if nested then return nested end
    end
    for _, attached in pairs(entry.agent_by_id or {}) do
      if attached.run and attached.run.id == run_id then return attached end
      local nested = find_agent(attached.agent, run_id)
      if nested then return nested end
    end
  end
  return nil
end

---@param state ForgeHarnessPresentationState
---@return table[]?
function M.selected_agent_history(state)
  if not state.selected_agent_run_id then return nil end
  local agent_entry = find_agent(M.history(state), state.selected_agent_run_id)
  if not agent_entry then return {} end
  local timeline = {}
  for _, exchange in ipairs(agent_entry.exchange or {}) do
    timeline[#timeline + 1] = {
      kind = "exchange",
      id = exchange.id,
      created_at_ms = exchange.created_at_ms,
      exchange = exchange,
      agent_by_id = {},
    }
  end
  return timeline
end

---@param state ForgeHarnessPresentationState
---@param run_id string
---@return table[]
function M.agent_exchange_list(state, run_id)
  local agent_entry = find_agent(M.history(state), run_id)
  return agent_entry and agent_entry.exchange or {}
end

return M
