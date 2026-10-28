local M = {}

---Keep confirmed state private and recompute predictions after every acknowledgement.
---@param initial table
---@param revision integer
---@param project fun(value: table, intent: table)
---@return table
function M.new(initial, revision, project)
  local confirmed, current_revision = vim.deepcopy(initial), revision
  local pending, recovering = {}, false
  local replica = {}
  function replica.view()
    local value = vim.deepcopy(confirmed)
    for _, operation in ipairs(pending) do project(value, operation.intent) end
    return value
  end
  function replica.begin(id, intent)
    for _, operation in ipairs(pending) do assert(operation.id ~= id, "duplicate optimistic operation") end
    pending[#pending + 1] = { id = id, intent = vim.deepcopy(intent) }
  end
  function replica.remove(id)
    for index, operation in ipairs(pending) do
      if operation.id == id then table.remove(pending, index) return operation.intent end
    end
  end
  function replica.replace(value, next_revision)
    if next_revision < current_revision then return false end
    confirmed, current_revision, recovering = vim.deepcopy(value), next_revision, false
    return true
  end
  function replica.patch(base_revision, next_revision, update)
    if next_revision <= current_revision then return false end
    if recovering or base_revision ~= current_revision then recovering = true return false end
    local next_value = vim.deepcopy(confirmed)
    update(next_value)
    confirmed, current_revision = next_value, next_revision
    return true
  end
  function replica.pending() return #pending end
  function replica.needs_snapshot() return recovering end
  return replica
end

return M
