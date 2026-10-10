local M = {}

---@param sequence ForgeBlockSequence
---@param changed table<string, boolean>
---@param retired table<string, boolean>
---@param state? table
function M.validate(sequence, changed, retired, state)
  local incoming, affected = {}, {}
  local function add_children(id)
    for owner in pairs(state and state.node_child and state.node_child[id] or {}) do
      if not retired[owner] then affected[owner] = true end
    end
  end
  for owner in pairs(changed) do
    affected[owner] = true
    local node = sequence.node[owner].entry.metadata.node
    if node and node ~= vim.NIL then
      assert(not incoming[node.id], "duplicate document node identity")
      local previous = state and state.node_owner[node.id]
      assert(not previous or previous == owner or changed[previous] or retired[previous], "duplicate document node identity")
      incoming[node.id] = owner
      add_children(node.id)
    end
  end
  for _, set in ipairs({ changed, retired }) do
    for owner in pairs(set) do
      local previous = state and state.block_node[owner]
      if previous then add_children(previous) end
      for id in pairs(state and state.fold and state.fold.endpoint[owner] or {}) do
        local record = state.fold.record[id]
        if not retired[record.owner] then
          affected[record.owner] = true
          local node = sequence.node[record.owner].entry.metadata.node
          if node and node ~= vim.NIL then add_children(node.id) end
        end
      end
    end
  end
  local function resolve(id)
    local owner = incoming[id] or state and state.node_owner[id]
    local entry = owner and not retired[owner] and sequence.node[owner]
    local node = entry and entry.entry.metadata.node
    assert(node and node ~= vim.NIL and node.id == id, "document node parent is absent: " .. id)
    return owner, entry.entry, node
  end
  local function endpoint(owner, entry, id)
    for _, fold in ipairs(entry.metadata.fold or {}) do
      if fold.id == id then
        local _, row = sequence:position(fold["end"].block)
        assert(row, "node endpoint is absent")
        return row + fold["end"].position.row, fold["end"].position.column
      end
    end
    local _, row = sequence:position(owner)
    return row + entry.row_count, 0
  end
  for owner in pairs(affected) do
    local entry = sequence.node[owner] and sequence.node[owner].entry
    local node = entry and entry.metadata.node
    if node and node ~= vim.NIL then
      local seen, parent = { [node.id] = true }, node.parent
      local position = sequence:position(owner)
      local end_row, end_column = endpoint(owner, entry, node.id)
      while parent and parent ~= vim.NIL do
        assert(not seen[parent], "document node parent cycle")
        seen[parent] = true
        local parent_owner, parent_entry, parent_node = resolve(parent)
        assert(sequence:position(parent_owner) < position, "document node parent must precede child")
        for _, fold in ipairs(parent_entry.metadata.fold or {}) do
          if fold.id == parent then
            local parent_row, parent_column = endpoint(parent_owner, parent_entry, parent)
            assert(end_row < parent_row or end_row == parent_row and end_column <= parent_column,
              "document node extends beyond parent fold: " .. node.id)
          end
        end
        parent = parent_node.parent
      end
    end
  end
end

---@param session table
---@param prepared table
---@param replace_all boolean
function M.update(session, prepared, replace_all)
  if replace_all then session.node_owner, session.block_node, session.node_child, session.node_parent = {}, {}, {}, {} end
  session.node_child, session.node_parent = session.node_child or {}, session.node_parent or {}
  for _, changed in ipairs({ prepared.changed, prepared.retired }) do
    for id in pairs(changed) do
      local node = session.block_node[id]
      local parent = node and session.node_parent[node]
      if parent and session.node_child[parent] then
        session.node_child[parent][id] = nil
        if not next(session.node_child[parent]) then session.node_child[parent] = nil end
      end
      if node then session.node_parent[node] = nil end
      if node and session.node_owner[node] == id then session.node_owner[node] = nil end
      session.block_node[id] = nil
    end
  end
  for id in pairs(prepared.changed) do
    local node = prepared.block[id].metadata.node
    if node and node ~= vim.NIL then
      session.node_owner[node.id], session.block_node[id] = id, node.id
      if node.parent and node.parent ~= vim.NIL then
        session.node_parent[node.id] = node.parent
        session.node_child[node.parent] = session.node_child[node.parent] or {}
        session.node_child[node.parent][id] = true
      end
    end
  end
end

return M
