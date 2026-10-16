local source = require("forge.lsp.source")
local snapshots = require("forge.lsp.snapshot")
local M = {}

---@class ForgeLspTarget
---@field path? string
---@field symbol? string
---@field line? integer
---@field column? integer

---@param snapshot ForgeLspSnapshot
---@param declaration table
---@return string
function M.name(snapshot, declaration)
  local names, parent = { declaration.name }, declaration.parent
  while parent ~= nil and parent ~= vim.NIL do
    local ancestor = snapshot.declarations[parent + 1]
    table.insert(names, 1, ancestor.name)
    parent = ancestor.parent
  end
  return table.concat(names, snapshot.path:match("%.rs$") and "::" or ".")
end

local function leaf(name) return name:match("([^:.]+)$") or name end

local function matches(snapshot, declaration, symbol)
  return declaration.kind ~= "impl_item" and (declaration.name == symbol or M.name(snapshot, declaration) == symbol)
end

local function display(workspace, path)
  path = source.canonical(path)
  if path:sub(1, #workspace + 1):lower() == (workspace .. "/"):lower() then return path:sub(#workspace + 2) end
  return path
end

local function ambiguity(workspace, candidates)
  local width, lines = 0, { "ambiguous_symbol: " .. #candidates .. " matches" }
  table.sort(candidates, function(left, right)
    return left.path .. string.format(":%09d:%09d", left.line, left.column) < right.path .. string.format(":%09d:%09d", right.line, right.column)
  end)
  for index = 1, math.min(10, #candidates) do width = math.max(width, math.min(256, vim.fn.strchars(candidates[index].symbol))) end
  for index = 1, math.min(10, #candidates) do
    local candidate = candidates[index]
    local name = vim.fn.strcharpart(candidate.symbol, 0, 256)
    lines[#lines + 1] = "  " .. name .. string.rep(" ", width - vim.fn.strchars(name) + 2)
      .. display(workspace, candidate.path) .. ":" .. candidate.line .. ":" .. candidate.column
  end
  if #candidates > 10 then lines[#lines + 1] = "  " .. (#candidates - 10) .. " additional matches omitted." end
  return table.concat(lines, "\n")
end

local function word(character)
  return character ~= "" and (character:match("[%w_]") ~= nil or character:byte() >= 128)
end

---@param workspace string
---@param target ForgeLspTarget
---@param get function
---@param query function
---@param receive fun(target: ForgeLspTarget?, failure: string?)
function M.resolve(workspace, target, get, query, receive)
  workspace = source.canonical(workspace)
  local function select(candidates)
    if #candidates == 0 then
      local location = target.path and display(workspace, target.path)
      receive(nil, "symbol_not_found: " .. tostring(target.symbol) .. (location and (target.line and " at " .. location .. ":" .. target.line or " in " .. location) or ""))
    elseif #candidates > 1 then receive(nil, ambiguity(workspace, candidates))
    else receive(candidates[1]) end
  end
  if target.path and target.line then
    get(target.path, function(snapshot)
      local line = snapshot.lines[target.line]
      if not line then receive(nil, "invalid_position: " .. display(workspace, target.path) .. ":" .. target.line .. " is outside the source.") return end
      local symbol = target.symbol and leaf(target.symbol)
      if not symbol then
        if target.column > vim.fn.strchars(line) + 1 then receive(nil, "invalid_position: " .. display(workspace, target.path) .. ":" .. target.line .. ":" .. target.column .. " is outside the source.")
        else receive(target) end
        return
      end
      local candidates, offset = {}, 1
      while true do
        local first, last = line:find(symbol, offset, true)
        if not first then break end
        offset = last + 1
        if not word(line:sub(first - 1, first - 1)) and not word(line:sub(last + 1, last + 1)) then
          local column = vim.str_utfindex(line, "utf-32", first - 1, false) + 1
          local ending = column + vim.fn.strchars(symbol)
          if not target.column or target.column >= column and target.column < ending then
            candidates[#candidates + 1] = { path = snapshot.path, line = target.line, column = column, symbol = symbol }
          end
        end
      end
      if #candidates == 0 and target.column then receive(nil, "target_mismatch: " .. target.symbol .. " does not match " .. display(workspace, target.path) .. ":" .. target.line .. ":" .. target.column) return end
      if #candidates ~= 1 or target.symbol == symbol then select(candidates) return end
      query({ operation = "definition", path = snapshot.path, line = target.line, column = candidates[1].column, limit = 50 }, function(reply)
        local index, found, actual = 1, false, {}
        local function next_definition()
          local row = reply.rows[index]
          index = index + 1
          if not row then
            if found then receive(candidates[1])
            elseif #actual > 0 then receive(nil, "target_mismatch: expected " .. target.symbol .. ", found " .. table.concat(actual, ", ") .. " at " .. display(workspace, snapshot.path) .. ":" .. target.line .. ":" .. candidates[1].column)
            else receive(nil, "target_mismatch: " .. target.symbol .. " has no matching definition at " .. display(workspace, snapshot.path) .. ":" .. target.line .. ":" .. candidates[1].column) end
            return
          end
          get(row.path, function(definition)
            local declaration = snapshots.enclosing(definition, row.line, row.column)
            if declaration then actual[#actual + 1] = M.name(definition, declaration) end
            found = found or declaration ~= nil and matches(definition, declaration, target.symbol)
            next_definition()
          end)
        end
        next_definition()
      end)
    end)
    return
  end
  local candidates, seen = {}, {}
  local function inspect(snapshot, row)
    if not snapshot.complete then return false end
    for _, declaration in ipairs(snapshot.declarations) do
      if (not row or row.line == declaration.line and row.column == declaration.column) and matches(snapshot, declaration, target.symbol) then
        local key = snapshot.path .. ":" .. declaration.line .. ":" .. declaration.column
        if not seen[key] then
          seen[key] = true
          candidates[#candidates + 1] = { path = snapshot.path, line = declaration.line, column = declaration.column, symbol = M.name(snapshot, declaration) }
        end
      end
    end
    return true
  end
  if target.path then
    get(target.path, function(snapshot)
      if not inspect(snapshot) then receive(nil, "partial: Symbol lookup incomplete. Structural analysis unavailable for " .. display(workspace, target.path) .. ".") else select(candidates) end
    end)
  else
    local page, incomplete = 0, false
    local function next_page()
      query({ operation = "workspace_symbols", query = leaf(target.symbol), limit = 50, offset = page * 50, scope = "workspace" }, function(reply)
        if reply.warnings and #reply.warnings > 0 then incomplete = true end
        local index = 1
        local function next_row()
          local row = reply.rows[index]
          index = index + 1
          if not row then
            if reply.next_offset then
              page = page + 1
              if page < 5 then next_page() else receive(nil, "partial: Symbol lookup incomplete. Search limit reached.") end
            elseif incomplete then receive(nil, "partial: Symbol lookup incomplete. Some candidate declarations could not be resolved.")
            else select(candidates) end
            return
          end
          get(row.path, function(snapshot)
            if not inspect(snapshot, row) then incomplete = true end
            next_row()
          end)
        end
        next_row()
      end)
    end
    next_page()
  end
end

return M
