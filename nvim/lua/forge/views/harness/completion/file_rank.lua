local M = {}

---@type table<string, table<string, integer>>
local selection_by_workspace = {}
local selection_sequence = 0

---Record accepted file selections within this editor lifetime and workspace.
---@param root string
---@param path string
function M.record(root, path)
  root = vim.fs.normalize(root)
  selection_sequence = selection_sequence + 1
  selection_by_workspace[root] = selection_by_workspace[root] or {}
  selection_by_workspace[root][path] = selection_sequence
end

---@param name string
---@param query string
---@return integer
local function filename_quality(name, query)
  if query == "" or name == query then return 0 end
  if name:match("^(.+)%.[^.]+$") == query then return 1 end
  if name:sub(1, #query) == query then return 2 end
  if name:find(query, 1, true) then return 4 end
  if #vim.fn.matchfuzzy({ name }, query) > 0 then return 6 end
  return 8
end

---@param left number[]
---@param right number[]
---@return boolean?
local function compare_rank(left, right)
  for index = 1, #left do
    if left[index] ~= right[index] then return left[index] < right[index] end
  end
end

---@param directory string[]
---@param query string[]
---@param filename integer
---@param trailing boolean
---@return number[]
local function directory_rank(directory, query, filename, trailing)
  local best = { 4, filename, 0 }
  if filename == 8 then return best end
  for last = #directory, 1, -1 do
    local cursor = last
    local exact = true
    local skipped = #directory - last
    local matched = true
    for index = #query, 1, -1 do
      local segment = query[index]
      while cursor > 0 and directory[cursor]:sub(1, #segment) ~= segment do
        cursor = cursor - 1
        skipped = skipped + 1
      end
      if cursor == 0 then matched = false break end
      exact = exact and directory[cursor] == segment
      cursor = cursor - 1
    end
    if matched then
      local tier = skipped == 0 and (exact and 1 or 2) or 3
      local rank = trailing and { tier, skipped, exact and 0 or 1 } or { tier, filename, skipped }
      if compare_rank(rank, best) then best = rank end
    end
  end
  return best
end

---Create a fresh query-aware comparator for Blink's current filtering pass.
---@return (string|fun(left: table, right: table): boolean?)[]
function M.sorts()
  local prefix = vim.api.nvim_get_current_line():sub(1, vim.api.nvim_win_get_cursor(0)[2])
  local query = prefix:match("@([^%s@]*)$")
  if vim.bo.filetype ~= "ForgeHarnessInput" or query == nil then return { "score", "sort_text" } end
  query = query:gsub("\\", "/")
  local folded_query = vim.fn.tolower(query)
  local qualified = query:find("/", 1, true) ~= nil
  local trailing = query:sub(-1) == "/"
  local query_parts = vim.split(folded_query, "/", { plain = true, trimempty = true })
  local filename_query = trailing and "" or table.remove(query_parts) or ""
  local harness = require("forge.session").harness
  local root = vim.fs.normalize(harness.session and harness.session.workspace or vim.fn.getcwd())
  local selections = selection_by_workspace[root] or {}
  local open = {}
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.bo[buffer].buflisted then
      open[vim.fs.normalize(vim.api.nvim_buf_get_name(buffer))] = true
    end
  end
  local ranks = {}

  ---@param item table
  ---@return number[]
  local function rank(item)
    if ranks[item] then return ranks[item] end
    local path = item.label:sub(2):gsub("\\", "/")
    local folded = vim.fn.tolower(path)
    local parts = vim.split(folded, "/", { plain = true })
    local filename = table.remove(parts)
    local quality = filename_quality(filename, filename_query)
    local result
    if query == "" then
      result = { 0, 0, 0 }
    elseif folded == folded_query then
      result = { -1, 0, 0 }
    elseif qualified then
      result = directory_rank(parts, query_parts, quality, trailing)
    else
      for _, directory in ipairs(parts) do
        if directory == folded_query then quality = math.min(quality, 3)
        elseif directory:sub(1, #folded_query) == folded_query then quality = math.min(quality, 5)
        elseif directory:find(folded_query, 1, true) then quality = math.min(quality, 7) end
      end
      result = { quality, 0, 0 }
    end
    -- Prefix depth must not dilute an exact filename or trailing-directory match.
    local fuzzy_score = 0
    if result[1] >= 4 and query ~= "" then
      local target = (qualified or quality >= 7) and folded or filename
      local scores = vim.fn.matchfuzzypos({ target }, folded_query)[3]
      fuzzy_score = scores[1] or 0
    end
    vim.list_extend(result, {
      -fuzzy_score,
      path:find(query, 1, true) and 0 or 1,
      -(selections[path] or 0),
      open[vim.fs.joinpath(root, path)] and 0 or 1,
      #path,
    })
    ranks[item] = result
    return result
  end

  return {
    function(left, right)
      local left_file = left.source_id == "harness_files"
      local right_file = right.source_id == "harness_files"
      if left_file ~= right_file then return left_file end
      if not left_file then return nil end
      local ordered = compare_rank(rank(left), rank(right))
      if ordered ~= nil then return ordered end
      return left.label < right.label
    end,
    "score",
    "sort_text",
  }
end

return M
