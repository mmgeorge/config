--- Computes the per-diff change inventory: the added, removed, and modified code symbols
--- (functions, structs, classes, traits, types, modules) a changeset touches, via the
--- bundled `diff_inventory` treesitter queries.
---
--- Caches old and new source lines per repo-relative path, so symbol extraction never
--- re-reads a file or re-runs git for the same path.

local M = {}
require("diff_review.query_runtime")

---@class DiffReviewInventoryCounts
---@field added integer
---@field removed integer
---@field modified integer

---@class DiffReviewInventoryChange
---@field name string
---@field relpath string
---@field line? integer

---@class DiffReviewInventoryChangeSet
---@field added DiffReviewInventoryChange[]
---@field removed DiffReviewInventoryChange[]
---@field modified DiffReviewInventoryChange[]

---@class DiffReviewInventoryRow
---@field label string
---@field added integer
---@field removed integer
---@field modified integer

---@class DiffReviewInventoryResult
---@field rows DiffReviewInventoryRow[]
---@field details table<string, DiffReviewInventoryChangeSet>?
---@field error? string

---@class DiffReviewInventoryFile
---@field filename string
---@field relpath string
---@field original_relpath? string
---@field git_status? string
---@field untracked? boolean
---@field hunks table[]

---@class DiffReviewInventoryComputeOpts
---@field cwd string
---@field sections table[]?
---@field git_list_async fun(command: string[], cb: fun(output: string[], code: integer, stderr?: string))
---@field read_file_lines? fun(filename: string): string[]?
---@field repo_relative? fun(filename: string): string?
---@field old_lines_by_relpath? table<string, string[]>
---@field new_lines_by_relpath? table<string, string[]>

local row_order = {
  "function",
  "struct",
  "class",
  "interface",
  "enum",
  "trait",
  "type",
  "module",
  "docs",
  "plans",
  "files",
}

local row_rank = {}
for index, label in ipairs(row_order) do
  row_rank[label] = index
end

local counted_symbol_kinds = {
  ["function"] = true,
  struct = true,
  class = true,
  interface = true,
  enum = true,
  trait = true,
  type = true,
  module = true,
}

local supported_langs = {
  rust = true,
  typescript = true,
  tsx = true,
}

---@param counts table<string, DiffReviewInventoryCounts>
---@param label string
---@param field "added"|"removed"|"modified"
---@param amount? integer
local function increment(counts, label, field, amount)
  amount = amount or 1
  if amount == 0 then return end
  local row = counts[label]
  if not row then
    row = { added = 0, removed = 0, modified = 0 }
    counts[label] = row
  end
  row[field] = row[field] + amount
end

---@param details table<string, DiffReviewInventoryChangeSet>
---@param label string
---@param action "added"|"removed"|"modified"
---@param change DiffReviewInventoryChange
local function add_detail(details, label, action, change)
  local row = details[label]
  if not row then
    row = { added = {}, removed = {}, modified = {} }
    details[label] = row
  end
  row[action][#row[action] + 1] = change
end

---@param path string?
---@return string
local function normalize_relpath(path)
  return (tostring(path or ""):gsub("\\", "/"):gsub("^%./", ""))
end

---@return "added"|"removed"|"modified"
local function artifact_action_for_status(status, untracked)
  status = tostring(status or "")
  if untracked or status == "??" or status:sub(1, 1) == "A" then
    return "added"
  end
  if status:sub(1, 1) == "D" then
    return "removed"
  end
  return "modified"
end

---@param relpath string
---@return string?
local function artifact_bucket(relpath)
  relpath = normalize_relpath(relpath):lower()
  for segment in relpath:gmatch("[^/]+") do
    if segment == "docs" then return "docs" end
    if segment == "plans" then return "plans" end
  end
  return nil
end

---@param opts DiffReviewInventoryComputeOpts
---@param file table
---@return string
local function file_relpath(opts, file)
  if opts.repo_relative and type(file.filename) == "string" then
    local relpath = opts.repo_relative(file.filename)
    if relpath and relpath ~= "" then return normalize_relpath(relpath) end
  end
  if type(file.relpath) == "string" and file.relpath ~= "" and not file.relpath:match("^%a:[/\\]") then
    return normalize_relpath(file.relpath)
  end
  return normalize_relpath(file.filename)
end

---@param files_by_relpath table<string, DiffReviewInventoryFile>
---@param opts DiffReviewInventoryComputeOpts
---@param file table
local function add_file(files_by_relpath, opts, file)
  if type(file) ~= "table" or not file.filename then return end
  local relpath = file_relpath(opts, file)
  if relpath == "" then return end
  local existing = files_by_relpath[relpath]
  if not existing then
    existing = {
      filename = file.filename,
      relpath = relpath,
      original_relpath = file.original_relpath,
      git_status = file.git_status,
      untracked = file.untracked == true,
      hunks = {},
    }
    files_by_relpath[relpath] = existing
  else
    existing.git_status = existing.git_status or file.git_status
    existing.original_relpath = existing.original_relpath or file.original_relpath
    existing.untracked = existing.untracked or file.untracked == true
  end
  for _, hunk in ipairs(file.hunks or {}) do
    existing.hunks[#existing.hunks + 1] = hunk
  end
end

---@param opts DiffReviewInventoryComputeOpts
---@return DiffReviewInventoryFile[]
local function collect_files(opts)
  local files_by_relpath = {}
  for _, section in ipairs(opts.sections or {}) do
    for _, file in ipairs(section.files or {}) do
      add_file(files_by_relpath, opts, file)
    end
  end

  local files = {}
  for _, file in pairs(files_by_relpath) do
    files[#files + 1] = file
  end
  table.sort(files, function(left, right)
    return left.relpath < right.relpath
  end)
  return files
end

---@param line string
---@return integer? old_start
---@return integer? new_start
local function parse_hunk_header(line)
  local old_start, new_start = line:match("^@@ %-(%d+),?%d* %+(%d+),?%d* @@")
  return tonumber(old_start), tonumber(new_start)
end

---@param set table<integer, boolean>
---@param line integer?
local function mark_line(set, line)
  if line and line > 0 then set[line] = true end
end

---@param changed { old: table<integer, boolean>, new: table<integer, boolean> }
---@param diff_text string?
local function add_changed_lines_from_diff(changed, diff_text)
  local old_line = nil
  local new_line = nil
  for _, raw_line in ipairs(vim.split(tostring(diff_text or ""), "\n", { plain = true })) do
    local line = raw_line:gsub("\r$", "")
    local old_start, new_start = parse_hunk_header(line)
    if old_start and new_start then
      old_line = old_start
      new_line = new_start
    elseif old_line and new_line then
      local prefix = line:sub(1, 1)
      if prefix == "+" and not line:find("^%+%+%+") then
        mark_line(changed.new, new_line)
        new_line = new_line + 1
      elseif prefix == "-" and not line:find("^%-%-%-") then
        mark_line(changed.old, old_line)
        old_line = old_line + 1
      elseif prefix == " " then
        old_line = old_line + 1
        new_line = new_line + 1
      end
    end
  end
end

---@param file DiffReviewInventoryFile
---@param action "added"|"removed"|"modified"
---@param new_lines string[]?
---@return { old: table<integer, boolean>, new: table<integer, boolean> }
local function changed_lines_for_file(file, action, new_lines)
  local changed = { old = {}, new = {} }
  for _, hunk in ipairs(file.hunks or {}) do
    add_changed_lines_from_diff(changed, hunk.diff)
  end
  if action == "added" and next(changed.new) == nil and type(new_lines) == "table" then
    for line_number = 1, #new_lines do
      changed.new[line_number] = true
    end
  end
  return changed
end

---@param filename string
---@param lines string[]?
---@return string?
local function language_for_filename(filename, lines)
  local args = { filename = filename }
  if type(lines) == "table" and #lines > 0 then
    args.contents = lines
  end
  local filetype = vim.filetype.match(args) or ""
  if filetype == "" then return nil end
  local lang = vim.treesitter.language.get_lang(filetype)
  if lang == "typescriptreact" then lang = "tsx" end
  if not lang or not supported_langs[lang] then return nil end
  return lang
end

---@param lines string[]?
---@return boolean
local function has_nul(lines)
  for _, line in ipairs(lines or {}) do
    if tostring(line):find("\0", 1, true) then return true end
  end
  return false
end

---@param opts DiffReviewInventoryComputeOpts
---@param file DiffReviewInventoryFile
---@param action "added"|"removed"|"modified"
---@return string[]?
local function current_lines_for_file(opts, file, action)
  if opts.new_lines_by_relpath and opts.new_lines_by_relpath[file.relpath] then
    return opts.new_lines_by_relpath[file.relpath]
  end
  if action == "removed" then return {} end
  if opts.read_file_lines then
    local lines = opts.read_file_lines(file.filename)
    if type(lines) == "table" and not has_nul(lines) then return lines end
  end
  return nil
end

---@param opts DiffReviewInventoryComputeOpts
---@param file DiffReviewInventoryFile
---@param action "added"|"removed"|"modified"
---@param cb fun(lines: string[]?, err?: string)
local function old_lines_for_file(opts, file, action, cb)
  local old_relpath = normalize_relpath(file.original_relpath or file.relpath)
  if opts.old_lines_by_relpath and opts.old_lines_by_relpath[old_relpath] then
    cb(opts.old_lines_by_relpath[old_relpath])
    return
  end
  if action == "added" then
    cb({})
    return
  end
  opts.git_list_async({ "git", "-C", opts.cwd, "show", "HEAD:" .. old_relpath }, function(output, code, stderr)
    if code ~= 0 then
      cb(nil, vim.trim(stderr or "") ~= "" and vim.trim(stderr or "") or ("git show failed for " .. old_relpath))
      return
    end
    cb(output or {})
  end)
end

---@class DiffReviewInventorySymbol
---@field kind string
---@field name string
---@field key string
---@field start_line integer
---@field end_line integer

---@param query vim.treesitter.Query
---@param buf integer
---@param node TSNode
---@param kind string
---@return string?
local function symbol_name(query, buf, node, kind)
  local exact_capture = "inventory." .. kind .. ".name"
  for capture_id, capture_node in query:iter_captures(node, buf) do
    local capture_name = query.captures[capture_id]
    if capture_name == exact_capture then
      local text = vim.treesitter.get_node_text(capture_node, buf)
      if text and text ~= "" then return text end
    end
  end
  return nil
end

---@param symbols DiffReviewInventorySymbol[]
---@param symbol DiffReviewInventorySymbol
---@return string
local function symbol_key(symbols, symbol)
  local parents = {}
  for _, candidate in ipairs(symbols) do
    if candidate ~= symbol
        and candidate.start_line <= symbol.start_line
        and candidate.end_line >= symbol.end_line
        and (candidate.start_line ~= symbol.start_line or candidate.end_line ~= symbol.end_line) then
      parents[#parents + 1] = candidate
    end
  end
  table.sort(parents, function(left, right)
    if left.start_line ~= right.start_line then return left.start_line < right.start_line end
    return left.end_line > right.end_line
  end)

  local names = {}
  for _, parent in ipairs(parents) do
    names[#names + 1] = parent.name
  end
  names[#names + 1] = symbol.name
  return symbol.kind .. ":" .. table.concat(names, ".")
end

---@param symbol DiffReviewInventorySymbol
---@return string
local function symbol_display_name(symbol)
  local prefix = symbol.kind .. ":"
  if symbol.key:sub(1, #prefix) == prefix then
    return symbol.key:sub(#prefix + 1)
  end
  return symbol.name
end

---@param buf integer
---@param query vim.treesitter.Query
---@param trees TSTree[]
---@return DiffReviewInventorySymbol[]
local function symbols_from_trees(buf, query, trees)
  if not trees or not trees[1] then return {} end
  local raw_symbols = {}
  for capture_id, node in query:iter_captures(trees[1]:root(), buf) do
    local capture_name = query.captures[capture_id]
    local kind = capture_name and capture_name:match("^inventory%.([%w_]+)$") or nil
    if kind then
      local name = symbol_name(query, buf, node, kind)
      if name and name ~= "" then
        local start_row, _, end_row, _ = node:range()
        raw_symbols[#raw_symbols + 1] = {
          kind = kind,
          name = name,
          key = "",
          start_line = start_row + 1,
          end_line = end_row + 1,
        }
      end
    end
  end

  table.sort(raw_symbols, function(left, right)
    if left.start_line ~= right.start_line then return left.start_line < right.start_line end
    return left.end_line > right.end_line
  end)
  for _, symbol in ipairs(raw_symbols) do
    symbol.key = symbol_key(raw_symbols, symbol)
  end
  return raw_symbols
end

---@param filename string
---@param lines string[]?
---@param cb fun(symbols?: DiffReviewInventorySymbol[], supported: boolean)
local function parse_symbols_async(filename, lines, cb)
  local lang = language_for_filename(filename, lines)
  if not lang then
    cb({}, false)
    return
  end

  local query_ok, query = pcall(vim.treesitter.query.get, lang, "diff_inventory")
  if not query_ok or not query then
    cb({}, false)
    return
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines or {})

  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not parser_ok or not parser then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    cb({}, false)
    return
  end

  local done = false
  local function finish(trees)
    if done then return end
    done = true
    local ok, symbols = pcall(symbols_from_trees, buf, query, trees)
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    cb(ok and symbols or {}, true)
  end

  local parse_ok, parsed = pcall(function()
    return parser:parse({ 0, 0, math.max(#(lines or {}), 1), 0 }, function(first, second)
      local trees = type(first) == "table" and first or second
      vim.schedule(function()
        finish(trees)
      end)
    end)
  end)
  if not parse_ok then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    cb({}, false)
  elseif parsed then
    vim.schedule(function()
      finish(parsed)
    end)
  end
end

---@param set table<integer, boolean>
---@param line integer
---@return boolean
local function line_in_set(set, line)
  return set[line] == true
end

---@param child DiffReviewInventorySymbol
---@param parent DiffReviewInventorySymbol
---@return boolean
local function is_strict_child(child, parent)
  return child.start_line >= parent.start_line
    and child.end_line <= parent.end_line
    and (child.start_line ~= parent.start_line or child.end_line ~= parent.end_line)
end

---@param symbol DiffReviewInventorySymbol
---@param changed_set table<integer, boolean>
---@param all_symbols DiffReviewInventorySymbol[]
---@return boolean
local function symbol_has_direct_changed_line(symbol, changed_set, all_symbols)
  for line in pairs(changed_set or {}) do
    if line >= symbol.start_line and line <= symbol.end_line then
      local inside_child = false
      for _, child in ipairs(all_symbols or {}) do
        if child ~= symbol and counted_symbol_kinds[child.kind] and is_strict_child(child, symbol) and line >= child.start_line and line <= child.end_line then
          inside_child = true
          break
        end
      end
      if not inside_child then return true end
    end
  end
  return false
end

---@param symbols DiffReviewInventorySymbol[]
---@return table<string, DiffReviewInventorySymbol>
local function symbol_map(symbols)
  local by_key = {}
  for _, symbol in ipairs(symbols or {}) do
    if counted_symbol_kinds[symbol.kind] then
      by_key[symbol.key] = symbol
    end
  end
  return by_key
end

---@param counts table<string, DiffReviewInventoryCounts>
---@param details table<string, DiffReviewInventoryChangeSet>
---@param file DiffReviewInventoryFile
---@param old_symbols DiffReviewInventorySymbol[]
---@param new_symbols DiffReviewInventorySymbol[]
---@param changed { old: table<integer, boolean>, new: table<integer, boolean> }
local function add_symbol_counts(counts, details, file, old_symbols, new_symbols, changed)
  local old_by_key = symbol_map(old_symbols)
  local new_by_key = symbol_map(new_symbols)
  for key, new_symbol in pairs(new_by_key) do
    local old_symbol = old_by_key[key]
    if not old_symbol then
      increment(counts, new_symbol.kind, "added")
      add_detail(details, new_symbol.kind, "added", {
        name = symbol_display_name(new_symbol),
        relpath = file.relpath,
        line = new_symbol.start_line,
      })
    elseif symbol_has_direct_changed_line(old_symbol, changed.old, old_symbols)
        or symbol_has_direct_changed_line(new_symbol, changed.new, new_symbols) then
      increment(counts, new_symbol.kind, "modified")
      add_detail(details, new_symbol.kind, "modified", {
        name = symbol_display_name(new_symbol),
        relpath = file.relpath,
        line = new_symbol.start_line,
      })
    end
  end
  for key, old_symbol in pairs(old_by_key) do
    if not new_by_key[key] then
      increment(counts, old_symbol.kind, "removed")
      add_detail(details, old_symbol.kind, "removed", {
        name = symbol_display_name(old_symbol),
        relpath = normalize_relpath(file.original_relpath or file.relpath),
        line = old_symbol.start_line,
      })
    end
  end
end

---@param counts table<string, DiffReviewInventoryCounts>
---@return DiffReviewInventoryRow[]
local function rows_from_counts(counts)
  local rows = {}
  for label, count in pairs(counts) do
    if (count.added or 0) ~= 0 or (count.removed or 0) ~= 0 or (count.modified or 0) ~= 0 then
      rows[#rows + 1] = {
        label = label,
        added = count.added or 0,
        removed = count.removed or 0,
        modified = count.modified or 0,
      }
    end
  end
  table.sort(rows, function(left, right)
    local left_rank = row_rank[left.label] or 999
    local right_rank = row_rank[right.label] or 999
    if left_rank ~= right_rank then return left_rank < right_rank end
    return left.label < right.label
  end)
  return rows
end

---@param counts table<string, DiffReviewInventoryCounts>
---@param details table<string, DiffReviewInventoryChangeSet>
---@param file DiffReviewInventoryFile
---@param action "added"|"removed"|"modified"
local function add_artifact_counts(counts, details, file, action)
  increment(counts, "files", action)
  add_detail(details, "files", action, {
    name = file.relpath,
    relpath = file.relpath,
  })
  local bucket = artifact_bucket(file.relpath)
  if bucket then
    increment(counts, bucket, action)
    add_detail(details, bucket, action, {
      name = file.relpath,
      relpath = file.relpath,
    })
  end
end

---@param opts DiffReviewInventoryComputeOpts
---@param cb fun(result: DiffReviewInventoryResult)
function M.compute_async(opts, cb)
  local files = collect_files(opts)
  local counts = {}
  local details = {}
  local pending = #files
  local errors = {}

  if pending == 0 then
    cb({ rows = {}, details = {} })
    return
  end

  local function finish_one()
    pending = pending - 1
    if pending > 0 then return end
    cb({
      rows = rows_from_counts(counts),
      details = details,
      error = #errors > 0 and table.concat(errors, "\n") or nil,
    })
  end

  for _, file in ipairs(files) do
    local action = artifact_action_for_status(file.git_status, file.untracked)
    add_artifact_counts(counts, details, file, action)

    local current_lines = current_lines_for_file(opts, file, action)
    if not current_lines and action ~= "removed" then
      finish_one()
    else
      local semantic_lang = language_for_filename(file.filename, current_lines)
      if not semantic_lang then
        finish_one()
      else
        old_lines_for_file(opts, file, action, function(old_lines, err)
          if err then
            errors[#errors + 1] = err
            finish_one()
            return
          end
          local changed = changed_lines_for_file(file, action, current_lines)
          parse_symbols_async(file.filename, old_lines or {}, function(old_symbols)
            parse_symbols_async(file.filename, current_lines or {}, function(new_symbols)
              add_symbol_counts(counts, details, file, old_symbols or {}, new_symbols or {}, changed)
              finish_one()
            end)
          end)
        end)
      end
    end
  end
end

M._changed_lines_for_diff = function(diff_text)
  local changed = { old = {}, new = {} }
  add_changed_lines_from_diff(changed, diff_text)
  return changed
end

return M
