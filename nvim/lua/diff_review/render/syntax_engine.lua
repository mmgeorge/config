--- Computes treesitter syntax and hunk-context data for the diff renderer: per-file and
--- per-diff syntax segments, context scopes, and prewarm scheduling. Producer layer
--- consumed by the row builders and orchestrators; owns no buffer/render state.
---@class DiffReviewSyntaxEngineModule
local M = {}

local config = require("diff_review.infra.config")
local notifications = require("diff_review.infra.notifications")
local diff_parse = require("diff_review.render.diff_parse")

local util = require("diff_review.infra.util")
local hunk_model = require("diff_review.render.hunk_model")
-- status_render edge kept lazy to avoid a load-time cycle.
local function status_render() return require("diff_review.views.status.status_render") end
-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
-- git_data requires syntax_engine back, so this edge stays lazy to avoid a load-time cycle.
local function git_data() return require("diff_review.git.git_data") end
local trace = require("diff_review.infra.perf_trace")
local session = require("diff_review.session")

-- Seams to init-owned helpers the syntax engine shares.
local parse_unified_diff = diff_parse.parse_unified_diff
local parse_hunk_body = diff_parse.parse_hunk_body
local loaded_file_buffer = util.loaded_file_buffer
local file_contains_nul = util.file_contains_nul
local lines_contain_nul = util.lines_contain_nul
local function status_render_current_model(...) return status_render().status_render_current_model(...) end
local function status_diff_hunks_for_file(...) return render_orchestrator().diff_hunks_for_file(...) end
local hunk_first_changed_current_line = diff_parse.hunk_first_changed_current_line
local detect_filetype = util.detect_filetype
local function notify_error(message, title) return notifications.error(message, title) end

-- Tree-sitter caches owned by the render engine, reset together on a status refresh.
-- `context` keyed "<file>:<line>", `file_syntax` by filename, `diff_syntax` by
-- "<file>:<side>:<sha>", `source_bufs` by filename. The functions below capture these
-- as upvalues, so a clear_* reassignment is visible to every reader at once.
local ts_context_cache = {}
local ts_syntax_cache = {}
local ts_diff_syntax_cache = {}
local ts_source_bufs = {}

--- Clear the per-line hunk-context cache before a status refresh re-resolves scopes.
function M.clear_context_cache()
  ts_context_cache = {}
end

--- Clear the per-diff-side syntax cache, forcing a re-parse.
function M.clear_diff_syntax_cache()
  ts_diff_syntax_cache = {}
end

--- Read a cached hunk-context entry by "<file>:<line>" key, or nil when unresolved.
function M.context_cache_entry(key)
  return ts_context_cache[key]
end

--- Expose the live hunk-context cache for inspection by the debug dump and tests.
function M.context_cache()
  return ts_context_cache
end

--- Read the cached file-syntax entry for a filename, or nil when absent.
function M.file_syntax_cache_entry(filename)
  return ts_syntax_cache[filename]
end

local function file_source_lines(filename)
  return trace.span("source.file_lines", session.status and session.status.buf or nil, {
    file = filename,
  }, function()
    if file_contains_nul(filename) then return nil end
    local loaded = loaded_file_buffer(filename)
    if loaded then
      if vim.bo[loaded].binary then return nil end
      return vim.api.nvim_buf_get_lines(loaded, 0, -1, false)
    end
    local read_ok, lines = pcall(vim.fn.readfile, filename)
    if not read_ok or type(lines) ~= "table" then return nil end
    if lines_contain_nul(lines) then return nil end
    return lines
  end)
end

local function treesitter_source_buffer(filename)
  return trace.span("treesitter.source_buffer", session.status and session.status.buf or nil, {
    file = filename,
  }, function()
    if file_contains_nul(filename) then return nil end
    local loaded = loaded_file_buffer(filename)
    if loaded then
      if vim.bo[loaded].binary then return nil end
      return loaded
    end

    ts_source_bufs = ts_source_bufs or {}
    local cached = ts_source_bufs[filename]
    if cached and vim.api.nvim_buf_is_valid(cached) then return cached end

    local lines = file_source_lines(filename)
    if not lines then return nil end

    local buf = vim.api.nvim_create_buf(false, true)
    vim.bo[buf].bufhidden = "wipe"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    trace.span("treesitter.source_buffer.set_lines", session.status and session.status.buf or nil, {
      file = filename,
      source_line_count = #lines,
    }, function()
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    end)
    vim.bo[buf].filetype = detect_filetype(filename, lines)
    ts_source_bufs[filename] = buf
    return buf
  end)
end

local function clear_treesitter_source_buffers()
  for _, buf in pairs(ts_source_bufs or {}) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
  for _, cached in pairs(ts_diff_syntax_cache or {}) do
    if type(cached) == "table" and cached.buf and vim.api.nvim_buf_is_valid(cached.buf) then
      pcall(vim.api.nvim_buf_delete, cached.buf, { force = true })
    end
  end
  ts_source_bufs = {}
  ts_syntax_cache = {}
  ts_diff_syntax_cache = {}
end

local function treesitter_line_segments(buf, tree, query, row, text)
  local segments = {}
  if text == "" then return segments end
  if not query then
    return { { text = text } }
  end

  local ranges = {}
  local ok = pcall(function()
    for id, node in query:iter_captures(tree:root(), buf, row, row + 1) do
      local capture = query.captures[id]
      if capture and capture ~= "" then
        local start_row, start_col, end_row, end_col = node:range()
        if start_row <= row and end_row >= row then
          local range_start = start_row == row and start_col or 0
          local range_end = end_row == row and end_col or #text
          if range_end > range_start then
            ranges[#ranges + 1] = {
              start_col = math.max(range_start, 0),
              end_col = math.min(range_end, #text),
              hl_group = "@" .. capture,
            }
          end
        end
      end
    end
  end)
  if not ok or #ranges == 0 then
    return { { text = text } }
  end

  local segment_start = 1
  local current_hl = nil
  for col = 0, #text - 1 do
    local char = text:sub(col + 1, col + 1)
    local hl_group = nil
    if not char:match("%s") then
      for _, range in ipairs(ranges) do
        if col >= range.start_col and col < range.end_col then
          hl_group = range.hl_group
        end
      end
    end
    if col == 0 then
      current_hl = hl_group
    elseif hl_group ~= current_hl then
      segments[#segments + 1] = {
        text = text:sub(segment_start, col),
        hl_group = current_hl,
      }
      segment_start = col + 1
      current_hl = hl_group
    end
  end

  segments[#segments + 1] = {
    text = text:sub(segment_start),
    hl_group = current_hl,
  }
  return segments
end

local function hunk_context_from_trees(buf, query, highlight_query, trees, target)
  if not trees or #trees == 0 then return nil end

  local scopes = {}
  for id, node in query:iter_captures(trees[1]:root(), buf) do
    if query.captures[id] == "scope" then
      local sr, _, er, _ = node:range()
      if sr <= target and er >= target then
        scopes[#scopes + 1] = { node = node, sr = sr, er = er }
      end
    end
  end

  if #scopes == 0 then return nil end

  -- Sort by start row descending (innermost scope first). If two captures start
  -- on the same row, prefer the shorter range as the inner scope.
  table.sort(scopes, function(a, b)
    if a.sr ~= b.sr then return a.sr > b.sr end
    return a.er < b.er
  end)

  -- Limit to 3 levels max
  local max_depth = math.min(#scopes, 3)

  local function scope_name(scope)
    if scope.name ~= nil then return scope.name end
    for cid, cnode in query:iter_captures(scope.node, buf) do
      if query.captures[cid] == "scope.name" then
        local name_text = vim.treesitter.get_node_text(cnode, buf)
        if name_text and name_text ~= "" then
          scope.name = name_text
          return scope.name
        end
        break
      end
    end
    scope.name = false
    return nil
  end

  local function scope_key(scope)
    return ("%s:%d:%d"):format(scope_name(scope) or scope.node:type(), scope.sr, scope.er)
  end

  -- Collect names from outermost to innermost
  local names = {}
  for i = max_depth, 1, -1 do
    local scope = scopes[i]
    local name_text = scope_name(scope)
    if name_text then names[#names + 1] = name_text end
  end

  if #names == 0 then return nil end
  local selected_scope = scopes[1]
  local lines = vim.api.nvim_buf_get_lines(buf, selected_scope.sr, selected_scope.er + 1, false)
  local start_text = lines[1] or ""
  local end_text = lines[#lines] or start_text

  local function same_node(left, right)
    if not left or not right then return false end
    local left_start_row, left_start_col, left_end_row, left_end_col = left:range()
    local right_start_row, right_start_col, right_end_row, right_end_col = right:range()
    return left:type() == right:type()
      and left_start_row == right_start_row
      and left_start_col == right_start_col
      and left_end_row == right_end_row
      and left_end_col == right_end_col
  end

  local function sorted_row_set(row_set)
    local rows = {}
    for row in pairs(row_set) do
      rows[#rows + 1] = row
    end
    table.sort(rows)
    return rows
  end

  local root = trees[1]:root()
  local function row_node_for_context(row)
    local row_text = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
    local row_node = nil
    local node_ok, node = pcall(function()
      return root:named_descendant_for_range(row, 0, row, #row_text)
    end)
    if node_ok then row_node = node end
    if not row_node then
      node_ok, node = pcall(function()
        return root:descendant_for_range(row, 0, row, #row_text)
      end)
      if node_ok then row_node = node end
    end
    while row_node do
      local parent = row_node:parent()
      if not parent then break end
      local parent_start_row, _, parent_end_row, _ = parent:range()
      if parent_start_row == row
        and parent_end_row == row
        and parent_start_row >= selected_scope.sr
        and parent_end_row <= selected_scope.er
        and not same_node(parent, selected_scope.node) then
        row_node = parent
      else
        break
      end
    end
    return row_node
  end

  local path_start_rows = {}
  local path_end_rows = {}
  local target_node = row_node_for_context(target)
  local target_row_node = target_node
  while target_node do
    local node_start_row, _, node_end_row, _ = target_node:range()
    if node_start_row < selected_scope.sr or node_end_row > selected_scope.er then break end
    if node_start_row < target then path_start_rows[node_start_row + 1] = true end
    if node_end_row > target then path_end_rows[node_end_row + 1] = true end
    if same_node(target_node, selected_scope.node) then break end
    target_node = target_node:parent()
  end

  local function same_parent_neighbor_rows(direction)
    local rows = {}
    local target_parent = target_row_node and target_row_node:parent() or nil
    if not target_parent then return rows end
    for offset = 1, 3 do
      local row = target + (direction * offset)
      if row < selected_scope.sr or row > selected_scope.er then break end
      local row_text = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
      if vim.trim(row_text) == "" then break end
      local neighbor_node = row_node_for_context(row)
      local neighbor_parent = neighbor_node and neighbor_node:parent() or nil
      if not same_node(neighbor_parent, target_parent) then break end
      rows[row + 1] = true
    end
    return rows
  end

  local ancestor_boundaries = {}
  local ancestor_scope = scopes[2]
  if ancestor_scope and ancestor_scope.sr < selected_scope.sr then
    local ancestor_text = vim.api.nvim_buf_get_lines(buf, ancestor_scope.sr, ancestor_scope.sr + 1, false)[1] or ""
    local ancestor_end_text = vim.api.nvim_buf_get_lines(buf, ancestor_scope.er, ancestor_scope.er + 1, false)[1] or ""
    ancestor_boundaries[#ancestor_boundaries + 1] = {
      key = scope_key(ancestor_scope),
      row = ancestor_scope.sr + 1,
      text = ancestor_text,
      segments = treesitter_line_segments(buf, trees[1], highlight_query, ancestor_scope.sr, ancestor_text),
      end_row = ancestor_scope.er + 1,
      end_text = ancestor_end_text,
      end_segments = treesitter_line_segments(buf, trees[1], highlight_query, ancestor_scope.er, ancestor_end_text),
    }
  end

  return {
    label = table.concat(names, "."),
    start_row = selected_scope.sr,
    end_row = selected_scope.er,
    start_text = start_text,
    end_text = end_text,
    start_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.sr, start_text),
    end_segments = treesitter_line_segments(buf, trees[1], highlight_query, selected_scope.er, end_text),
    ancestor_boundaries = ancestor_boundaries,
    path_start_rows = sorted_row_set(path_start_rows),
    path_end_rows = sorted_row_set(path_end_rows),
    sibling_before_rows = sorted_row_set(same_parent_neighbor_rows(-1)),
    sibling_after_rows = sorted_row_set(same_parent_neighbor_rows(1)),
  }
end

local function cached_hunk_context(filename, line, callback_key, on_update)
  ts_context_cache = ts_context_cache or {}
  local cache_key = filename .. ":" .. line
  local cached = ts_context_cache[cache_key]
  if cached == false then return nil end
  if type(cached) == "string" then return cached end
  if type(cached) == "table" and not cached.pending then return cached end
  if type(cached) == "table" and cached.pending then
    if on_update then cached.callbacks[callback_key] = on_update end
    return nil
  end

  ts_context_cache[cache_key] = {
    pending = true,
    callbacks = on_update and { [callback_key] = on_update } or {},
  }
  git_data().compute_hunk_context_async(filename, line, function(context)
    local pending = ts_context_cache and ts_context_cache[cache_key]
    local callbacks = type(pending) == "table" and pending.callbacks or {}
    ts_context_cache[cache_key] = context or false
    for _, callback in pairs(callbacks) do
      local ok, err = pcall(callback, context)
      if not ok then notify_error("Tree-sitter context update failed: " .. tostring(err)) end
    end
  end)
  return nil
end

local function cached_file_syntax(filename, callback_key, on_update)
  local status_buf = session.status and session.status.buf or nil
  return trace.span("syntax.cached_file_syntax", status_buf, {
    file = filename,
    callback_key = callback_key,
  }, function()
    ts_syntax_cache = ts_syntax_cache or {}
    local cached = ts_syntax_cache[filename]
    if cached == false then return nil, false end
    if type(cached) == "table" and not cached.pending then return cached, false end
    if type(cached) == "table" and cached.pending then
      if on_update then cached.callbacks[callback_key] = on_update end
      return nil, true
    end

    ts_syntax_cache[filename] = {
      pending = true,
      callbacks = on_update and { [callback_key] = on_update } or {},
    }
    trace.span("syntax.cached_file_syntax.compute", status_buf, {
      file = filename,
      callback_key = callback_key,
    }, function()
      git_data().compute_file_syntax_async(filename, function(syntax)
        local pending = ts_syntax_cache and ts_syntax_cache[filename]
        local callbacks = type(pending) == "table" and pending.callbacks or {}
        ts_syntax_cache[filename] = syntax or false
        for _, callback in pairs(callbacks) do
          local ok, err = pcall(callback, syntax)
          if not ok then notify_error("Tree-sitter syntax update failed: " .. tostring(err)) end
        end
      end)
    end)
    return nil, true
  end)
end

local function hunk_context_label(context)
  if type(context) == "string" then return context end
  if type(context) == "table" then return context.label end
  return nil
end

local function hunk_context_scope_key(context)
  if type(context) ~= "table" then return nil end
  return ("%s:%d:%d"):format(context.label, context.start_row, context.end_row)
end

local function same_hunk_context_scope(left, right)
  local left_key = hunk_context_scope_key(left)
  return left_key ~= nil and left_key == hunk_context_scope_key(right)
end

local function same_hunk_ancestor_scope(left, right)
  local left_key = hunk_model.context_ancestor_key(left)
  return left_key ~= nil and left_key == hunk_model.context_ancestor_key(right)
end

local function line_indent(text)
  return tostring(text or ""):match("^%s*") or ""
end

local function hunk_visible_source_lines(diff_lines)
  local lines = type(diff_lines) == "table" and diff_lines
    or vim.split(tostring(diff_lines or ""), "\n", { plain = true })
  local visible = {}
  local in_hunk = false
  for _, diff_line in ipairs(lines) do
    if diff_line:match("^@@") then
      in_hunk = true
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == " " or prefix == "+" or prefix == "-" then
        visible[diff_line:sub(2)] = true
      end
    end
  end
  return visible
end

local function status_hunk_context_line(hunk)
  if not hunk then return nil end
  local diff = tostring(hunk.diff or "")
  local blocks = parse_unified_diff(diff)
  local parsed_hunk = blocks[1] and blocks[1].hunks and blocks[1].hunks[1] or nil
  if not parsed_hunk then return hunk.pos end
  return hunk_first_changed_current_line(parse_hunk_body(parsed_hunk))
end

local function hunk_line_visible_in_context_scope(parsed_line, context)
  if parsed_line.prefix ~= " " or type(context) ~= "table" then return true end
  if not parsed_line.new_line then return true end
  local scope_start = context.start_row + 1
  local scope_end = context.end_row + 1
  return parsed_line.new_line >= scope_start and parsed_line.new_line <= scope_end
end

local function diff_syntax_source_lines(diff_text, side)
  return trace.span("syntax.diff_source_lines", session.status and session.status.buf or nil, {
    side = side,
    diff_len = #(diff_text or ""),
  }, function()
    local lines = {}
    for _, block in ipairs(parse_unified_diff(diff_text)) do
      for _, hunk in ipairs(block.hunks) do
        local parsed_hunk = parse_hunk_body(hunk)
        for _, parsed_line in ipairs(parsed_hunk.lines) do
          if parsed_line.prefix == " "
            or (side == "old" and parsed_line.prefix == "-")
            or (side == "new" and parsed_line.prefix == "+") then
            lines[#lines + 1] = parsed_line.code
          end
        end
      end
    end
    return lines
  end)
end

local function cached_diff_syntax(filename, diff_text, side, callback_key, on_update)
  local status_buf = session.status and session.status.buf or nil
  return trace.span("syntax.cached_diff_syntax", status_buf, {
    file = filename,
    side = side,
    callback_key = callback_key,
    diff_len = #(diff_text or ""),
  }, function()
    ts_diff_syntax_cache = ts_diff_syntax_cache or {}
    local diff_hash = trace.span("syntax.cached_diff_syntax.sha256", status_buf, {
      file = filename,
      side = side,
      diff_len = #(diff_text or ""),
    }, function()
      return vim.fn.sha256(diff_text or "")
    end)
    local cache_key = ("%s:%s:%s"):format(filename, side, diff_hash)
    local cached = ts_diff_syntax_cache[cache_key]
    if cached == false then return nil, false end
    if type(cached) == "table" and not cached.pending then return cached, false end
    if type(cached) == "table" and cached.pending then
      if on_update then cached.callbacks[callback_key] = on_update end
      return nil, true
    end

    local lines = trace.span("syntax.cached_diff_syntax.source_lines", status_buf, {
      file = filename,
      side = side,
      diff_len = #(diff_text or ""),
    }, function()
      return diff_syntax_source_lines(diff_text, side)
    end)
    if #lines == 0 then return nil, false end
    ts_diff_syntax_cache[cache_key] = {
      pending = true,
      callbacks = on_update and { [callback_key] = on_update } or {},
    }
    trace.span("syntax.cached_diff_syntax.compute", status_buf, {
      file = filename,
      side = side,
      callback_key = callback_key,
      source_line_count = #lines,
    }, function()
      git_data().compute_diff_syntax_async(filename, lines, function(syntax)
        local pending = ts_diff_syntax_cache and ts_diff_syntax_cache[cache_key]
        local callbacks = type(pending) == "table" and pending.callbacks or {}
        ts_diff_syntax_cache[cache_key] = syntax or false
        for _, callback in pairs(callbacks) do
          local ok, err = pcall(callback, syntax)
          if not ok then notify_error("Tree-sitter diff syntax update failed: " .. tostring(err)) end
        end
        if syntax and status_render_current_model and session.status and session.status.buf then
          vim.schedule(function()
            if session.status and session.status.buf and vim.api.nvim_buf_is_valid(session.status.buf) then
              status_render_current_model()
            end
          end)
        end
      end)
    end)
    return nil, true
  end)
end

local function old_file_syntax_source_lines(filename, diff_text)
  return trace.span("syntax.old_file_source_lines", session.status and session.status.buf or nil, {
    file = filename,
    diff_len = #(diff_text or ""),
  }, function()
    local lines = file_source_lines(filename)
    if not lines then return nil end

    local hunks = {}
    trace.span("syntax.old_file_source_lines.parse", session.status and session.status.buf or nil, {
      file = filename,
      diff_len = #(diff_text or ""),
    }, function()
      for _, block in ipairs(parse_unified_diff(diff_text)) do
        for _, hunk in ipairs(block.hunks) do
          hunks[#hunks + 1] = parse_hunk_body(hunk, { preserve_trailing_blank = true })
        end
      end
    end)
    if #hunks == 0 then return nil end

    local old_lines = vim.deepcopy(lines)
    local rewrite_ok = trace.span("syntax.old_file_source_lines.rewrite", session.status and session.status.buf or nil, {
      file = filename,
      source_line_count = #lines,
      hunk_count = #hunks,
    }, function()
      for hunk_index = #hunks, 1, -1 do
        local hunk = hunks[hunk_index]
        local replacement = {}
        for _, parsed_line in ipairs(hunk.lines) do
          if parsed_line.prefix == " " or parsed_line.prefix == "-" then
            replacement[#replacement + 1] = parsed_line.code
          end
        end

        local start_index = math.max(tonumber(hunk.new_start) or 1, 1)
        local remove_count = math.max(tonumber(hunk.new_count) or 0, 0)
        if start_index > #old_lines + 1 then return false end
        if remove_count > 0 and (start_index + remove_count - 1) > #old_lines then return false end
        for _ = 1, remove_count do
          if start_index <= #old_lines then
            table.remove(old_lines, start_index)
          end
        end
        for replacement_index = #replacement, 1, -1 do
          table.insert(old_lines, start_index, replacement[replacement_index])
        end
      end
      return true
    end)
    if not rewrite_ok then return nil end

    return old_lines
  end)
end

local function diff_new_side_matches_file(filename, diff_text)
  return trace.span("syntax.diff_new_side_matches_file", session.status and session.status.buf or nil, {
    file = filename,
    diff_len = #(diff_text or ""),
  }, function()
    local lines = file_source_lines(filename)
    if not lines then return false end

    for _, block in ipairs(parse_unified_diff(diff_text)) do
      for _, hunk in ipairs(block.hunks) do
        local parsed_hunk = parse_hunk_body(hunk, { preserve_trailing_blank = true })
        local expected = {}
        for _, parsed_line in ipairs(parsed_hunk.lines) do
          if parsed_line.prefix == " " or parsed_line.prefix == "+" then
            expected[#expected + 1] = parsed_line.code
          end
        end
        local start_index = math.max(tonumber(parsed_hunk.new_start) or 1, 1)
        if start_index > #lines + 1 then return false end
        if #expected > 0 and (start_index + #expected - 1) > #lines then return false end
        for offset, expected_line in ipairs(expected) do
          if lines[start_index + offset - 1] ~= expected_line then return false end
        end
      end
    end

    return true
  end)
end

local function cached_old_file_syntax(filename, diff_text, callback_key, on_update)
  local status_buf = session.status and session.status.buf or nil
  return trace.span("syntax.cached_old_file_syntax", status_buf, {
    file = filename,
    callback_key = callback_key,
    diff_len = #(diff_text or ""),
  }, function()
    ts_diff_syntax_cache = ts_diff_syntax_cache or {}
    local diff_hash = trace.span("syntax.cached_old_file_syntax.sha256", status_buf, {
      file = filename,
      diff_len = #(diff_text or ""),
    }, function()
      return vim.fn.sha256(diff_text or "")
    end)
    local cache_key = ("%s:old-file:%s"):format(filename, diff_hash)
    local cached = ts_diff_syntax_cache[cache_key]
    if cached == false then return nil, false end
    if type(cached) == "table" and not cached.pending then return cached, false end
    if type(cached) == "table" and cached.pending then
      if on_update then cached.callbacks[callback_key] = on_update end
      return nil, true
    end

    local lines = trace.span("syntax.cached_old_file_syntax.source_lines", status_buf, {
      file = filename,
      diff_len = #(diff_text or ""),
    }, function()
      return old_file_syntax_source_lines(filename, diff_text)
    end)
    if not lines or #lines == 0 then return nil, false end
    ts_diff_syntax_cache[cache_key] = {
      pending = true,
      callbacks = on_update and { [callback_key] = on_update } or {},
    }
    trace.span("syntax.cached_old_file_syntax.compute", status_buf, {
      file = filename,
      callback_key = callback_key,
      source_line_count = #lines,
    }, function()
      git_data().compute_diff_syntax_async(filename, lines, function(syntax)
        local pending = ts_diff_syntax_cache and ts_diff_syntax_cache[cache_key]
        local callbacks = type(pending) == "table" and pending.callbacks or {}
        ts_diff_syntax_cache[cache_key] = syntax or false
        for _, callback in pairs(callbacks) do
          local ok, err = pcall(callback, syntax)
          if not ok then notify_error("Tree-sitter old file syntax update failed: " .. tostring(err)) end
        end
        if syntax and status_render_current_model and session.status and session.status.buf then
          vim.schedule(function()
            if session.status and session.status.buf and vim.api.nvim_buf_is_valid(session.status.buf) then
              status_render_current_model()
            end
          end)
        end
      end)
    end)
    return nil, true
  end)
end

local function diff_uses_file_syntax(hunk_staged, opts)
  opts = opts or {}
  if opts.syntax_source == "file" then return true end
  if opts.syntax_source == "diff" then return false end
  for _, staged in ipairs(hunk_staged or {}) do
    if staged then return false end
  end
  return true
end

local function prewarm_diff_syntax(filename, diff_text, hunk_staged, callback_key, on_update, opts)
  local status_buf = session.status and session.status.buf or nil
  return trace.span("syntax.prewarm_diff_syntax", status_buf, {
    file = filename,
    callback_key = callback_key,
    diff_len = #(diff_text or ""),
    staged_count = #(hunk_staged or {}),
    syntax_source = opts and opts.syntax_source or nil,
  }, function()
    opts = opts or {}
    local syntax_diff_text = opts.syntax_diff_text or diff_text
    local use_file_syntax = trace.span("syntax.prewarm_diff_syntax.uses_file_syntax", status_buf, {
      file = filename,
      callback_key = callback_key,
      syntax_source = opts.syntax_source,
      staged_count = #(hunk_staged or {}),
    }, function()
      return diff_uses_file_syntax(hunk_staged, opts)
    end)
    local new_side_matches_file = false
    if use_file_syntax then
      new_side_matches_file = trace.span("syntax.prewarm_diff_syntax.new_side_matches_file", status_buf, {
        file = filename,
        callback_key = callback_key,
        diff_len = #(syntax_diff_text or ""),
      }, function()
        return diff_new_side_matches_file(filename, syntax_diff_text)
      end)
    end
    trace.event("syntax.prewarm_diff_syntax.route", status_buf, {
      file = filename,
      callback_key = callback_key,
      use_file_syntax = use_file_syntax,
      new_side_matches_file = new_side_matches_file,
      diff_len = #(diff_text or ""),
      syntax_diff_len = #(syntax_diff_text or ""),
    })
    if use_file_syntax and new_side_matches_file then
      local old_syntax, old_pending = trace.span("syntax.prewarm_diff_syntax.cached_old_file", status_buf, {
        file = filename,
        callback_key = callback_key,
        diff_len = #(syntax_diff_text or ""),
      }, function()
        return cached_old_file_syntax(filename, syntax_diff_text, callback_key .. ":old-file", on_update)
      end)
      if not old_syntax and not old_pending then
        trace.span("syntax.prewarm_diff_syntax.fallback_old_diff", status_buf, {
          file = filename,
          callback_key = callback_key,
          diff_len = #(diff_text or ""),
        }, function()
          cached_diff_syntax(filename, diff_text, "old", callback_key .. ":old", on_update)
        end)
      end
      local syntax, pending = trace.span("syntax.prewarm_diff_syntax.cached_file", status_buf, {
        file = filename,
        callback_key = callback_key,
      }, function()
        return cached_file_syntax(filename, callback_key .. ":file", on_update)
      end)
      if not syntax and not pending then
        trace.span("syntax.prewarm_diff_syntax.fallback_new_diff", status_buf, {
          file = filename,
          callback_key = callback_key,
          diff_len = #(diff_text or ""),
        }, function()
          cached_diff_syntax(filename, diff_text, "new", callback_key .. ":new", on_update)
        end)
      end
    else
      trace.span("syntax.prewarm_diff_syntax.diff_old", status_buf, {
        file = filename,
        callback_key = callback_key,
        diff_len = #(diff_text or ""),
      }, function()
        cached_diff_syntax(filename, diff_text, "old", callback_key .. ":old", on_update)
      end)
      trace.span("syntax.prewarm_diff_syntax.diff_new", status_buf, {
        file = filename,
        callback_key = callback_key,
        diff_len = #(diff_text or ""),
      }, function()
        cached_diff_syntax(filename, diff_text, "new", callback_key .. ":new", on_update)
      end)
    end
  end)
end

local function status_file_syntax_diff_text(file)
  return trace.span("syntax.status_file_syntax_diff_text", session.status and session.status.buf or nil, {
    file = file and file.filename or nil,
  }, function()
    if not file then return nil end
    local hunks = trace.span("syntax.status_file_syntax_diff_text.hunks", session.status and session.status.buf or nil, {
      file = file.filename,
    }, function()
      return status_diff_hunks_for_file(file)
    end)
    local diffs = {}
    for _, hunk in ipairs(hunks) do
      if hunk.diff and hunk.diff ~= "" then diffs[#diffs + 1] = hunk.diff end
    end
    if #diffs == 0 then return nil end
    local diff_text = table.concat(diffs, "\n")
    trace.event("syntax.status_file_syntax_diff_text.result", session.status and session.status.buf or nil, {
      file = file.filename,
      hunk_count = #hunks,
      diff_count = #diffs,
      diff_len = #diff_text,
    })
    return diff_text
  end)
end

local function status_prewarm_hunk_budget(hunk_count, options)
  options = options or config.options or config.options or config.defaults
  hunk_count = math.max(0, math.floor(tonumber(hunk_count) or 0))
  local max_hunks = tonumber(options and options.status_cursor_prewarm_max_hunks)
  if max_hunks == nil then max_hunks = tonumber(config.defaults.status_cursor_prewarm_max_hunks) or 12 end
  -- A non-positive budget disables cursor-driven file prewarm entirely.
  if max_hunks <= 0 then return 0 end
  return math.min(hunk_count, math.floor(max_hunks))
end

local function prewarm_file_diff_syntax(file, callback_key_prefix, on_update, opts)
  return trace.span("syntax.prewarm_file_diff_syntax", session.status and session.status.buf or nil, {
    file = file and file.filename or nil,
    callback_key_prefix = callback_key_prefix,
    syntax_source = opts and opts.syntax_source or nil,
  }, function()
    if not (file and file.filename) then return end
    opts = opts or {}
    local combined_diff = nil
    if opts.syntax_source == "file" then
      combined_diff = trace.span("syntax.prewarm_file_diff_syntax.combined_diff", session.status and session.status.buf or nil, {
        file = file.filename,
        callback_key_prefix = callback_key_prefix,
      }, function()
        return status_file_syntax_diff_text(file)
      end)
    end
    if type(combined_diff) == "string" and combined_diff ~= "" then
      opts = vim.tbl_extend("force", opts, { syntax_diff_text = combined_diff })
    end
    local hunks = trace.span("syntax.prewarm_file_diff_syntax.hunks", session.status and session.status.buf or nil, {
      file = file.filename,
      callback_key_prefix = callback_key_prefix,
    }, function()
      return status_diff_hunks_for_file(file)
    end)
    local hunk_budget = status_prewarm_hunk_budget(#hunks)
    trace.event("syntax.prewarm_file_diff_syntax.start_hunks", session.status and session.status.buf or nil, {
      file = file.filename,
      callback_key_prefix = callback_key_prefix,
      hunk_count = #hunks,
      prewarm_budget = hunk_budget,
      capped = hunk_budget < #hunks,
      combined_diff_len = type(combined_diff) == "string" and #combined_diff or nil,
    })
    -- Bound cursor-driven prewarm: hovering a many-hunk file must not fan out into
    -- hundreds of Tree-sitter parses, which froze the editor on large diffs.
    local warmed = 0
    for hunk_index, hunk in ipairs(hunks) do
      if warmed >= hunk_budget then break end
      if hunk.diff and hunk.diff ~= "" then
        local callback_key = ("%s:%s:%d"):format(callback_key_prefix, file.filename, hunk_index)
        M.prewarm_diff_syntax(file.filename, hunk.diff, { hunk.staged }, callback_key, on_update, opts)
        warmed = warmed + 1
      end
    end
  end)
end

local function status_syntax_source_for_entry_kind(entry_kind)
  if entry_kind == nil
    or entry_kind == "file"
    or entry_kind == "hunk"
    or entry_kind == "pr_file"
    or entry_kind == "pr_hunk" then
    return "file"
  end
  return "diff"
end

-- Public surface (init/hunk_model/render reach these via seams).
M.file_source_lines = file_source_lines
M.same_hunk_ancestor_scope = same_hunk_ancestor_scope
M.status_hunk_context_line = status_hunk_context_line
M.old_file_syntax_source_lines = old_file_syntax_source_lines
M.diff_new_side_matches_file = diff_new_side_matches_file
M.cached_old_file_syntax = cached_old_file_syntax
M.diff_uses_file_syntax = diff_uses_file_syntax
M.prewarm_diff_syntax = prewarm_diff_syntax
M.status_file_syntax_diff_text = status_file_syntax_diff_text
M.status_prewarm_hunk_budget = status_prewarm_hunk_budget
M.status_syntax_source_for_entry_kind = status_syntax_source_for_entry_kind
M.treesitter_source_buffer = treesitter_source_buffer
M.clear_treesitter_source_buffers = clear_treesitter_source_buffers
M.treesitter_line_segments = treesitter_line_segments
M.hunk_context_from_trees = hunk_context_from_trees
M.cached_hunk_context = cached_hunk_context
M.cached_file_syntax = cached_file_syntax
M.hunk_context_label = hunk_context_label
M.hunk_context_scope_key = hunk_context_scope_key
M.same_hunk_context_scope = same_hunk_context_scope
M.line_indent = line_indent
M.hunk_visible_source_lines = hunk_visible_source_lines
M.hunk_line_visible_in_context_scope = hunk_line_visible_in_context_scope
M.diff_syntax_source_lines = diff_syntax_source_lines
M.cached_diff_syntax = cached_diff_syntax
M.prewarm_file_diff_syntax = prewarm_file_diff_syntax

return M
