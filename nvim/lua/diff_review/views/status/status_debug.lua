--- Owns the GitStatus debug/diagnostics surface: the structured event log, the per-render perf
--- timer that feeds the debug log, and the row/extmark/syntax inspection dump. Dev-only — the hot
--- path reaches it only when debug logging is enabled.
---
--- Reads live status state, namespaces, and the perf module through the init module via dr().

local config = require("diff_review.infra.config")

--- Resolve the init module lazily so diagnostics can read orchestrator state without a load-time
--- circular require.
local function dr()
  return require("diff_review")
end

local M = {}

function M.enabled()
  local global_enabled = vim.g.diff_review_gitstatus_debug
  return dr()._gitstatus_debug_force == true
    or dr()._gitstatus_debug_enabled == true
    or global_enabled == true
    or global_enabled == 1
end

---@param value any
---@return string
function M.one_line(value)
  return vim.inspect(value):gsub("\r", "\\r"):gsub("\n", "\\n")
end

---@param value any
---@return string
function M.text(value)
  return tostring(value or ""):gsub("\r", "\\r"):gsub("\n", "\\n")
end

---@param event string
---@param payload? table
function M.event(event, payload)
  if not M.enabled() then return end
  local line = ("GitStatus debug event time=%s event=%s payload=%s"):format(
    os.date("%Y-%m-%d %H:%M:%S"),
    tostring(event),
    M.one_line(payload or {})
  )
  local ok, err = pcall(vim.fn.writefile, { line }, dr()._gitstatus_debug_log_path(), "a")
  if not ok then
    dr()._notify_debug("GitStatus debug event failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
  end
end

function M.perf_enabled()
  local options = dr().config or config.options or config.defaults
  local global_enabled = vim.g.diff_review_gitstatus_perf
  return dr()._gitstatus_debug_perf_force == true
    or dr()._gitstatus_debug_perf_enabled == true
    or global_enabled == true
    or global_enabled == 1
    or (options and options.status_perf_logging == true)
    or M.enabled()
end

---@return integer
function M.perf_now()
  local uv = vim.uv or vim.loop
  if uv and uv.hrtime then return uv.hrtime() end
  return math.floor(vim.fn.reltimefloat(vim.fn.reltime()) * 1000000000)
end

---@param started integer
---@return number
function M.perf_elapsed_ms(started)
  local elapsed = M.perf_now() - started
  return math.floor((elapsed / 1000000) * 1000 + 0.5) / 1000
end

function M.flush_perf()
  local lines = dr()._gitstatus_debug_perf_queue
  dr()._gitstatus_debug_perf_queue = nil
  dr()._gitstatus_debug_perf_flush_pending = false
  if not (lines and #lines > 0) then return end
  local path = dr()._gitstatus_debug_log_path()
  local text = table.concat(lines, "\n") .. "\n"
  local uv = vim.uv or vim.loop
  local function report_error(err)
    if not err then return end
    pcall(vim.schedule, function()
      dr()._notify_debug("GitStatus perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
    end)
  end
  if uv and uv.fs_open and uv.fs_write and uv.fs_close then
    uv.fs_open(path, "a", 438, function(open_err, fd)
      if open_err or not fd then
        report_error(open_err or "open failed")
        return
      end
      uv.fs_write(fd, text, -1, function(write_err)
        uv.fs_close(fd, function(close_err)
          report_error(write_err or close_err)
        end)
      end)
    end)
    return
  end
  local ok, err = pcall(vim.fn.writefile, lines, dr()._gitstatus_debug_log_path(), "a")
  if not ok then
    dr()._notify_debug("GitStatus perf log failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
  end
end

---@param event string
---@param payload? table
function M.perf_event(event, payload)
  if not M.perf_enabled() then return end
  dr()._gitstatus_debug_perf_sequence = (dr()._gitstatus_debug_perf_sequence or 0) + 1
  local line = ("GitStatus perf seq=%d time=%s event=%s payload=%s"):format(
    dr()._gitstatus_debug_perf_sequence,
    os.date("%Y-%m-%d %H:%M:%S"),
    tostring(event),
    M.one_line(payload or {})
  )
  dr()._gitstatus_debug_perf_queue = dr()._gitstatus_debug_perf_queue or {}
  dr()._gitstatus_debug_perf_queue[#dr()._gitstatus_debug_perf_queue + 1] = line
  if dr()._gitstatus_debug_perf_flush_pending then return end
  dr()._gitstatus_debug_perf_flush_pending = true
  vim.defer_fn(function()
    M.flush_perf()
  end, 25)
end

---@param event string
---@param payload table?
---@param callback fun(): any
---@return any
function M.perf_span(event, payload, callback)
  if not M.perf_enabled() then return callback() end
  local started = M.perf_now()
  local function pack_results(...)
    return { n = select("#", ...), ... }
  end
  local results = pack_results(pcall(callback))
  local ok = results[1]
  local next_payload = vim.deepcopy(payload or {})
  next_payload.ms = M.perf_elapsed_ms(started)
  if not ok then
    next_payload.error = tostring(results[2])
    M.perf_event(event .. ".error", next_payload)
    error(results[2], 0)
  end
  M.perf_event(event, next_payload)
  local unpack_values = table.unpack or unpack
  return unpack_values(results, 2, results.n)
end
function M.row_text(row)
  if type(row) ~= "table" then return M.text(row) end
  local parts = {}
  for _, chunk in ipairs(row) do
    if type(chunk) == "string" then
      parts[#parts + 1] = chunk
    elseif type(chunk) == "table" then
      if type(chunk[1]) == "string" then parts[#parts + 1] = chunk[1] end
      if type(chunk.virt_text) == "table" then
        local virtual_parts = {}
        for _, virtual_chunk in ipairs(chunk.virt_text) do
          if type(virtual_chunk) == "table" and type(virtual_chunk[1]) == "string" then
            virtual_parts[#virtual_parts + 1] = virtual_chunk[1]
          end
        end
        if #virtual_parts > 0 then parts[#parts + 1] = "<virt:" .. table.concat(virtual_parts, "") .. ">" end
      end
    end
  end
  return table.concat(parts, "")
end

---@param rows table?
---@param limit integer
---@return table[]
function M.row_preview(rows, limit)
  local preview = {}
  if type(rows) ~= "table" then return preview end
  for row_index = 1, math.min(#rows, limit) do
    local row = rows[row_index]
    preview[#preview + 1] = {
      index = row_index,
      text = M.row_text(row),
      hunk_header = type(row) == "table" and row.diff_review_hunk_header == true or nil,
      boundary = type(row) == "table" and row.diff_review_boundary == true or nil,
    }
  end
  return preview
end

---@param cache any
---@return string
function M.cache_state(cache)
  if cache == nil then return "nil" end
  if cache == false then return "false" end
  if type(cache) == "table" then
    if cache.pending then return "pending" end
    local parts = { "ready" }
    if cache.buf then parts[#parts + 1] = "buf=" .. tostring(cache.buf) end
    if cache.tree then parts[#parts + 1] = "tree=true" end
    if cache.highlight_query then parts[#parts + 1] = "query=true" end
    return table.concat(parts, " ")
  end
  return type(cache)
end

---@param details table
---@return string
function M.extmark_details(details)
  local parts = {}
  if details.hl_group ~= nil then parts[#parts + 1] = "hl=" .. M.one_line(details.hl_group) end
  if details.line_hl_group ~= nil then parts[#parts + 1] = "line_hl=" .. M.one_line(details.line_hl_group) end
  if details.hl_eol ~= nil then parts[#parts + 1] = "eol=" .. tostring(details.hl_eol) end
  if details.priority ~= nil then parts[#parts + 1] = "prio=" .. tostring(details.priority) end
  if details.end_col ~= nil then parts[#parts + 1] = "end=" .. tostring(details.end_col) end
  if details.virt_text ~= nil then parts[#parts + 1] = "virt=" .. M.one_line(details.virt_text) end
  if details.conceal ~= nil then parts[#parts + 1] = "conceal=" .. M.one_line(details.conceal) end
  if details.url ~= nil then parts[#parts + 1] = "url=" .. M.one_line(details.url) end
  return table.concat(parts, " ")
end

---@param buf integer
---@param row integer 1-based
---@return string[]
function M.extmarks_for_row(buf, row)
  local lines = {}
  local namespaces = vim.api.nvim_get_namespaces()
  local namespace_items = {}
  for name, namespace in pairs(namespaces) do
    namespace_items[#namespace_items + 1] = { name = name, namespace = namespace }
  end
  table.sort(namespace_items, function(left, right)
    return left.name < right.name
  end)

  for _, item in ipairs(namespace_items) do
    local ok, marks = pcall(
      vim.api.nvim_buf_get_extmarks,
      buf,
      item.namespace,
      { row - 1, 0 },
      { row - 1, -1 },
      { details = true, overlap = true }
    )
    if ok and type(marks) == "table" and #marks > 0 then
      for _, mark in ipairs(marks) do
        local details = mark[4] or {}
        lines[#lines + 1] = ("    ns=%s(%d) col=%s %s"):format(
          item.name,
          item.namespace,
          tostring(mark[3]),
          M.extmark_details(details)
        )
      end
    end
  end
  return lines
end

---@param win integer
---@param row integer 1-based
---@param line string
---@return string
function M.first_token_cell(win, row, line)
  local start_col, end_col = line:find("[%a_][%w_]*")
  if not start_col then return "token=<none>" end
  local token = line:sub(start_col, end_col)
  local screen_ok, screen_pos = pcall(vim.fn.screenpos, win, row, start_col)
  if not (screen_ok and type(screen_pos) == "table" and tonumber(screen_pos.row) and tonumber(screen_pos.row) > 0) then
    return ("token=%s screen=<not-visible>"):format(M.one_line(token))
  end
  local cell_ok, cell = pcall(vim.api.nvim__inspect_cell, 1, screen_pos.row - 1, screen_pos.col - 1)
  if not cell_ok then
    return ("token=%s screen=%s cell_error=%s"):format(
      M.one_line(token),
      M.one_line(screen_pos),
      M.text(cell)
    )
  end
  return ("token=%s screen=%s cell=%s"):format(
    M.one_line(token),
    M.one_line(screen_pos),
    M.one_line(cell)
  )
end

---@param entry DiffReviewStatusEntry?
---@return string
function M.entry(entry)
  if not entry then return "entry=nil" end
  local parts = {
    "kind=" .. tostring(entry.kind),
    "id=" .. tostring(entry.id),
  }
  if entry.file then
    parts[#parts + 1] = "file=" .. tostring(entry.file.filename)
    parts[#parts + 1] = "relpath=" .. tostring(entry.file.relpath)
    parts[#parts + 1] = "file_status=" .. tostring(entry.file.git_status or entry.file.status)
  end
  if entry.hunk then
    parts[#parts + 1] = "hunk_pos=" .. tostring(entry.hunk.pos)
    parts[#parts + 1] = "hunk_staged=" .. tostring(entry.hunk.staged)
  end
  if entry.diff_line then
    parts[#parts + 1] = "diff=" .. M.one_line(entry.diff_line)
  end
  return table.concat(parts, " ")
end

---@param state table
---@param filename string
---@return string[]
function M.file_syntax(state, filename)
  local lines = {}
  if not filename or filename == "" then return lines end
  if state.gitstatus_debug_seen_files[filename] then return lines end
  state.gitstatus_debug_seen_files[filename] = true

  lines[#lines + 1] = "  syntax file=" .. filename
  local file_entry = nil
  for _, section in ipairs(state.sections or {}) do
    for _, file in ipairs(section.files or {}) do
      if file.filename == filename then
        file_entry = file
        break
      end
    end
    if file_entry then break end
  end
  if file_entry then
    local diff_text = dr()._status_file_syntax_diff_text(file_entry)
    lines[#lines + 1] = "    combined_diff_len=" .. tostring(diff_text and #diff_text or 0)
    lines[#lines + 1] = "    new_side_matches_file=" .. tostring(diff_text and dr()._diff_new_side_matches_file(filename, diff_text) or nil)
  else
    lines[#lines + 1] = "    status_file=<not-found>"
  end
  local source_lines = dr()._file_source_lines(filename)
  lines[#lines + 1] = "    source_lines=" .. tostring(source_lines and #source_lines or nil)
  lines[#lines + 1] = "    file_syntax_cache=" .. M.cache_state(dr()._ts_syntax_cache and dr()._ts_syntax_cache[filename])
  return lines
end

---@param buf integer
---@param reason string
function M.dump(buf, reason)
  if not M.enabled() then return end
  local state = dr()._status_states and dr()._status_states[buf] or (dr()._status and dr()._status.buf == buf and dr()._status) or nil
  if not (state and (state.view_kind == "status" or state.view_kind == "pr" or state.view_kind == "review") and vim.api.nvim_buf_is_valid(buf)) then return end
  state.gitstatus_debug_dump_reason = reason
  if state.gitstatus_debug_dump_pending then return end
  state.gitstatus_debug_dump_pending = true

  vim.defer_fn(function()
    state = dr()._status_states and dr()._status_states[buf] or (dr()._status and dr()._status.buf == buf and dr()._status) or nil
    if state then state.gitstatus_debug_dump_pending = false end
    if not M.enabled() then return end
    if not (state and (state.view_kind == "status" or state.view_kind == "pr" or state.view_kind == "review") and vim.api.nvim_buf_is_valid(buf)) then return end
    reason = state.gitstatus_debug_dump_reason or reason
    local win = vim.fn.win_findbuf(buf)[1]
    local lines = {
      "",
      "GitStatus debug dump",
      "reason=" .. tostring(reason),
      "time=" .. os.date("%Y-%m-%d %H:%M:%S"),
      "buf=" .. tostring(buf),
      "win=" .. tostring(win),
      "cwd=" .. tostring(state.cwd),
      "view_kind=" .. tostring(state.view_kind),
      "request_id=" .. tostring(state.request_id),
      "reconcile_generation=" .. tostring(state.reconcile_generation),
      "line_count=" .. tostring(vim.api.nvim_buf_line_count(buf)),
      "filetype=" .. tostring(vim.bo[buf].filetype),
      "buftype=" .. tostring(vim.bo[buf].buftype),
    }

    if win and vim.api.nvim_win_is_valid(win) then
      lines[#lines + 1] = "window_options=" .. M.one_line({
        conceallevel = vim.wo[win].conceallevel,
        concealcursor = vim.wo[win].concealcursor,
        foldenable = vim.wo[win].foldenable,
        foldlevel = vim.wo[win].foldlevel,
        foldmethod = vim.wo[win].foldmethod,
        linebreak = vim.wo[win].linebreak,
        wrap = vim.wo[win].wrap,
        winbar = vim.wo[win].winbar,
      })
    else
      lines[#lines + 1] = "window_options=<no-valid-window>"
    end

    lines[#lines + 1] = ""
    lines[#lines + 1] = "syntax caches:"
    state.gitstatus_debug_seen_files = {}
    for _, entry in pairs(state.entries or {}) do
      if entry and entry.file and entry.file.filename then
        vim.list_extend(lines, M.file_syntax(state, entry.file.filename))
      end
    end
    state.gitstatus_debug_seen_files = nil

    lines[#lines + 1] = ""
    lines[#lines + 1] = "visible rows:"
    local start_row = 1
    local end_row = vim.api.nvim_buf_line_count(buf)
    if win and vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_call(win, function()
        start_row = vim.fn.line("w0")
        end_row = vim.fn.line("w$")
      end)
    end
    local buffer_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
    for offset, line in ipairs(buffer_lines) do
      local row = start_row + offset - 1
      local entry = state.entries and state.entries[row] or nil
      lines[#lines + 1] = ("%4d | %s"):format(row, M.text(line))
      lines[#lines + 1] = "    " .. M.entry(entry)
      if win and vim.api.nvim_win_is_valid(win) then
        lines[#lines + 1] = "    " .. M.first_token_cell(win, row, line)
      end
      vim.list_extend(lines, M.extmarks_for_row(buf, row))
    end

    lines[#lines + 1] = ""
    lines[#lines + 1] = "diff rows missing Tree-sitter extmarks:"
    local missing_count = 0
    for row, entry in pairs(state.entries or {}) do
      local diff_line = entry and entry.diff_line or nil
      if diff_line and diff_line.code and diff_line.code:match("%S") then
        local has_treesitter = false
        for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, dr()._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
          local details = mark[4] or {}
          if type(details.hl_group) == "string" and details.hl_group:sub(1, 1) == "@" then
            has_treesitter = true
            break
          elseif type(details.hl_group) == "table" then
            for _, group in ipairs(details.hl_group) do
              if tostring(group):sub(1, 1) == "@" then
                has_treesitter = true
                break
              end
            end
            if has_treesitter then break end
          end
        end
        if not has_treesitter then
          missing_count = missing_count + 1
          lines[#lines + 1] = ("%4d | %s | %s"):format(row, M.text(diff_line.code), M.entry(entry))
        end
      end
    end
    if missing_count == 0 then lines[#lines + 1] = "  none" end

    local ok, err = pcall(vim.fn.writefile, lines, dr()._gitstatus_debug_log_path(), "a")
    if not ok then
      dr()._notify_debug("GitStatus debug dump failed: " .. tostring(err), vim.log.levels.WARN, { title = "GitStatus" })
    end
  end, 250)
end

return M
