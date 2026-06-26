vim.loader.enable(false)

local diff_review = require("diff_review")
local status_render = require("diff_review.views.status.status_render")
local ui = require("diff_review.infra.ui")
local session = require("diff_review.session")
local syntax_engine = require("diff_review.render.syntax_engine")
local gh = require("diff_review.integrations.gh")

-- Diff-body syntax/background/intraline live in the decoration span store and are
-- emitted by the provider; drive the test seam to apply them into the decorate
-- namespace, then read marks from both namespaces (gutter stays in _status_ns).
local function row_marks(buf, row)
  pcall(status_render.status_decorate_rows, buf, row, row)
  local marks = vim.api.nvim_buf_get_extmarks(buf, ui.status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ui.status_decorate_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
    marks[#marks + 1] = mark
  end
  return marks
end

local ferrous_root = "D:/code/ferrous"
local calls = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function git_lines(args)
  local command = { "git", "-c", "safe.directory=" .. ferrous_root, "-C", ferrous_root }
  vim.list_extend(command, args)
  local output = vim.fn.systemlist(command)
  assert_true(vim.v.shell_error == 0, table.concat(command, " ") .. " failed:\n" .. table.concat(output, "\n"))
  return output
end

local full_unstaged_diff = git_lines({ "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff", "--unified=0" })
local full_staged_diff = git_lines({ "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff", "--unified=0", "--cached" })
local full_unstaged_name_status = git_lines({ "diff", "--name-status" })
local full_staged_name_status = git_lines({ "diff", "--cached", "--name-status" })
local full_untracked = git_lines({ "ls-files", "--others", "--exclude-standard" })

assert_true(#full_unstaged_diff > 0, "Ferrous working tree has no unstaged diff to reproduce")

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { ferrous_root }, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\trev-parse\t--short\tHEAD" then return { "ferrous" }, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\tlog\t-1\t--format=%s" then return { "ferrous full syntax fixture" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. ferrous_root .. "\tls-files\t--others\t--exclude-standard" then return full_untracked, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\tdiff\t--cached\t--name-status" then return full_staged_name_status, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\tdiff\t--name-status" then return full_unstaged_name_status, 0 end
  if key == "git\t-C\t" .. ferrous_root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return full_unstaged_diff, 0
  end
  if key == "git\t-C\t" .. ferrous_root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return full_staged_diff, 0
  end
  if key == "git\t-C\t" .. ferrous_root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20" then return {}, 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  calls[#calls + 1] = command_key(command)
  local output, code = backend.systemlist(command)
  cb(output, code, "")
end

function backend.system()
  return "", 0
end

function backend.system_async(_, _, cb)
  cb({ code = 0, stdout = "", stderr = "", output = "" })
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_dump(buf)
  return table.concat(buffer_lines(buf), "\n")
end

local function find_row(buf, pattern)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then return index end
  end
  return nil
end

local function wait_for(condition, message)
  local ok = vim.wait(8000, condition, 20)
  if ok then return end
  if type(message) == "function" then error(message(), 2) end
  error(message, 2)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function extmark_groups(buf, row)
  local groups = {}
  local marks = row_marks(buf, row)
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        groups[#groups + 1] = tostring(group)
      end
    elseif details.hl_group then
      groups[#groups + 1] = tostring(details.hl_group)
    elseif details.line_hl_group then
      groups[#groups + 1] = "line:" .. tostring(details.line_hl_group)
    end
  end
  return table.concat(groups, ",")
end

local function line_has_treesitter_highlight(buf, row)
  return extmark_groups(buf, row):find("@", 1, true) ~= nil
end

local function first_diff_match_mismatch(filename, diff_text)
  local lines = syntax_engine.file_source_lines(filename)
  if not lines then return "file source unavailable" end

  local hunk_header = nil
  local expected = nil
  for _, line in ipairs(vim.split(diff_text or "", "\n", { plain = true })) do
    local new_start_text = line:match("^@@ %-%d+,?%d* %+(%d+),?%d* @@")
    if new_start_text then
      if hunk_header and expected then
        for offset, expected_line in ipairs(expected.lines) do
          local actual = lines[expected.start + offset - 1]
          if actual ~= expected_line then
            return ("%s expected file line %d = %q, got %q"):format(hunk_header, expected.start + offset - 1, expected_line, actual)
          end
        end
      end
      hunk_header = line
      expected = { start = tonumber(new_start_text) or 1, lines = {} }
    elseif expected then
      local prefix = line:sub(1, 1)
      if (prefix == " " or prefix == "+") and not line:find("^%+%+%+ ") then
        expected.lines[#expected.lines + 1] = line:sub(2)
      end
    end
  end

  if hunk_header and expected then
    for offset, expected_line in ipairs(expected.lines) do
      local actual = lines[expected.start + offset - 1]
      if actual ~= expected_line then
        return ("%s expected file line %d = %q, got %q"):format(hunk_header, expected.start + offset - 1, expected_line, actual)
      end
    end
  end

  return "all context/addition lines match current file"
end

local function syntax_debug_for_row(buf, row)
  local status = session.status or {}
  local entry = status.entries and status.entries[row] or nil
  if not (entry and entry.file and entry.hunk) then return "no status entry for row " .. tostring(row) end
  local file = entry.file
  local syntax_diff_text = syntax_engine.status_file_syntax_diff_text(file)
  local file_match = syntax_diff_text and syntax_engine.diff_new_side_matches_file(file.filename, syntax_diff_text)
  local hunk_match = entry.hunk.diff and syntax_engine.diff_new_side_matches_file(file.filename, entry.hunk.diff)
  local source_lines = syntax_engine.file_source_lines(file.filename)
  local diff_line = entry.diff_line or {}
  local current_line = diff_line.line and source_lines and source_lines[diff_line.line] or nil
  local cache = syntax_engine.file_syntax_cache_entry(file.filename)
  local cache_state = type(cache) == "table" and (cache.pending and "pending" or "ready") or tostring(cache)
  local lines = buffer_lines(buf)
  local neighbors = {}
  for line_number = math.max(1, row - 2), math.min(#lines, row + 2) do
    local neighbor_entry = status.entries and status.entries[line_number] or nil
    local neighbor_diff = neighbor_entry and neighbor_entry.diff_line or nil
    neighbors[#neighbors + 1] = ("%d: %s | entry=%s | extmarks=%s"):format(
      line_number,
      lines[line_number] or "",
      vim.inspect(neighbor_diff),
      extmark_groups(buf, line_number)
    )
  end
  return table.concat({
    "syntax diagnostics:",
    "  filename=" .. tostring(file.filename),
    "  relpath=" .. tostring(file.relpath),
    "  line_text=" .. vim.inspect(lines[row]),
    "  row_diff_line=" .. vim.inspect(diff_line),
    "  current_file_line=" .. vim.inspect(current_line),
    "  combined_diff_match=" .. tostring(file_match),
    "  hunk_diff_match=" .. tostring(hunk_match),
    "  match_mismatch=" .. first_diff_match_mismatch(file.filename, syntax_diff_text or ""),
    "  ts_file_cache=" .. cache_state,
    "  neighbors:",
    table.concat(neighbors, "\n"),
  }, "\n")
end

local function cell_at_text(buf, row, text)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local text_start = line:find(text, 1, true)
  assert_true(text_start ~= nil, ("missing text %q on row %d: %s"):format(text, row, line))
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, text_start - 1 })
  vim.cmd("normal! zt")
  vim.cmd("redraw!")
  local screen_pos = vim.fn.screenpos(vim.api.nvim_get_current_win(), row, text_start)
  assert_true(
    type(screen_pos) == "table" and tonumber(screen_pos.row) and tonumber(screen_pos.row) > 0,
    ("unable to resolve screen position for %q on row %d: %s"):format(text, row, vim.inspect(screen_pos))
  )
  local inspected_cell = nil
  local inspected_ok = false
  local expected_char = text:sub(1, 1)
  for offset = -8, 40 do
    local ok, cell = pcall(vim.api.nvim__inspect_cell, 1, screen_pos.row - 1, screen_pos.col - 1 + offset)
    if ok and type(cell) == "table" and type(cell[2]) == "table" and cell[1] == expected_char then
      inspected_ok = true
      inspected_cell = cell
      break
    end
  end
  assert_true(inspected_ok and inspected_cell ~= nil, "unable to inspect rendered screen cell")
  local cell = inspected_cell
  cell[2]._inspect_cell = cell
  return cell[2]
end

local function normal_foreground()
  local normal = vim.api.nvim_get_hl(0, { name = "Normal", link = false })
  return normal.fg
end

local function cell_has_highlight(cell, highlight)
  local inspect_cell = cell and cell._inspect_cell or nil
  for _, item in ipairs(type(inspect_cell) == "table" and inspect_cell[3] or {}) do
    if item.hi_name == highlight then return true end
  end
  return false
end

local function assert_syntax_row(buf, row, text)
  wait_for(function()
    return line_has_treesitter_highlight(buf, row)
  end, function()
    return ("row %d has no Tree-sitter extmarks for %q: %s\n\n%s\n\n%s"):format(
      row,
      text,
      extmark_groups(buf, row),
      syntax_debug_for_row(buf, row),
      buffer_dump(buf)
    )
  end)
  local cell = cell_at_text(buf, row, text)
  assert_true(
    cell.foreground and cell.foreground ~= normal_foreground(),
    ("%q rendered with normal foreground on row %d: %s extmarks=%s"):format(text, row, vim.inspect(cell), extmark_groups(buf, row))
  )
  assert_true(
    not cell_has_highlight(cell, "Special"),
    ("%q rendered with markdown Special overlay on row %d: %s extmarks=%s"):format(text, row, vim.inspect(cell), extmark_groups(buf, row))
  )
end

local function assert_syntax_text(buf, pattern, text)
  wait_for(function()
    local row = find_row(buf, pattern)
    return row ~= nil and line_has_treesitter_highlight(buf, row)
  end, function()
    local row = find_row(buf, pattern)
    if not row then return ("missing row for %q\n\n%s"):format(pattern, buffer_dump(buf)) end
    return ("row %d has no Tree-sitter extmarks for %q: %s\n\n%s\n\n%s"):format(
      row,
      text,
      extmark_groups(buf, row),
      syntax_debug_for_row(buf, row),
      buffer_dump(buf)
    )
  end)
  local row = find_row(buf, pattern)
  assert_true(row ~= nil, "missing row for " .. pattern)
  local cell = cell_at_text(buf, row, text)
  assert_true(
    cell.foreground and cell.foreground ~= normal_foreground(),
    ("%q rendered with normal foreground on row %d: %s extmarks=%s"):format(text, row, vim.inspect(cell), extmark_groups(buf, row))
  )
  assert_true(
    not cell_has_highlight(cell, "Special"),
    ("%q rendered with markdown Special overlay on row %d: %s extmarks=%s"):format(text, row, vim.inspect(cell), extmark_groups(buf, row))
  )
end

local function model_store_block_range(buf)
  local lines = buffer_lines(buf)
  local start_row = find_row(buf, "blue/engine/plugins/model/src/pbr/model_store.rs")
  if not start_row then return nil, nil end
  for row = start_row + 1, #lines do
    if lines[row]:match("^Modified ")
      or lines[row]:match("^New%s+")
      or lines[row]:match("^Removed ") then
      return start_row + 1, row - 1
    end
  end
  return start_row + 1, #lines
end

local function first_model_store_code_row_without_syntax(buf)
  local start_row, end_row = model_store_block_range(buf)
  if not start_row then return nil, "model_store.rs block is not visible" end
  local lines = buffer_lines(buf)
  for row = start_row, end_row do
    local line = lines[row] or ""
    local trimmed = vim.trim(line)
    if trimmed ~= ""
      and trimmed ~= "..."
      and not trimmed:match("^@@")
      and not line_has_treesitter_highlight(buf, row) then
      return row, line
    end
  end
  return nil, nil
end

local function first_model_store_code_row_with_plain_foreground(buf)
  local start_row, end_row = model_store_block_range(buf)
  if not start_row then return nil, "model_store.rs block is not visible" end
  local lines = buffer_lines(buf)
  for row = start_row, end_row do
    local line = lines[row] or ""
    local trimmed = vim.trim(line)
    if trimmed ~= "" and trimmed ~= "..." and not trimmed:match("^@@") and not trimmed:match("^//") then
      local start_col, end_col = line:find("[%a_][%w_]*")
      if start_col then
        local token = line:sub(start_col, end_col)
        local cell = cell_at_text(buf, row, token)
        if not (cell.foreground and cell.foreground ~= normal_foreground()) then
          return row, line, token, cell
        end
      end
    end
  end
  return nil, nil, nil, nil
end

local function assert_model_store_block_syntax(buf)
  wait_for(function()
    local row = first_model_store_code_row_without_syntax(buf)
    return row == nil
  end, function()
    local row, line = first_model_store_code_row_without_syntax(buf)
    if not row then return "model_store.rs block never became visible" end
    return ("model_store.rs row %d has no Tree-sitter extmarks: %s\n\n%s\n\n%s"):format(
      row,
      line,
      syntax_debug_for_row(buf, row),
      buffer_dump(buf)
    )
  end)
  local visual_row, visual_line, token, cell = first_model_store_code_row_with_plain_foreground(buf)
  assert_true(
    visual_row == nil,
    ("model_store.rs row %d renders %q with Normal foreground: %s\n%s"):format(
      visual_row or -1,
      token or "",
      vim.inspect(cell),
      visual_line or ""
    )
  )
end

local function run()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()

  wait_for(function()
    return find_row(buf, "blue/engine/plugins/model/src/pbr/model_store.rs") ~= nil
  end, function()
    return "model_store.rs did not render\n" .. buffer_dump(buf) .. "\n\ncalls:\n" .. table.concat(calls, "\n")
  end)

  trigger_normal_mapping("<Tab>", find_row(buf, "blue/engine/plugins/model/src/pbr/model_store.rs"))
  wait_for(function()
    return find_row(buf, "pub particle_bind_group") ~= nil
      and find_row(buf, "let particle_bind_group =") ~= nil
      and find_row(buf, "context.create_bind_group(shaders::model::particle_render") ~= nil
  end, function()
    return "model_store.rs did not expand with the broken hunk\n" .. buffer_dump(buf)
  end)

  assert_model_store_block_syntax(buf)
  assert_syntax_text(buf, "pub particle_bind_group", "pub")
  assert_syntax_text(buf, "let particle_bind_group =", "let")
  assert_syntax_text(buf, "context.create_bind_group(shaders::model::particle_render", "create_bind_group")
  assert_syntax_text(buf, "primitive_transform: primitive_transforms.binding()", "binding")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
