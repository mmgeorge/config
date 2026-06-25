--- Tracks the editable "Issues:" head line for the GitStatus view, persisting the
--- referenced issue numbers to the repo `.gitstatus` file and keeping the buffer line
--- modifiable only while the cursor rests on it.
---
--- Reads buffer state through the active status registry and routes head-line patches
--- back through the render core, so the issues line stays in sync without owning render.
---@class DiffReviewStatusIssuesModule
local M = {}

local notifications = require("diff_review.infra.notifications")

--- Resolve the status state for a buffer from the active registry, falling back to the
--- current singleton, so issue edits target the right buffer's state.
---@param buf integer
---@return table?
local function status_for_buf(buf)
  local diff_review = require("diff_review")
  return diff_review._status_states and diff_review._status_states[buf] or diff_review._status
end

---@param cwd string?
---@return string?
function M.path(cwd)
  if not cwd or cwd == "" then return nil end
  return vim.fs.joinpath(cwd, ".gitstatus")
end

---@param values any
---@return integer[]
function M.normalize_numbers(values)
  local numbers = {}
  local seen = {}
  if type(values) ~= "table" then return numbers end
  for _, value in ipairs(values) do
    local number = tonumber(value)
    if number and number > 0 and math.floor(number) == number and not seen[number] then
      seen[number] = true
      numbers[#numbers + 1] = number
    end
  end
  table.sort(numbers)
  return numbers
end

---@param line string?
---@return integer[]
function M.parse_line(line)
  local body = tostring(line or ""):gsub("^%s*Issues:%s*", "")
  body = vim.trim(body)
  if body == "" or body:lower() == "none" then return {} end
  local values = {}
  local seen_text = {}
  for issue_text in body:gmatch("#%s*(%d+)") do
    if not seen_text[issue_text] then
      seen_text[issue_text] = true
      values[#values + 1] = issue_text
    end
  end
  for issue_text in body:gmatch("%f[%d](%d+)%f[%D]") do
    if not seen_text[issue_text] then
      seen_text[issue_text] = true
      values[#values + 1] = issue_text
    end
  end
  return M.normalize_numbers(values)
end

---@param numbers integer[]?
---@return string
function M.text(numbers)
  numbers = M.normalize_numbers(numbers)
  if #numbers == 0 then return "none" end
  local parts = {}
  for _, number in ipairs(numbers) do
    parts[#parts + 1] = "#" .. tostring(number)
  end
  return table.concat(parts, " ")
end

---@param left integer[]?
---@param right integer[]?
---@return boolean
function M.equal(left, right)
  left = M.normalize_numbers(left)
  right = M.normalize_numbers(right)
  if #left ~= #right then return false end
  for index, number in ipairs(left) do
    if number ~= right[index] then return false end
  end
  return true
end

---@param cwd string
---@return table
function M.read_state(cwd)
  local path = M.path(cwd)
  if not path or vim.fn.filereadable(path) ~= 1 then
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local read_ok, lines = pcall(vim.fn.readfile, path)
  if not read_ok then
    vim.notify("GitStatus state read failed: " .. tostring(lines), vim.log.levels.WARN, { title = "GitStatus" })
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local content = table.concat(lines or {}, "\n")
  if vim.trim(content) == "" then
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local decode_ok, decoded = pcall(vim.json.decode, content)
  if not (decode_ok and type(decoded) == "table") then
    vim.notify("GitStatus state read failed: invalid .gitstatus JSON", vim.log.levels.WARN, { title = "GitStatus" })
    return { cwd = cwd, numbers = {}, saved_numbers = {} }
  end
  local numbers = M.normalize_numbers(decoded.issues)
  return { cwd = cwd, numbers = numbers, saved_numbers = vim.deepcopy(numbers), data = decoded }
end

---@param status table?
---@param cwd string?
---@return table?
function M.ensure_state(status, cwd)
  if not (status and cwd and cwd ~= "") then return nil end
  if status.issues and status.issues.cwd == cwd then return status.issues end
  status.issues = M.read_state(cwd)
  return status.issues
end

---@param cwd string
---@param numbers integer[]
---@param existing table?
---@return boolean
---@return string?
function M.write(cwd, numbers, existing)
  local path = M.path(cwd)
  if not path then return false, "missing git root" end
  local data = type(existing) == "table" and vim.deepcopy(existing) or {}
  if vim.fn.filereadable(path) == 1 and not existing then
    local read_ok, lines = pcall(vim.fn.readfile, path)
    if not read_ok then return false, tostring(lines) end
    local content = table.concat(lines or {}, "\n")
    if vim.trim(content) ~= "" then
      local decode_ok, decoded = pcall(vim.json.decode, content)
      if not (decode_ok and type(decoded) == "table") then return false, "invalid .gitstatus JSON" end
      data = decoded
    end
  end
  data.issues = M.normalize_numbers(numbers)
  local write_ok, write_result = pcall(vim.fn.writefile, { vim.json.encode(data) }, path)
  if not write_ok then return false, tostring(write_result) end
  if write_result ~= 0 then return false, "writefile returned " .. tostring(write_result) end
  return true, nil
end

---@param issues_state table?
---@return DiffReviewStatusHeadLine
function M.head_line(issues_state)
  local numbers = issues_state and issues_state.numbers or {}
  local text = M.text(numbers)
  return {
    segments = {
      { ("%-8s"):format("Issues:"), "DiffReviewStatusLabel" },
      { text, text == "none" and "Comment" or "DiffReviewStatusPR" },
    },
    entry = { id = "issues", kind = "issues", issues = issues_state },
  }
end

---@param status table
---@param head_line DiffReviewStatusHeadLine
function M.replace_head_line(status, head_line)
  if not (status and status.head_lines) then return end
  for index, line in ipairs(status.head_lines) do
    if line.entry and line.entry.id == "issues" then
      status.head_lines[index] = head_line
      return
    end
  end
end

---@param buf integer
---@return boolean
function M.patch_line(buf)
  local status = status_for_buf(buf)
  if not status then return false end
  local head_line = M.head_line(status.issues)
  M.replace_head_line(status, head_line)
  return require("diff_review")._status_patch_head_line(buf, "issues", head_line)
end

---@param buf integer
---@return integer?
function M.row(buf)
  local status = status_for_buf(buf)
  if status and status.entries then
    for row, entry in pairs(status.entries) do
      if entry and entry.id == "issues" then return row end
    end
  end
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return nil end
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:match("^Issues:%s*") then return index end
  end
  return nil
end

---@param buf integer
---@return string
function M.current_line(buf)
  local row = M.row(buf)
  if not row then return "" end
  return vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
end

---@param buf integer
---@return integer[]
function M.current_numbers(buf)
  return M.parse_line(M.current_line(buf))
end

---@param buf integer
---@return boolean
function M.cursor_on_row(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local row = M.row(buf)
  return row ~= nil and vim.api.nvim_win_get_cursor(0)[1] == row
end

---@param buf integer
function M.refresh_modified(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if vim.b[buf].diff_review_status_rendering then return end
  local status = status_for_buf(buf)
  if not (status and status.view_kind == "status") then return end
  local issues_state = status.issues or M.ensure_state(status, status.cwd)
  local saved_numbers = issues_state and issues_state.saved_numbers or {}
  vim.bo[buf].modified = not M.equal(M.current_numbers(buf), saved_numbers)
end

---@param buf integer
function M.sync_modifiable(buf)
  return require("diff_review")._status_perf_span("status_issues.sync_modifiable", buf, nil, function()
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
    if vim.b[buf].diff_review_status_rendering then return end
    local status = status_for_buf(buf)
    if not (status and status.view_kind == "status") then return end
    vim.bo[buf].modifiable = M.cursor_on_row(buf)
  end)
end

---@param buf integer
function M.save(buf)
  local status = status_for_buf(buf)
  if not (status and status.view_kind == "status" and status.cwd) then
    notifications.error("GitStatus state save failed: not in a git status buffer", "GitStatus")
    return
  end
  local issues_state = status.issues or M.ensure_state(status, status.cwd) or {}
  local numbers = M.current_numbers(buf)
  local ok, err = M.write(status.cwd, numbers, issues_state.data)
  if not ok then
    notifications.error("GitStatus state save failed: " .. tostring(err), "GitStatus")
    vim.bo[buf].modified = true
    M.sync_modifiable(buf)
    return
  end
  issues_state.cwd = status.cwd
  issues_state.numbers = vim.deepcopy(numbers)
  issues_state.saved_numbers = vim.deepcopy(numbers)
  issues_state.data = issues_state.data or {}
  issues_state.data.issues = vim.deepcopy(numbers)
  status.issues = issues_state
  M.patch_line(buf)
  vim.bo[buf].modified = false
  M.sync_modifiable(buf)
end

---@param buf integer
function M.attach(buf)
  if vim.b[buf].diff_review_status_issues_attached then return end
  vim.b[buf].diff_review_status_issues_attached = true
  vim.bo[buf].buftype = "acwrite"
  local group = vim.api.nvim_create_augroup("DiffReviewStatusIssues" .. tostring(buf), { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "CursorMovedI", "ModeChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      require("diff_review")._status_perf_span("status_issues.autocmd_sync_modifiable", buf, nil, function()
        M.sync_modifiable(buf)
      end)
    end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "InsertLeave" }, {
    group = group,
    buffer = buf,
    callback = function()
      M.refresh_modified(buf)
      M.sync_modifiable(buf)
    end,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buf,
    callback = function()
      M.save(buf)
    end,
  })
  vim.keymap.set("i", "<CR>", function()
    if M.cursor_on_row(buf) then return "" end
    return "\r"
  end, { buffer = buf, expr = true, desc = "Keep GitStatus issue list on one line" })
end

return M
