vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local root = "D:/mock/project"
local original_compute_hunk_context_async = diff_review.compute_hunk_context_async
local original_compute_diff_syntax_async = diff_review.compute_diff_syntax_async

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local unstaged_diff = table.concat({
  "diff --git a/a.txt b/a.txt",
  "index 1111111..2222222 100644",
  "--- a/a.txt",
  "+++ b/a.txt",
  "@@ -1 +1 @@",
  "-one",
  "+two",
  "diff --git a/b.txt b/b.txt",
  "index 3333333..4444444 100644",
  "--- a/b.txt",
  "+++ b/b.txt",
  "@@ -1 +1 @@",
  "-red",
  "+blue",
}, "\n")

local staged_diff = unstaged_diff
local staged = false
local calls = {}

---@class DiffReviewMockCall
---@field kind "systemlist"|"system"|"systemlist_async"|"system_async"
---@field command DiffReviewGitCommand
---@field key string
---@field input? string

local function record(kind, command, input)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

---@type DiffReviewGitBackend
local backend = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

---@param command DiffReviewGitCommand
---@return string[] output
---@return integer code
function backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)

  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { "abc1234" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { "mock subject" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{upstream}" then
    return { "origin/master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{upstream}" then
    return { "def5678" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{upstream}" then
    return { "upstream subject" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{push}" then
    return { "origin/master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{push}" then
    return { "def5678" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{push}" then
    return { "push subject" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return { "c.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return staged and { "M\ta.txt", "M\tb.txt" } or {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return staged and {} or { "M\ta.txt", "M\tb.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=3" then
    return vim.split(staged and "" or unstaged_diff, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=3\t--cached" then
    return vim.split(staged and staged_diff or "", "\n", { plain = true }), 0
  end

  return {}, 1
end

---@param command DiffReviewGitCommand
---@param cb DiffReviewGitListCallback
function backend.systemlist_async(command, cb)
  record("systemlist_async", command)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

---@param command DiffReviewGitCommand
---@param input? string
---@return string output
---@return integer code
function backend.system(command, input)
  record("system", command, input)
  local key = command_key(command)
  if key == "git\t-C\t" .. root .. "\tadd\t-u\t--\ta.txt" then
    staged = true
    return "", 0
  end
  if key == "git\t-C\t" .. root .. "\tadd\t-u\t--\tb.txt" then
    staged = true
    return "", 0
  end
  if key == "git\t-C\t" .. root .. "\trestore\t--staged\t--\ta.txt" then
    staged = false
    return "", 0
  end
  if key == "git\t-C\t" .. root .. "\trestore\t--staged\t--\tb.txt" then
    staged = false
    return "", 0
  end
  return "unexpected command: " .. key, 1
end

---@param command DiffReviewGitCommand
---@param input? string
---@param cb DiffReviewGitTextCallback
function backend.system_async(command, input, cb)
  record("system_async", command, input)
  vim.defer_fn(function()
    local output, code = backend.system(command, input)
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 5)
end

function backend.delete()
  return 0
end

local function contains_line(lines, pattern)
  for _, line in ipairs(lines) do
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

local function buffer_contains(buf, pattern)
  return contains_line(vim.api.nvim_buf_get_lines(buf, 0, -1, false), pattern)
end

local function wait_for(condition, message)
  assert_true(vim.wait(1000, condition, 10), message)
end

local function count_blank_lines_between(lines, first_heading, second_heading)
  local first_index = nil
  local second_index = nil
  for index, line in ipairs(lines) do
    if line:find(first_heading, 1, true) then
      first_index = index
    elseif line:find(second_heading, 1, true) then
      second_index = index
      break
    end
  end
  assert_true(first_index ~= nil, "missing heading: " .. first_heading)
  assert_true(second_index ~= nil, "missing heading: " .. second_heading)
  local blanks = 0
  for index = first_index + 1, second_index - 1 do
    if lines[index] == "" then blanks = blanks + 1 end
  end
  return blanks
end

local function find_rows(lines, first_pattern, second_pattern)
  local first_row = nil
  local second_row = nil
  for index, line in ipairs(lines) do
    if line:find(first_pattern, 1, true) then
      first_row = index
    elseif line:find(second_pattern, 1, true) then
      second_row = index
    end
  end
  assert_true(first_row ~= nil, "missing row: " .. first_pattern .. "\n" .. table.concat(lines, "\n"))
  assert_true(second_row ~= nil, "missing row: " .. second_pattern .. "\n" .. table.concat(lines, "\n"))
  return first_row, second_row
end

local function trigger_visual_mapping(key, first_row, second_row)
  vim.fn.setpos("'<", { 0, first_row, 1, 0 })
  vim.fn.setpos("'>", { 0, second_row, 1, 0 })
  local mapping = vim.fn.maparg(key, "x", false, true)
  assert_true(type(mapping.callback) == "function", "missing visual mapping for " .. key)
  mapping.callback()
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function saw_system_call(expected_key)
  for _, call in ipairs(calls) do
    if call.kind == "system" and call.key == expected_key then
      return true
    end
  end
  return false
end

local function run()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  local ts_requests = 0
  local syntax_requests = 0
  diff_review.compute_hunk_context_async = function(_, _, cb)
    ts_requests = ts_requests + 1
    vim.defer_fn(function()
      cb({
        label = "AsyncScope",
        start_row = 0,
        end_row = 0,
        start_text = "AsyncScope",
        end_text = "AsyncScope",
      })
    end, 5)
  end
  diff_review.compute_diff_syntax_async = function(filename, lines_for_syntax, cb)
    syntax_requests = syntax_requests + 1
    original_compute_diff_syntax_async(filename, lines_for_syntax, cb)
  end
  diff_review.setup({ about_auto_generate = false })
  vim.wo.number = true
  vim.wo.relativenumber = true
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  assert_true(vim.bo[buf].filetype == "GitStatus", "GitStatus buffer did not open")
  assert_true(not vim.wo.number and not vim.wo.relativenumber, "GitStatus buffer should hide line numbers")

  local real_buf = vim.api.nvim_create_buf(true, false)
  vim.bo[real_buf].filetype = "lua"
  vim.api.nvim_win_set_buf(0, real_buf)
  assert_true(vim.wo.number and vim.wo.relativenumber, "leaving GitStatus buffer should restore line numbers")
  vim.api.nvim_win_set_buf(0, buf)
  assert_true(not vim.wo.number and not vim.wo.relativenumber, "re-entering GitStatus buffer should hide line numbers")

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(lines, "Loading DiffReview..."), "DiffReview did not render a loading state before async data completed")
  wait_for(function() return buffer_contains(buf, "Head:") end, "status did not render after async load")
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(lines, "Head:"), "missing Head row")
  assert_true(contains_line(lines, "master"), "missing branch row")
  assert_true(contains_line(lines, "Unstaged changes (2)"), "missing unstaged heading")
  assert_true(contains_line(lines, "Untracked files (1)"), "missing untracked heading")
  assert_true(contains_line(lines, "Modified " .. root .. "/a.txt +1 -1"), "missing modified a.txt prefix")
  assert_true(contains_line(lines, "Modified " .. root .. "/b.txt +1 -1"), "missing modified b.txt prefix")
  assert_true(contains_line(lines, "New      " .. root .. "/c.txt new"), "missing new c.txt prefix")
  assert_true(diff_review._status_file_change_label({ git_status = "D" }) == "Removed", "deleted file label should render as Removed")
  assert_true(contains_line(lines, "a.txt +1 -1"), "missing a.txt file row")
  assert_true(contains_line(lines, "b.txt +1 -1"), "missing b.txt file row")
  assert_true(contains_line(lines, "c.txt new"), "missing untracked file row")
  assert_true(count_blank_lines_between(lines, "Unstaged changes", "Untracked files") == 1, "wrong heading spacing")

  local first_row, second_row = find_rows(lines, "a.txt +1 -1", "b.txt +1 -1")
  vim.api.nvim_win_set_cursor(0, { first_row, 0 })
  vim.cmd("doautocmd <nomodeline> CursorMoved")
  wait_for(function() return syntax_requests > 0 end, "file row cursor movement did not prewarm diff syntax")
  trigger_normal_mapping("<Tab>", first_row)
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(lines, "@@ +1 -1"), "missing fallback hunk header before async treesitter context")
  wait_for(function() return buffer_contains(buf, "AsyncScope") end, "async treesitter context did not render boundary row")
  assert_true(ts_requests > 0, "treesitter async context was not requested")
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  first_row, second_row = find_rows(lines, "a.txt +1 -1", "b.txt +1 -1")
  trigger_normal_mapping("<Tab>", first_row)
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  first_row, second_row = find_rows(lines, "a.txt +1 -1", "b.txt +1 -1")

  trigger_visual_mapping("S", first_row, second_row)
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(lines, "Staged changes (2)"), "optimistic stage did not update status immediately")
  assert_true(not contains_line(lines, "Loading DiffReview..."), "stage action replaced status UI with loading state")
  first_row, second_row = find_rows(lines, "a.txt +1 -1", "b.txt +1 -1")
  trigger_visual_mapping("U", first_row, second_row)
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(lines, "Unstaged changes (2)"), "optimistic unstage did not update status immediately")
  assert_true(not contains_line(lines, "Loading DiffReview..."), "unstage action replaced status UI with loading state")

  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\ta.txt")
      and saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tb.txt")
  end, "stage commands did not run")
  assert_true(saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\ta.txt"), "missing git add -u for a.txt")
  assert_true(saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tb.txt"), "missing git add -u for b.txt")
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\ta.txt")
      and saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tb.txt")
  end, "unstage commands did not run")
  assert_true(saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\ta.txt"), "missing git restore for a.txt")
  assert_true(saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tb.txt"), "missing git restore for b.txt")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2)") end, "status did not reconcile after queued stage/unstage")

  local discard_mapping = vim.fn.maparg("j", "x", false, true)
  assert_true(type(discard_mapping.callback) == "function", "missing visual discard mapping")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
diff_review.compute_hunk_context_async = original_compute_hunk_context_async
diff_review.compute_diff_syntax_async = original_compute_diff_syntax_async
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
