vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local root = "D:/diffreview-binary-root"
local calls = {}
local state = {}
local original_compute_hunk_context_async = diff_review.compute_hunk_context_async
local original_compute_diff_syntax_async = diff_review.compute_diff_syntax_async

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(kind, command)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
  }
end

local function sorted_keys(map)
  local keys = {}
  for key in pairs(map or {}) do
    keys[#keys + 1] = key
  end
  table.sort(keys)
  return keys
end

local function name_status(files, status)
  local lines = {}
  for _, relpath in ipairs(sorted_keys(files)) do
    lines[#lines + 1] = status .. "\t" .. relpath
  end
  return lines
end

local function reset_state(next_state)
  state = {
    unstaged_binary = next_state.unstaged_binary or {},
    staged_binary = next_state.staged_binary or {},
    untracked_binary = next_state.untracked_binary or {},
  }
  calls = {}
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  for index, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row " .. needle .. "\n" .. table.concat(status_lines(buf), "\n"), 2)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function render_and_wait(buf, needle)
  diff_review.render_status(buf)
  wait_for(function() return buffer_contains(buf, needle) end, "status did not render " .. needle)
end

local function write_binary_file(path)
  local uv = vim.uv or vim.loop
  local fd, open_err = uv.fs_open(path, "w", 420)
  assert_true(fd ~= nil, "open binary file failed: " .. tostring(open_err))
  local ok, write_err = uv.fs_write(fd, "not text\0data", 0)
  uv.fs_close(fd)
  assert_true(ok ~= nil, "write binary file failed: " .. tostring(write_err))
end

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)

  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "binary test" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return sorted_keys(state.untracked_binary), 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return name_status(state.unstaged_binary, "M"), 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return name_status(state.staged_binary, "M"), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then
    return {}, 0
  end

  return {}, 1
end

function backend.systemlist_async(command, cb)
  record("systemlist_async", command)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "", output = "" })
  end, 5)
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function assert_binary_status(buf, file_row_text, message)
  render_and_wait(buf, file_row_text)
  trigger_normal_mapping("<Tab>", find_row(buf, file_row_text))
  assert_true(buffer_contains(buf, "No textual diff"), message .. "\n" .. table.concat(status_lines(buf), "\n"))
end

local function run()
  vim.fn.delete(root, "rf")
  vim.fn.mkdir(root, "p")
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)

  local ts_requests = 0
  local syntax_requests = 0
  diff_review.compute_hunk_context_async = function(_, _, cb)
    ts_requests = ts_requests + 1
    cb("unexpected")
  end
  diff_review.compute_diff_syntax_async = function(_, _, cb)
    syntax_requests = syntax_requests + 1
    cb(nil)
  end

  diff_review.setup()
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "Head:") end, "status did not render")

  reset_state({ unstaged_binary = { ["binary-modified.bin"] = true } })
  assert_binary_status(buf, "binary-modified.bin +0 -0", "unstaged binary file rendered textual diff")

  reset_state({ staged_binary = { ["binary-staged.bin"] = true } })
  assert_binary_status(buf, "binary-staged.bin +0 -0", "staged binary file rendered textual diff")

  write_binary_file(root .. "/binary-untracked.bin")
  reset_state({ untracked_binary = { ["binary-untracked.bin"] = true } })
  assert_binary_status(buf, "binary-untracked.bin new", "untracked binary file rendered textual diff")

  assert_true(ts_requests == 0, "binary files requested Tree-sitter context")
  assert_true(syntax_requests == 0, "binary files requested Tree-sitter diff syntax")
end

local ok, err = xpcall(run, debug.traceback)
vim.fn.delete(root, "rf")
diff_review.reset_git_backend()
gh.reset_backend()
diff_review.compute_hunk_context_async = original_compute_hunk_context_async
diff_review.compute_diff_syntax_async = original_compute_diff_syntax_async
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
