--- Runs git commands asynchronously behind a pluggable backend seam, owning the
--- injected backend (tests) and falling back to the editor's async process runner.
--- Routes process failures into the callback result rather than throwing.
---@class DiffReviewGitBackendModule
---@field current DiffReviewGitBackend? injected backend; nil uses the process runner
---@field debug_request_id integer
local M = { current = nil, debug_request_id = 0 }

local trace = require("diff_review.infra.perf_trace")

--- Injects a custom or mock Git command execution backend.
---@param backend DiffReviewGitBackend? Mock backend implementation, or nil to use default process runner.
function M.set_backend(backend)
  M.current = backend
end

--- Resets the active Git execution backend to the default process runner.
function M.reset_backend()
  M.current = nil
end

--- Constructs the command arguments array for generating a standard unified diff.
---@param cwd string Working directory path.
---@param extra_args? string[] Optional additional CLI arguments passed to `git diff`.
---@return string[] command Full argument array for command execution.
function M.git_diff_command(cwd, extra_args)
  local command = {
    "git", "-C", cwd,
    "-c", "core.quotepath=false",
    "diff", "--no-color", "--no-ext-diff", "--unified=0",
  }
  for _, arg in ipairs(extra_args or {}) do
    command[#command + 1] = arg
  end
  return command
end

--- Constructs the command arguments array for displaying a commit's unified diff.
---@param cwd string Working directory path.
---@param commit_oid string Target Git commit object ID.
---@return string[] command Full argument array for `git show`.
function M.git_show_diff_command(cwd, commit_oid)
  return {
    "git", "-C", cwd,
    "show", "--format=", "--no-color", "--no-ext-diff", "--unified=0", commit_oid,
  }
end

--- Combines standard output and error output into a single unified text string.
---@param stdout string? Standard output content.
---@param stderr string? Standard error content.
---@return string output Combined output string.
function M.system_output(stdout, stderr)
  stdout = tostring(stdout or "")
  stderr = tostring(stderr or "")
  if stdout == "" then return stderr end
  if stderr == "" then return stdout end
  local separator = stdout:sub(-1) == "\n" and "" or "\n"
  return stdout .. separator .. stderr
end

--- Executes an asynchronous process command, capturing stdout, stderr, and exit code.
--- Invokes `cb` on the Neovim event loop once the process exits.
---@param command DiffReviewGitCommand Command arguments array.
---@param input? string Optional standard input text passed to the process.
---@param cb DiffReviewGitTextCallback Callback receiving the command result table.
function M.system_text_async(command, input, cb)
  M.debug_request_id = M.debug_request_id + 1
  local request_id = M.debug_request_id
  local started = vim.uv.hrtime()
  trace.event("git.command.start", nil, {
    request_id = request_id,
    command = command,
    input_bytes = input and #input or 0,
  })

  ---@param result DiffReviewGitCommandResult
  local function finish(result)
    trace.event("git.command.done", nil, {
      request_id = request_id,
      command = command,
      code = result.code,
      elapsed_ms = math.floor((vim.uv.hrtime() - started) / 1000000),
      stdout_bytes = #(result.stdout or ""),
      stderr_bytes = #(result.stderr or ""),
    })
    cb(result)
  end

  local backend = M.current
  if backend and backend.system_async then
    backend.system_async(command, input, finish)
    return
  end
  if backend and backend.system then
    vim.schedule(function()
      local output, code = backend.system(command, input)
      local text = tostring(output or "")
      finish({ code = code or 0, stdout = text, stderr = "", output = text })
    end)
    return
  end

  local ok, process = pcall(vim.system, command, {
    text = true,
    stdin = input,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      finish({ code = result.code or 0, stdout = stdout, stderr = stderr, output = M.system_output(stdout, stderr) })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process)
      finish({ code = -1, stdout = "", stderr = message, output = message })
    end)
  end
end

---@param data string|string[]?
---@return string
local function normalize_system_chunk(data)
  if type(data) == "table" then
    return table.concat(data, "\n")
  end
  return tostring(data or "")
end

--- Executes an asynchronous process, streaming completed lines incrementally to `on_line`.
--- Invokes `cb` with complete aggregated output once the process exits.
---@param command DiffReviewGitCommand Command arguments array.
---@param input? string Optional standard input text passed to the process.
---@param on_line fun(line: string) Callback receiving trimmed lines as they arrive.
---@param cb DiffReviewGitTextCallback Final callback receiving the full command result table.
function M.system_text_stream_async(command, input, on_line, cb)
  local backend = M.current
  if backend and backend.system_stream_async then
    backend.system_stream_async(command, input, on_line, cb)
    return
  end
  if backend and backend.system_async then
    backend.system_async(command, input, cb, on_line)
    return
  end
  if backend and backend.system then
    vim.schedule(function()
      local output, code = backend.system(command, input)
      local text = tostring(output or "")
      text = text:gsub("\r\n", "\n")
      if text:sub(-1) == "\n" then text = text:sub(1, -2) end
      local lines = text == "" and {} or vim.split(text, "\n", { plain = true })
      for _, line in ipairs(lines) do
        if line ~= "" then on_line(line) end
      end
      cb({ code = code or 0, stdout = text, stderr = "", output = text })
    end)
    return
  end

  local stdout = {}
  local stderr = {}
  local pending = { stdout = "", stderr = "" }

  ---@param stream "stdout"|"stderr"
  ---@param data string|string[]?
  local function collect(stream, data)
    local text = normalize_system_chunk(data)
    if text == "" then return end
    local chunks = stream == "stdout" and stdout or stderr
    chunks[#chunks + 1] = text

    text = pending[stream] .. text:gsub("\r", "\n")
    local parts = vim.split(text, "\n", { plain = true })
    pending[stream] = table.remove(parts) or ""
    for _, line in ipairs(parts) do
      line = vim.trim(line)
      if line ~= "" then vim.schedule(function() on_line(line) end) end
    end
  end

  local ok, process = pcall(vim.system, command, {
    text = true,
    stdin = input,
    stdout = function(_, data)
      collect("stdout", data)
    end,
    stderr = function(_, data)
      collect("stderr", data)
    end,
  }, function(result)
    vim.schedule(function()
      for _, stream in ipairs({ "stdout", "stderr" }) do
        local line = vim.trim(pending[stream] or "")
        if line ~= "" then on_line(line) end
      end
      local stdout_text = table.concat(stdout)
      local stderr_text = table.concat(stderr)
      cb({
        code = result.code or 0,
        stdout = stdout_text,
        stderr = stderr_text,
        output = M.system_output(stdout_text, stderr_text),
      })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process)
      cb({ code = -1, stdout = "", stderr = message, output = message })
    end)
  end
end

---@param text string
---@return string[]
local function text_to_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text:sub(-1) == "\n" then
    text = text:sub(1, -2)
  end
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end
M.text_to_lines = text_to_lines

--- Executes an asynchronous command, parsing standard output into a list of lines.
---@param command DiffReviewGitCommand Command arguments array.
---@param cb DiffReviewGitListCallback Callback receiving lines array, exit code, and combined output.
function M.systemlist_async(command, cb)
  local backend = M.current
  if backend and backend.systemlist_async then
    backend.systemlist_async(command, cb)
    return
  end
  if backend and backend.systemlist then
    vim.schedule(function()
      local output, code = backend.systemlist(command)
      if type(output) == "string" then
        output = text_to_lines(output)
      end
      cb(output or {}, code or 0, "")
    end)
    return
  end

  M.system_text_async(command, nil, function(result)
    cb(text_to_lines(result.stdout), result.code, result.output)
  end)
end

--- Removes a file or directory path through the active backend.
---@param path string File or directory path to remove.
---@return integer code 0 on success or nonzero error code.
function M.delete_path(path)
  local backend = M.current
  if backend and backend.delete then
    return backend.delete(path)
  end
  return vim.fn.delete(path)
end

--- Resolves the Git repository root directory asynchronously from a target directory path.
---@param cwd? string Working directory to inspect, or nil for Neovim current directory.
---@param cb fun(root?: string, err?: string) Callback receiving resolved root path or error.
function M.git_root_at_async(cwd, cb)
  local command = { "git" }
  if cwd and cwd ~= "" then
    vim.list_extend(command, { "-C", cwd })
  end
  vim.list_extend(command, { "rev-parse", "--show-toplevel" })
  M.systemlist_async(command, function(output, code, stderr)
    local root = output[1]
    if code ~= 0 or not root or root == "" then
      local message = vim.trim(stderr or "")
      cb(nil, message ~= "" and message or "Not a git repository")
      return
    end
    cb(vim.trim(root), nil)
  end)
end

--- Resolves the Git repository root directory asynchronously for the current working directory.
---@param cb fun(root?: string, err?: string) Callback receiving resolved root path or error.
function M.git_root_async(cb)
  M.git_root_at_async(nil, cb)
end

--- Synchronously resolves the Git repository root when running with a test backend.
---@return string? root Repository root path, or nil.
---@return string? err Error message when resolution fails or backend is not active.
function M.git_root_sync_for_test_backend()
  local backend = M.current
  if not (backend and backend.systemlist) then
    return nil, "Synchronous git root is unavailable"
  end
  local output, code = backend.systemlist({ "git", "rev-parse", "--show-toplevel" })
  if type(output) == "string" then output = text_to_lines(output) end
  local root = output and output[1]
  if code ~= 0 or not root or root == "" then
    return nil, "Not a git repository"
  end
  return vim.trim(root), nil
end

--- Executes a Git command asynchronously rooted at a specific repository root directory.
---@param root string Git repository root path.
---@param args string[] Git command arguments (e.g. `{"add", "file.txt"}`).
---@param input? string Optional standard input passed to Git.
---@param cb fun(result: DiffReviewGitCommandResult) Callback receiving structured execution result.
function M.run_git_at_root_async(root, args, input, cb)
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  M.system_text_async(command, input, function(result)
    cb({
      ok = result.code == 0,
      code = result.code,
      output = vim.trim(result.output or ""),
      stdout = result.stdout,
      stderr = result.stderr,
      root = root,
      args = args,
    })
  end)
end

--- Resolves the Git root directory and executes a Git command asynchronously.
---@param args string[] Git command arguments.
---@param input? string Optional standard input text.
---@param cb fun(result: DiffReviewGitCommandResult) Callback receiving execution result.
function M.run_git_async(args, input, cb)
  M.git_root_async(function(root, root_err)
    if not root then
      cb({
        ok = false,
        code = -1,
        output = root_err or "Unable to find git root",
        args = args,
      })
      return
    end
    M.run_git_at_root_async(root, args, input, cb)
  end)
end

--- Synchronously executes a Git command when running under a test backend.
---@param args string[] Git command arguments.
---@param input? string Optional standard input text.
---@return DiffReviewGitCommandResult result Command execution result table.
function M.run_git_sync_for_test_backend(args, input)
  local root, root_err = M.git_root_sync_for_test_backend()
  if not root then
    return {
      ok = false,
      code = -1,
      output = root_err or "Unable to find git root",
      args = args,
    }
  end
  local backend = M.current
  if not (backend and backend.system) then
    return {
      ok = false,
      code = -1,
      output = "Synchronous git is unavailable",
      root = root,
      args = args,
    }
  end
  local command = { "git", "-C", root }
  vim.list_extend(command, args)
  local output, code = backend.system(command, input)
  return {
    ok = (code or 0) == 0,
    code = code or 0,
    output = vim.trim(tostring(output or "")),
    root = root,
    args = args,
  }
end

return M
