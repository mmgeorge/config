--- Runs git commands asynchronously behind a pluggable backend seam, owning the
--- injected backend (tests) and falling back to the editor's async process runner.
--- Routes process failures into the callback result rather than throwing.
---@class DiffReviewGitBackendModule
---@field current DiffReviewGitBackend? injected backend; nil uses the process runner
local M = { current = nil }

---@param backend DiffReviewGitBackend?
function M.set_backend(backend)
  M.current = backend
end

function M.reset_backend()
  M.current = nil
end

---@param cwd string
---@param extra_args? string[]
---@return string[]
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

---@param cwd string
---@param commit_oid string
---@return string[]
function M.git_show_diff_command(cwd, commit_oid)
  return {
    "git", "-C", cwd,
    "show", "--format=", "--no-color", "--no-ext-diff", "--unified=0", commit_oid,
  }
end

---@param stdout string?
---@param stderr string?
---@return string
function M.system_output(stdout, stderr)
  stdout = tostring(stdout or "")
  stderr = tostring(stderr or "")
  if stdout == "" then return stderr end
  if stderr == "" then return stdout end
  local separator = stdout:sub(-1) == "\n" and "" or "\n"
  return stdout .. separator .. stderr
end

---@param command DiffReviewGitCommand
---@param input? string
---@param cb DiffReviewGitTextCallback
function M.system_text_async(command, input, cb)
  local backend = M.current
  if backend and backend.system_async then
    backend.system_async(command, input, cb)
    return
  end
  if backend and backend.system then
    vim.schedule(function()
      local output, code = backend.system(command, input)
      local text = tostring(output or "")
      cb({ code = code or 0, stdout = text, stderr = "", output = text })
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
      cb({ code = result.code or 0, stdout = stdout, stderr = stderr, output = M.system_output(stdout, stderr) })
    end)
  end)
  if not ok then
    vim.schedule(function()
      local message = tostring(process)
      cb({ code = -1, stdout = "", stderr = message, output = message })
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

---@param command DiffReviewGitCommand
---@param input? string
---@param on_line fun(line: string)
---@param cb DiffReviewGitTextCallback
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

---@param command DiffReviewGitCommand
---@param cb DiffReviewGitListCallback
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

---@param path string
---@return integer code
function M.delete_path(path)
  local backend = M.current
  if backend and backend.delete then
    return backend.delete(path)
  end
  return vim.fn.delete(path)
end

--- Resolve the repository root from `cwd` so callers do not depend on Neovim's launch directory.
---@param cwd? string
---@param cb fun(root?: string, err?: string)
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

---@param cb fun(root?: string, err?: string)
function M.git_root_async(cb)
  M.git_root_at_async(nil, cb)
end

---@return string? root
---@return string? err
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

---@param root string
---@param args string[]
---@param input? string
---@param cb fun(result: DiffReviewGitCommandResult)
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

---@param args string[]
---@param input? string
---@param cb fun(result: DiffReviewGitCommandResult)
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

---@param args string[]
---@param input? string
---@return DiffReviewGitCommandResult
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
