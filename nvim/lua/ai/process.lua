local M = {}

---@param stdout string?
---@param stderr string?
---@return string
local function combined_output(stdout, stderr)
  stdout = tostring(stdout or "")
  stderr = tostring(stderr or "")
  if stdout == "" then return stderr end
  if stderr == "" then return stdout end
  return stdout .. (stdout:sub(-1) == "\n" and "" or "\n") .. stderr
end

---@param request AIProcessRequest
---@param cb AIProcessCallback
function M.run(request, cb)
  local ok, process = pcall(vim.system, request.command, {
    text = true,
    stdin = request.input,
    cwd = request.cwd,
    stdout = true,
    stderr = true,
  }, function(result)
    vim.schedule(function()
      local stdout = result.stdout or ""
      local stderr = result.stderr or ""
      cb({
        code = result.code or 0,
        stdout = stdout,
        stderr = stderr,
        output = combined_output(stdout, stderr),
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

return M
