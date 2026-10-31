local M = {}

local windows_sandbox_description = {
  elevated = "Dedicated low-privilege users and filesystem ACLs. Setup needs admin approval; sandboxed commands do not run as administrator.",
  unelevated = "Restricted token for your current user, without admin setup. Weaker network isolation than Elevated; denied-read paths are unsupported.",
  mxc = "Prefer Microsoft Execution Containers on compatible Windows 11; no admin setup. Falls back to Elevated if unavailable. Child processes stop when the command exits.",
}

---@class ForgeDirectoryPolicy
---@field sandbox boolean
---@field read_access "workspace"|"full"
---@field write_access "off"|"workspace"|"full"
---@field readable_directory string[]
---@field writable_directory string[]
---@field windows_sandbox "elevated"|"unelevated"|"mxc"

---@param session {access?: ForgeDirectoryPolicy}
---@return ForgeDirectoryPolicy
function M.policy(session)
  return vim.tbl_deep_extend("force", {
    sandbox = true, read_access = "full", write_access = "workspace",
    readable_directory = {}, writable_directory = {}, windows_sandbox = "elevated",
  }, vim.deepcopy(session.access or {}))
end

---@param session {backend: string}
---@param capability? {os_sandbox?: boolean}
---@return boolean
function M.supports_sandbox(session, capability)
  return session.backend == "codex" and capability ~= nil and capability.os_sandbox == true
end

---@param policy ForgeDirectoryPolicy
---@param supported boolean
---@return string
function M.sandbox_description(policy, supported)
  if not supported then
    return "OS sandbox unavailable: Forge supports native isolation only with Codex. File-access rules and approval checks still apply to tool requests."
  end
  if not policy.sandbox then
    return "Off: commands run without OS sandbox isolation. Tool permissions and approvals still apply, but cannot contain unrestricted commands."
  end
  local boundary = policy.write_access == "off" and "commands have read-only filesystem access"
    or policy.write_access == "full" and "writes are allowed throughout the filesystem"
    or "writes stay in the workspace and extra write directories"
  return "Codex OS sandbox on: " .. boundary .. ". Network access is enabled; approval rules are separate."
end

---@param policy ForgeDirectoryPolicy
---@param supported boolean
---@return string
function M.windows_sandbox_description(policy, supported)
  local description = "Codex on Windows only. " .. assert(windows_sandbox_description[policy.windows_sandbox])
  if not supported then return "Windows mode unavailable for this provider. " .. description end
  return description
end

---@param state {session: {id: string, workspace: string, access?: ForgeDirectoryPolicy}, access_preference?: ForgeDirectoryPolicy}
---@param apply fun(policy: ForgeDirectoryPolicy, completed: fun(applied: boolean))
---@param back function
---@param operation "read"|"write"
---@return ForgeForm
function M.directories(state, apply, back, operation)
  assert(operation == "read" or operation == "write", "directory operation must be read or write")
  local field = operation == "read" and "readable_directory" or "writable_directory"
  local title = operation == "read" and "Extra read directories" or "Extra write directories"
  local session_id = state.session.id
  local function current() return state.session and state.session.id == session_id end
  local function selected_policy()
    return M.policy({ access = state.access_preference or state.session.access })
  end
  return require("forge.shared.form").open({
    title = title,
    fields = { { id = "directories", label = "One directory per line", multiline = true,
      value = table.concat(selected_policy()[field], "\n") } },
    save = function(values, completed)
      if not current() then completed("This session is no longer active.") return end
      local directories, seen = {}, {}
      for line, value in ipairs(vim.split(values.directories, "\n", { plain = true })) do
        value = vim.trim(value)
        if value ~= "" then
          local path = vim.fn.fnamemodify(value, ":p")
          local canonical = vim.uv.fs_realpath(path)
          local stat = canonical and vim.uv.fs_stat(canonical)
          if not stat or stat.type ~= "directory" then
            completed("Line " .. line .. ": directory does not exist: " .. value)
            return
          end
          local identity = vim.fn.has("win32") == 1 and canonical:lower() or canonical
          if not seen[identity] then
            directories[#directories + 1], seen[identity] = canonical, true
          end
        end
      end
      local policy = selected_policy()
      policy[field] = directories
      apply(policy, function(applied)
        if applied then completed()
        else completed("Could not save directory access. Your draft is retained.") end
      end)
    end,
    reload = function(completed)
      if not current() then completed(nil, "This session is no longer active.") return end
      completed({ directories = table.concat(selected_policy()[field], "\n") })
    end,
    close = function() if current() then back() end end,
  })
end

return M
