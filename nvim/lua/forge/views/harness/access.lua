local M = {}
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")

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

---@param state {session: {id: string, workspace: string, access?: ForgeDirectoryPolicy}}
---@param host {control_win?: integer}
---@param apply fun(policy: ForgeDirectoryPolicy, completed: fun())
---@param back function
---@param operation "read"|"write"
function M.directories(state, host, apply, back, operation)
  assert(operation == "read" or operation == "write", "directory operation must be read or write")
  local field = operation == "read" and "readable_directory" or "writable_directory"
  local label = operation == "read" and "readable" or "writable"
  local scope = operation == "read" and "read_access" or "write_access"
  local session_id = state.session.id
  local function current() return state.session and state.session.id == session_id end
  local open
  local function edit(index)
    local policy = M.policy(state.session)
    picker.close(false)
    vim.ui.input({ prompt = label:gsub("^%l", string.upper) .. " directory: ", default = policy[field][index] or "", completion = "dir" }, function(value)
      if not current() then return end
      if not value then open() return end
      value = vim.trim(value)
      if value == "" then open() return end
      local path = vim.fn.fnamemodify(value, ":p")
      local canonical = vim.uv.fs_realpath(path)
      local stat = canonical and vim.uv.fs_stat(canonical)
      if not stat or stat.type ~= "directory" then
        notifications.error("Directory does not exist: " .. value, "Harness access")
        open()
        return
      end
      policy[field][index] = canonical
      apply(policy, open)
    end)
  end
  local function remove(context)
    local index = context.option and context.option.directory_index
    if not index then return end
    local policy = M.policy(state.session)
    picker.close(false)
    picker.open({ owner = "harness-directory-remove", host = host,
      page_list = { { id = "remove", title = "Remove " .. label .. " directory?", subtitle = policy[field][index],
        option_list = { { id = "cancel", label = "Keep directory" }, { id = "remove", label = "Remove directory" } } } },
      on_close = open,
      on_confirm = function(result)
        if not current() then return end
        if result.option.id == "remove" then
          table.remove(policy[field], index)
          apply(policy, open)
        else open() end
        return false
      end,
    })
  end
  open = function()
    if not current() then return end
    local policy = M.policy(state.session)
    local options = { { id = "add", label = "+ Add directory…" } }
    local function normalize(path)
      path = vim.uv.fs_realpath(path) or path
      path = path:gsub("\\", "/"):gsub("/$", "")
      return vim.fn.has("win32") == 1 and path:lower() or path
    end
    local roots = { normalize(state.session.workspace or "") }
    for _, directory in ipairs(policy[field]) do roots[#roots + 1] = normalize(directory) end
    for index, directory in ipairs(policy[field]) do
      local path, covered = normalize(directory), false
      for root_index, root in ipairs(roots) do
        if root_index ~= index + 1 and (path == root or path:sub(1, #root + 1) == root .. "/") then covered = true end
      end
      options[#options + 1] = { id = tostring(index), directory_index = index, label = directory,
        detail = covered and "Already covered" or nil }
    end
    picker.close(false)
    picker.open({ owner = "harness-" .. label .. "-directories", host = host,
      page_list = { { id = "directories", title = "Additional " .. label .. " directories",
        subtitle = policy[scope] == "off" and "Retained grants are inactive while writing is off"
          or policy[scope] == "full" and "Already covered by full " .. operation .. " access" or "Allowed alongside the workspace · independent of sandbox",
        option_list = options, footer = "Enter add/edit  d remove  q back" } },
      action_list = { { id = "remove", key = "d", callback = remove } },
      on_close = back,
      on_confirm = function(result)
        edit(result.option.directory_index or #policy[field] + 1)
        return false
      end,
    })
  end
  open()
end

return M
