local M = {}
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")

function M.policy(session)
  return vim.deepcopy(session.access or {
    sandbox = true, write_access = "workspace", writable_directory = {}, windows_sandbox = "elevated",
  })
end

---@param state table
---@param host table
---@param apply fun(policy: table, completed: function)
---@param back function
function M.directories(state, host, apply, back)
  local session_id = state.session.id
  local function current() return state.session and state.session.id == session_id end
  local open
  local function edit(index)
    local policy = M.policy(state.session)
    picker.close(false)
    vim.ui.input({ prompt = "Writable directory: ", default = policy.writable_directory[index] or "", completion = "dir" }, function(value)
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
      policy.writable_directory[index] = canonical
      apply(policy, open)
    end)
  end
  local function remove(context)
    local index = context.option and context.option.directory_index
    if not index then return end
    local policy = M.policy(state.session)
    picker.close(false)
    picker.open({ owner = "harness-directory-remove", host = host,
      page_list = { { id = "remove", title = "Remove writable directory?", subtitle = policy.writable_directory[index],
        option_list = { { id = "cancel", label = "Keep directory" }, { id = "remove", label = "Remove directory" } } } },
      on_close = open,
      on_confirm = function(result)
        if not current() then return end
        if result.option.id == "remove" then
          table.remove(policy.writable_directory, index)
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
      path = path:gsub("\\", "/"):gsub("/$", "")
      return vim.fn.has("win32") == 1 and path:lower() or path
    end
    local roots = { normalize(state.session.workspace or "") }
    for _, directory in ipairs(policy.writable_directory) do roots[#roots + 1] = normalize(directory) end
    for index, directory in ipairs(policy.writable_directory) do
      local path, covered = normalize(directory), false
      for root_index, root in ipairs(roots) do
        if root_index ~= index + 1 and (path == root or path:sub(1, #root + 1) == root .. "/") then covered = true end
      end
      options[#options + 1] = { id = tostring(index), directory_index = index, label = directory,
        detail = covered and "Already covered" or nil }
    end
    picker.close(false)
    picker.open({ owner = "harness-writable-directories", host = host,
      page_list = { { id = "directories", title = "Additional writable directories",
        subtitle = not policy.sandbox and "Inactive · sandbox disabled" or policy.write_access == "full" and "Inactive · full write access" or "Saved for this workspace",
        option_list = options, footer = "Enter add/edit  d remove  q back" } },
      action_list = { { id = "remove", key = "d", callback = remove } },
      on_close = back,
      on_confirm = function(result)
        edit(result.option.directory_index or #policy.writable_directory + 1)
        return false
      end,
    })
  end
  open()
end

return M
