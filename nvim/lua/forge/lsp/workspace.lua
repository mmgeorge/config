local source = require("forge.lsp.source")
local roots = require("forge.lsp.root")
local M = {}
local excluded = { [".git"] = true, node_modules = true, target = true, vendor = true, build = true, dist = true, [".cache"] = true }
local languages = { rust = { "rust" }, typescript = { "typescript", "javascript" }, lua = { "lua" } }

---@class ForgeLspWorkspace
---@field path string Project directory, absolute after host authorization.
---@field language "rust"|"typescript"|"lua"

---Collect manifest directories and resolve Lua source roots without launching servers.
---@return fun() cancel
local function inventory(root, authorize, active, receive)
  local root_stat = vim.uv.fs_stat(root)
  if not root_stat or root_stat.type ~= "directory" then receive(nil, "invalid_path: workspace listing requires a directory") return function() end end
  local queue, cursor, entries, seen, warnings = { source.canonical(root) }, 1, {}, {}, {}
  local lua_paths, cancelled, cancel_root = {}, false, nil
  local function running() return not cancelled and active() end
  local function add(path, language, anchor)
    path = source.canonical(path)
    local key = path .. "\n" .. language
    if seen[key] then return end
    seen[key] = true
    entries[#entries + 1] = { path = path, language = language, anchor = anchor }
  end
  local function resolve_lua()
    if #lua_paths == 0 then receive(entries, nil, warnings) return end
    local config, failure = M.config({ path = root, language = "lua" })
    if not config then warnings[#warnings + 1] = failure receive(entries, nil, warnings) return end
    local index = 1
    local resolved_directories = {}
    local function next_lua()
      if not running() then return end
      local path = lua_paths[index]
      index = index + 1
      if not path then receive(entries, nil, warnings) return end
      local directory = vim.fs.dirname(path)
      if type(config.root_dir) ~= "function" and resolved_directories[directory] then vim.schedule(next_lua) return end
      resolved_directories[directory] = true
      cancel_root = roots.resolve(config, path, nil, function(resolved, root_failure)
        if not running() then return end
        if root_failure then receive(nil, root_failure) return end
        if not resolved then
          warnings[#warnings + 1] = "unavailable: no Lua workspace root for " .. path
          vim.schedule(next_lua) return
        end
        if seen[source.canonical(resolved) .. "\nlua"] then vim.schedule(next_lua) return end
        authorize({ resolved }, function(allowed, access_failure)
          if not running() then return end
          if access_failure or not allowed[resolved] then receive(nil, access_failure or "access_denied: " .. resolved) return end
          add(resolved, "lua", path)
          vim.schedule(next_lua)
        end)
      end)
    end
    next_lua()
  end
  local function next_directory()
    if not running() then return end
    local directory = queue[cursor]
    cursor = cursor + 1
    if not directory then resolve_lua() return end
    authorize({ directory }, function(allowed, failure)
      if not running() then return end
      if failure then receive(nil, failure) return end
      if not allowed[directory] then receive(nil, "access_denied: " .. directory) return end
      vim.uv.fs_stat(directory .. "/.git", vim.schedule_wrap(function(_, boundary)
        if not running() then return end
        if boundary and directory ~= source.canonical(root) then next_directory() return end
        vim.uv.fs_scandir(directory, vim.schedule_wrap(function(scan_error, scan)
          if not running() then return end
          if scan_error then receive(nil, scan_error) return end
          local directory_lua_paths = {}
          while true do
            local name, kind = vim.uv.fs_scandir_next(scan)
            if not name then break end
            local path = directory .. "/" .. name
            if kind == "directory" and not excluded[name] then queue[#queue + 1] = path
            elseif kind == "file" then
              if name == "Cargo.toml" then add(directory, "rust", path)
              elseif name == "package.json" then add(directory, "typescript", path)
              elseif name:match("%.lua$") then directory_lua_paths[#directory_lua_paths + 1] = path end
            end
          end
          if #directory_lua_paths > 0 then
            authorize(directory_lua_paths, function(allowed_source, source_failure)
              if not running() then return end
              if source_failure then receive(nil, source_failure) return end
              for _, path in ipairs(directory_lua_paths) do
                if not allowed_source[path] then receive(nil, "access_denied: " .. path) return end
                lua_paths[#lua_paths + 1] = path
              end
              next_directory()
            end)
          else next_directory() end
        end))
      end))
    end)
  end
  next_directory()
  return function() cancelled = true if cancel_root then cancel_root() end end
end

---List selectable project directories and language keys without starting servers.
---@return fun() cancel
function M.list(root, authorize, active, receive, options)
  return inventory(root, authorize, active, function(entries, failure, warnings)
    if failure then receive(nil, failure) return end
    table.sort(entries, function(left, right)
      if left.path == right.path then return left.language < right.language end
      return left.path < right.path
    end)
    local rows = {}
    local offset = options and options.offset or 0
    local limit = options and options.limit or #entries
    for index = offset + 1, math.min(#entries, offset + limit) do
      local entry = entries[index]
      rows[#rows + 1] = { path = entry.path, line = 1, column = 1, text = entry.language }
    end
    receive({ rows = rows, source = "workspace selection", warnings = warnings,
      next_offset = offset + limit < #entries and offset + limit or nil })
  end)
end

---Select one enabled configuration for the requested language.
---@param selection ForgeLspWorkspace
---@return vim.lsp.Config?, string?
function M.config(selection)
  if type(selection) ~= "table" or type(selection.path) ~= "string" or selection.path == "" or not languages[selection.language] then
    return nil, "invalid_workspace: expected path and language (rust, typescript, or lua)"
  end
  local family = selection.language
  local options = require("forge").config or {}
  local preferred = options.lsp and options.lsp.workspace_servers and options.lsp.workspace_servers[family]
  local candidates = {}
  for _, config in ipairs(vim.lsp.get_configs({ enabled = true })) do
    local matches = false
    for _, filetype in ipairs(languages[family]) do matches = matches or vim.tbl_contains(config.filetypes or {}, filetype) end
    if matches and (not preferred or preferred == config.name) then candidates[#candidates + 1] = config end
  end
  if #candidates ~= 1 then
    local names = vim.tbl_map(function(config) return config.name end, candidates)
    table.sort(names)
    return nil, (#candidates == 0 and "unavailable: no enabled LSP configuration for " or "ambiguous_server: configure lsp.workspace_servers for ") .. family .. " (" .. table.concat(names, ", ") .. ")"
  end
  return candidates[1]
end

---Resolve and initialize one selected project's configuration.
---@param selection ForgeLspWorkspace
---@return fun() cancel
function M.resolve(selection, authorize, receive)
  local config, config_error = M.config(selection)
  if not config then receive(nil, config_error) return function() end end
  local cancelled, completed = false, false
  local cancel_wait
  local function finish(value, failure)
    if cancelled or completed then return end
    completed = true
    receive(value, failure)
  end
  local function start(anchor)
    cancel_wait = require("forge.lsp.startup").selected(anchor, config, function(client, failure)
      finish(client and { client = client, selection = selection } or nil, failure)
    end, selection.language == "lua" and selection.path or nil)
  end
  authorize({ selection.path }, function(allowed, failure)
    if cancelled then return end
    if failure or not allowed[selection.path] then finish(nil, failure or "access_denied: " .. selection.path) return end
    local stat = vim.uv.fs_stat(selection.path)
    if not stat or stat.type ~= "directory" then finish(nil, "invalid_workspace: workspace path must be a directory") return end
    if selection.language ~= "lua" then
      local anchor = selection.path .. (selection.language == "rust" and "/Cargo.toml" or "/package.json")
      authorize({ anchor }, function(allowed_anchor, anchor_failure)
        if cancelled then return end
        if anchor_failure or not allowed_anchor[anchor] then finish(nil, anchor_failure or "access_denied: " .. anchor) return end
        local anchor_stat = vim.uv.fs_stat(anchor)
        if not anchor_stat or anchor_stat.type ~= "file" then finish(nil, "invalid_workspace: missing project manifest: " .. anchor) return end
        start(anchor)
      end)
    else
      local client_anchor
      for _, client in ipairs(vim.lsp.get_clients({ name = config.name, _uninitialized = true })) do
        local root = client.root_dir or client.config.root_dir
        if client.name == config.name and type(root) == "string" and source.canonical(root) == source.canonical(selection.path)
          and not client:is_stopped() then
          client_anchor = selection.path
          break
        end
      end
      if client_anchor then start(client_anchor) return end
      local cancel_inventory = inventory(selection.path, authorize, function() return not cancelled and not completed end, function(entries, inventory_failure)
        if inventory_failure then finish(nil, inventory_failure) return end
        for _, entry in ipairs(entries) do
          if entry.language == "lua" and source.canonical(entry.path) == source.canonical(selection.path) then start(entry.anchor) return end
        end
        finish(nil, "invalid_workspace: no Lua source resolves to the selected root")
      end)
      if not cancel_wait then cancel_wait = cancel_inventory end
    end
  end)
  return function() cancelled = true if cancel_wait then cancel_wait() end end
end

return M
