local source = require("forge.lsp.source")
local M = {}
local excluded = { [".git"] = true, node_modules = true, target = true, vendor = true, build = true, dist = true, [".cache"] = true }

---List authorized manifests without resolving roots or starting servers.
function M.list(root, authorize, active, receive)
  local root_stat = vim.uv.fs_stat(root)
  if not root_stat or root_stat.type ~= "directory" then receive(nil, "invalid_path: workspace listing requires a directory") return end
  local queue, cursor, rows = { source.canonical(root) }, 1, {}
  local function next_directory()
    if not active() then return end
    local directory = queue[cursor]
    cursor = cursor + 1
    if not directory then
      table.sort(rows, function(left, right) return left.path < right.path end)
      receive({ rows = rows, source = "workspace manifests" })
      return
    end
    authorize({ directory }, function(allowed, failure)
      if not active() then return end
      if failure then receive(nil, failure) return end
      if not allowed[directory] then receive(nil, "access_denied: " .. directory) return end
      vim.uv.fs_stat(directory .. "/.git", vim.schedule_wrap(function(_, boundary)
        if not active() then return end
        if boundary and directory ~= source.canonical(root) then next_directory() return end
        vim.uv.fs_scandir(directory, vim.schedule_wrap(function(scan_error, scan)
          if not active() then return end
          if scan_error then receive(nil, scan_error) return end
          while true do
            local name, kind = vim.uv.fs_scandir_next(scan)
            if not name then break end
            if kind == "directory" and not excluded[name] then queue[#queue + 1] = directory .. "/" .. name
            elseif kind == "file" and (name == "Cargo.toml" or name == "package.json") then
              rows[#rows + 1] = { path = directory .. "/" .. name, line = 1, column = 1,
                text = name == "Cargo.toml" and "Rust" or "TypeScript / JavaScript" }
            end
          end
          next_directory()
        end))
      end))
    end)
  end
  next_directory()
end

---Select one enabled editor configuration for a manifest.
function M.config(manifest)
  local name = vim.fs.basename(manifest)
  if name ~= "Cargo.toml" and name ~= "package.json" then return nil, "invalid_workspace: expected Cargo.toml or package.json" end
  local family = name == "Cargo.toml" and "rust" or "typescript"
  local options = require("forge").config or {}
  local preferred = options.lsp and options.lsp.workspace_servers and options.lsp.workspace_servers[family]
  local candidates = {}
  for _, config in ipairs(vim.lsp.get_configs({ enabled = true })) do
    local matches = vim.tbl_contains(config.filetypes or {}, "rust")
    if family == "typescript" then matches = vim.tbl_contains(config.filetypes or {}, "typescript") or vim.tbl_contains(config.filetypes or {}, "javascript") end
    if matches and (not preferred or preferred == config.name) then candidates[#candidates + 1] = config end
  end
  if #candidates ~= 1 then
    local names = vim.tbl_map(function(config) return config.name end, candidates)
    table.sort(names)
    return nil, (#candidates == 0 and "unavailable: no enabled LSP configuration for " or "ambiguous_server: configure lsp.workspace_servers for ") .. family .. " (" .. table.concat(names, ", ") .. ")"
  end
  return candidates[1]
end

---Resolve and initialize only the selected manifest configuration.
function M.resolve(manifest, authorize, receive)
  if type(manifest) ~= "string" then receive(nil, "invalid_workspace: manifest is required") return function() end end
  local cancelled, completed = false, false
  local cancel_wait
  local function finish(value, failure)
    if cancelled or completed then return end
    completed = true
    receive(value, failure)
  end
  authorize({ manifest }, function(allowed, failure)
    if cancelled then return end
    if failure or not allowed[manifest] then finish(nil, failure or "access_denied: " .. manifest) return end
    local stat = vim.uv.fs_stat(manifest)
    if not stat or stat.type ~= "file" then finish(nil, "invalid_workspace: manifest file does not exist") return end
    local config, config_error = M.config(manifest)
    if not config then finish(nil, config_error) return end
    local startup = require("forge.lsp.startup")
    cancel_wait = startup.selected(manifest, config, function(client, startup_error)
      finish(client and { client = client, manifest = manifest } or nil, startup_error)
    end)
  end)
  return function() cancelled = true if cancel_wait then cancel_wait() end end
end

return M
