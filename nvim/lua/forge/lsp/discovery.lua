local source = require("forge.lsp.source")
local M = {}
local excluded = { [".git"] = true, node_modules = true, target = true, vendor = true, dist = true, build = true }
local extensions = { rs = true, ts = true, tsx = true, mts = true, cts = true, js = true, jsx = true, mjs = true, cjs = true, lua = true }

---@param path string
---@param depth integer
---@param authorize function
---@param active fun(): boolean
---@param receive fun(paths: string[]?, warnings: string[]?, failure: string?)
function M.files(path, depth, authorize, active, receive)
  local queue, files, warnings, visited = { { path = path, depth = 0 } }, {}, {}, {}
  local entries, directories = 0, 0
  local function next_directory()
    if not active() then return end
    local item = table.remove(queue, 1)
    if not item then table.sort(files) receive(files, warnings) return end
    local canonical = source.canonical(item.path)
    if visited[canonical] then next_directory() return end
    visited[canonical] = true
    authorize({ canonical }, function(allowed, failure)
      if not active() then return end
      if failure then receive(nil, nil, failure) return end
      if not allowed[canonical] then warnings[#warnings + 1] = "access_denied: " .. canonical next_directory() return end
      vim.uv.fs_stat(canonical, vim.schedule_wrap(function(stat_error, stat)
        if not active() then return end
        if stat_error then
          if canonical == source.canonical(path) then receive(nil, nil, source.read_failure(canonical, stat_error))
          else warnings[#warnings + 1] = source.read_failure(canonical, stat_error) next_directory() end
          return
        end
        if stat.type == "file" then files[#files + 1] = canonical next_directory() return end
        directories = directories + 1
        if directories > 128 or entries >= 4096 then warnings[#warnings + 1] = "Directory scan limit reached." queue = {} next_directory() return end
        vim.uv.fs_scandir(canonical, vim.schedule_wrap(function(scan_error, request)
          if not active() then return end
          if scan_error then warnings[#warnings + 1] = source.read_failure(canonical, scan_error) next_directory() return end
          local children = {}
          while true do
            local name, kind = vim.uv.fs_scandir_next(request)
            if not name then break end
            entries = entries + 1
            if entries > 4096 then warnings[#warnings + 1] = "Directory scan limit reached." break end
            children[#children + 1] = { name = name, kind = kind }
          end
          table.sort(children, function(left, right) return left.name < right.name end)
          for _, child in ipairs(children) do
            local target = canonical .. "/" .. child.name
            if child.kind == "directory" and not excluded[child.name] and item.depth < depth then
              queue[#queue + 1] = { path = target, depth = item.depth + 1 }
            elseif child.kind == "file" and extensions[child.name:match("%.([^%.]+)$")] then files[#files + 1] = source.canonical(target) end
          end
          next_directory()
        end))
      end))
    end)
  end
  next_directory()
end

return M
