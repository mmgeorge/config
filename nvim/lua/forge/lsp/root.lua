local source = require("forge.lsp.source")
local M = {}

---Resolve an enabled configuration's root without starting a language server.
---@param config vim.lsp.Config
---@param path string
---@param buffer integer?
---@param receive fun(root?: string, failure?: string)
---@return fun() cancel
function M.resolve(config, path, buffer, receive)
  local finished, owned_buffer, timer = false, nil, nil
  local function cleanup()
    if timer and not timer:is_closing() then timer:stop() timer:close() end
    timer = nil
    if owned_buffer and vim.api.nvim_buf_is_valid(owned_buffer) and not vim.bo[owned_buffer].modified
      and #vim.fn.win_findbuf(owned_buffer) == 0 then
      pcall(vim.api.nvim_buf_delete, owned_buffer, { force = true })
    end
    owned_buffer = nil
  end
  local function finish(root, failure)
    if finished then return end
    finished = true
    cleanup()
    receive(root, failure)
  end
  local function resolve(target_buffer)
    if finished then return end
    local ok, failure = pcall(function()
      if type(config.root_dir) == "function" then
        config.root_dir(target_buffer, function(root) finish(root) end)
      else
        finish(config.root_dir or (config.root_markers and vim.fs.root(path, config.root_markers)))
      end
    end)
    if not ok then finish(nil, "root_failed: " .. config.name .. " — " .. tostring(failure)) end
  end
  if type(config.root_dir) == "function" then
    timer = assert(vim.uv.new_timer())
    timer:start(60000, 0, vim.schedule_wrap(function()
      finish(nil, "timeout: selected LSP root resolution exceeded 60 seconds")
    end))
    buffer = buffer or source.loaded(path)
    if buffer then resolve(buffer)
    else
      source.read(path, function(contents, failure)
        if finished then return end
        if failure then finish(nil, failure) return end
        local opened, owned, open_failure = source.open(path, contents, vim.filetype.match({ filename = path }) or "")
        if not opened then finish(nil, open_failure) return end
        if owned then owned_buffer = opened end
        resolve(opened)
      end)
    end
  else resolve(buffer) end
  return function() finished = true cleanup() end
end

return M
