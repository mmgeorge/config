---@module 'blink.cmp'
local FileSource = {}

local cache = {}

local function workspace_root()
  local harness = require("diff_review.session").harness
  return harness.session and harness.session.workspace or vim.fn.getcwd()
end

local function finish(callback, items)
  callback({ items = items, is_incomplete_backward = false, is_incomplete_forward = false })
end

local function load_workspace_files(root, callback)
  if cache[root] then
    callback(cache[root])
    return
  end
  vim.system({ "git", "ls-files", "--cached", "--others", "--exclude-standard" }, {
    cwd = root,
    text = true,
    stdout = true,
    stderr = true,
  }, function(result)
    local files = {}
    if result.code == 0 then
      for path in result.stdout:gmatch("[^\r\n]+") do files[#files + 1] = path:gsub("\\", "/") end
    end
    cache[root] = files
    vim.schedule(function() callback(files) end)
  end)
end

function FileSource.new()
  return setmetatable({}, { __index = FileSource })
end

function FileSource:get_trigger_characters()
  return { "@" }
end

function FileSource:enabled()
  local line = vim.api.nvim_get_current_line()
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  return line:sub(1, cursor_column):match("@[^%s@]*$") ~= nil
end

function FileSource:get_completions(_, callback)
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_column = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local token_start = line:sub(1, cursor_column):match(".*()@[^%s@]*$")
  if not token_start then
    finish(callback, {})
    return
  end
  load_workspace_files(workspace_root(), function(files)
    local items = {}
    for _, path in ipairs(files) do
      local label = "@" .. path
      items[#items + 1] = {
        label = label,
        filterText = label,
        detail = "Workspace file",
        kind = vim.lsp.protocol.CompletionItemKind.File,
        textEdit = {
          newText = label,
          range = {
            start = { line = row, character = token_start - 1 },
            ["end"] = { line = row, character = cursor_column },
          },
        },
      }
    end
    finish(callback, items)
  end)
end

function FileSource._clear_cache()
  cache = {}
end

return FileSource
