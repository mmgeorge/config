--- Blink.cmp completion source providing GitHub contributor and reviewer suggestions.
---@module 'blink.cmp'
---@class DiffReviewBlinkReviewerSource: blink.cmp.Source
local source = {}

--- Instantiates a new reviewer completion source.
---@param opts table? Source configuration options.
---@return table source Constructed completion source instance.
function source.new(opts)
  local self = setmetatable({}, { __index = source })
  self.opts = opts or {}
  return self
end

--- Determines whether reviewer completion is active at the current cursor column.
---@return boolean enabled True if cursor is positioned after a reviewer trigger character.
function source:enabled()
  local repo_cache = require("github.repo_cache")
  if not repo_cache.user_completion_enabled(0) then return false end
  local line = vim.api.nvim_get_current_line()
  local cursor_col = vim.api.nvim_win_get_cursor(0)[2]
  local before_cursor = line:sub(1, cursor_col)
  return before_cursor:match(".*@[%w_-]*$") ~= nil
end

--- Returns trigger character list for reviewer completions.
---@return string[] triggers Array of trigger characters.
function source:get_trigger_characters()
  return { "@" }
end

--- Generates completion items matching contributor logins for the repository.
---@param ctx table Completion context descriptor.
---@param callback fun(result: table) Completion response callback.
function source:get_completions(ctx, callback)
  local repo_cache = require("github.repo_cache")
  local repo = repo_cache.completion_repo(0)
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_col = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local before_cursor = line:sub(1, cursor_col)
  local token_start = before_cursor:match(".*()@[%w_-]*$")
  local items = {}

  if token_start then
    for _, reviewer in ipairs(repo_cache.contributors(repo)) do
      local label = "@" .. reviewer.login
      items[#items + 1] = {
        label = label,
        filterText = label,
        sortText = reviewer.login,
        detail = reviewer.name and reviewer.name ~= "" and reviewer.name or repo,
        kind = vim.lsp.protocol.CompletionItemKind.Text,
        textEdit = {
          newText = label,
          range = {
            start = { line = row, character = token_start - 1 },
            ["end"] = { line = row, character = cursor_col },
          },
        },
      }
    end
  end

  callback({
    items = items,
    is_incomplete_backward = false,
    is_incomplete_forward = false,
  })
end

return source
