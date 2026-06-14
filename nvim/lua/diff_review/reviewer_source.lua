---@module 'blink.cmp'
---@class blink.cmp.Source
local source = {}

---@param opts table?
---@return table
function source.new(opts)
  local self = setmetatable({}, { __index = source })
  self.opts = opts or {}
  return self
end

---@return boolean
function source:enabled()
  if not vim.b.diff_review_pr_reviewer_completion then return false end
  local line = vim.api.nvim_get_current_line()
  local cursor_col = vim.api.nvim_win_get_cursor(0)[2]
  local before_cursor = line:sub(1, cursor_col)
  return before_cursor:match("^Review:%s*.*@[%w_-]*$") ~= nil
end

---@return string[]
function source:get_trigger_characters()
  return { "@" }
end

---@param ctx table
---@param callback fun(result: table)
function source:get_completions(ctx, callback)
  local repo_cache = require("github.repo_cache")
  local repo = repo_cache.buffer_repo(0)
  local row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local cursor_col = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local before_cursor = line:sub(1, cursor_col)
  local token_start = before_cursor:match(".*()@[%w_-]*$")
  local items = {}

  if token_start and before_cursor:match("^Review:%s*.*@[%w_-]*$") then
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
