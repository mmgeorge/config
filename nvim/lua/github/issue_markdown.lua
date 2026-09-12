local M = {}

local language_registered = false

local function namespace()
  local ok, ui = pcall(require, "render-markdown.core.ui")
  return ok and type(ui) == "table" and ui.ns or vim.api.nvim_get_namespaces()["render-markdown.nvim"]
end

local function body_rows(replica)
  local block = replica and replica.block and replica.block["region:body"]
  if not block then return nil end
  local _, start_row = replica.sequence:position("region:body")
  return start_row, start_row + block.row_count
end

local function prune(buffer, start_row, end_row)
  local ns = namespace()
  if not ns then return end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buffer, ns, 0, -1, {})) do
    if mark[2] < start_row or mark[2] >= end_row then
      pcall(vim.api.nvim_buf_del_extmark, buffer, ns, mark[1])
    end
  end
end

function M.render(replica, window)
  local start_row, end_row = body_rows(replica)
  if not start_row or not window or not vim.api.nvim_win_is_valid(window) then return false end
  local ok, renderer = pcall(require, "render-markdown")
  if not ok or type(renderer.render) ~= "function" then return false end
  local ns = namespace()
  if ns then vim.api.nvim_buf_clear_namespace(replica.buffer, ns, 0, -1) end
  if not language_registered then
    pcall(vim.treesitter.language.register, "markdown", "ForgeGithubIssue")
    language_registered = true
  end
  pcall(vim.treesitter.start, replica.buffer, "markdown")
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = window })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = window })
  local rendered = pcall(renderer.render, {
    buf = replica.buffer,
    win = window,
    config = {
      enabled = true,
      render_modes = true,
      debounce = 0,
      completions = { lsp = { enabled = false } },
      sign = { enabled = false },
      win_options = {
        conceallevel = { default = conceallevel, rendered = conceallevel },
        concealcursor = { default = concealcursor, rendered = concealcursor },
      },
      on = { render = function() prune(replica.buffer, start_row, end_row) end },
    },
  })
  if rendered then prune(replica.buffer, start_row, end_row) end
  return rendered
end

function M.clear(replica)
  local ns = namespace()
  if ns and replica and vim.api.nvim_buf_is_valid(replica.buffer) then
    vim.api.nvim_buf_clear_namespace(replica.buffer, ns, 0, -1)
  end
  if replica and vim.api.nvim_buf_is_valid(replica.buffer) then pcall(vim.treesitter.stop, replica.buffer) end
end

return M
