local config = require("diff_review.config")
local gh = require("diff_review.gh")
local highlights = require("diff_review.highlights")
local notifications = require("diff_review.notifications")

---@class DiffReviewPRViewOptions
---@field cwd? string

---@class DiffReviewPRViewModule
---@field _ns integer
---@field _last_buf? integer

---@type DiffReviewPRViewModule
local M = {}

M._ns = vim.api.nvim_create_namespace("diff_review_pr_view")

---@param text string
---@return string[]
local function markdown_lines(text)
  text = tostring(text or ""):gsub("\r\n", "\n")
  if text == "" then return { "_No description._" } end
  return vim.split(text, "\n", { plain = true })
end

---@param oid string?
---@return string
local function short_oid(oid)
  oid = tostring(oid or "")
  if oid == "" then return "0000000" end
  return oid:sub(1, 7)
end

---@param pr DiffReviewGhPR
---@return string
local function head_subject(pr)
  local commits = pr.commits or {}
  local commit = commits[#commits]
  return commit and commit.messageHeadline or ""
end

---@param pr DiffReviewGhPR
---@return string
local function head_oid(pr)
  local commits = pr.commits or {}
  local commit = commits[#commits]
  return short_oid((commit and commit.oid) or pr.headRefOid)
end

---@param path string
---@return string
local function display_path(path)
  return tostring(path or ""):gsub("/", "\\")
end

---@param lines string[]
---@param text string
---@return integer
local function add_line(lines, text)
  lines[#lines + 1] = text
  return #lines
end

---@param highlights_out table[]
---@param line integer
---@param start_col integer
---@param end_col integer
---@param hl_group string
local function add_highlight(highlights_out, line, start_col, end_col, hl_group)
  highlights_out[#highlights_out + 1] = {
    line = line,
    start_col = start_col,
    end_col = end_col,
    hl_group = hl_group,
  }
end

---@param pr DiffReviewGhPR
---@return string[] lines
---@return table[] highlights_out
local function build_lines(pr)
  local lines = {}
  local highlights_out = {}

  local hint_line = add_line(lines, "Hint: b browse")
  add_highlight(highlights_out, hint_line, 0, 6, "DiffReviewStatusHint")
  add_highlight(highlights_out, hint_line, 6, 7, "DiffReviewStatusHintKey")
  add_highlight(highlights_out, hint_line, 7, #lines[hint_line], "DiffReviewStatusHint")

  add_line(lines, "")
  local title_line = add_line(lines, "Title: " .. (pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))))
  add_highlight(highlights_out, title_line, 0, 6, "DiffReviewStatusLabel")
  add_highlight(highlights_out, title_line, 7, #lines[title_line], "DiffReviewStatusPath")

  add_line(lines, "")
  local description_line = add_line(lines, "Description:")
  add_highlight(highlights_out, description_line, 0, #lines[description_line], "DiffReviewStatusLabel")
  for _, line in ipairs(markdown_lines(pr.body)) do
    add_line(lines, line)
  end

  add_line(lines, "")
  local head_line = add_line(lines, ("Head:    %-7s %s %s"):format(head_oid(pr), pr.headRefName or "", head_subject(pr)))
  add_highlight(highlights_out, head_line, 0, 8, "DiffReviewStatusLabel")
  add_highlight(highlights_out, head_line, 8, 15, "DiffReviewStatusObjectId")
  local branch_start = 16
  local branch_end = branch_start + #(pr.headRefName or "")
  add_highlight(highlights_out, head_line, branch_start, branch_end, "DiffReviewStatusBranch")

  add_line(lines, "")
  local changes_line = add_line(lines, ("Changes (%d)"):format(pr.changedFiles or #(pr.files or {})))
  add_highlight(highlights_out, changes_line, 0, #lines[changes_line], "DiffReviewStatusHeader")
  for _, file in ipairs(pr.files or {}) do
    local stats = ("+%d -%d"):format(file.additions or 0, file.deletions or 0)
    local line = add_line(lines, ("%s %s"):format(display_path(file.path), stats))
    local stats_start = #lines[line] - #stats
    add_highlight(highlights_out, line, 0, math.max(stats_start - 1, 0), "DiffReviewStatusPath")
    add_highlight(highlights_out, line, stats_start, #lines[line], "DiffReviewAddRange")
  end

  return lines, highlights_out
end

---@param buf integer
local function enable_markdown_renderer(buf)
  vim.schedule(function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local current = vim.api.nvim_get_current_buf()
    if current ~= buf then return end
    local ok, renderer = pcall(require, "render-markdown")
    if ok and type(renderer.buf_enable) == "function" then
      pcall(renderer.buf_enable)
    end
  end)
end

---@param buf integer
---@param pr DiffReviewGhPR
local function setup_keymaps(buf, pr)
  local opts = { buffer = buf, silent = true, nowait = true }
  vim.keymap.set("n", "b", function()
    if not gh.browse_pr(pr) then
      notifications.error("Unable to open PR URL", "DiffReview")
    end
  end, vim.tbl_extend("force", opts, { desc = "Browse pull request" }))
end

---@param pr DiffReviewGhPR
---@param opts? DiffReviewPRViewOptions
---@return integer? buf
function M.open(pr, opts)
  opts = opts or {}
  if not pr then return nil end
  highlights.setup()

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "markdown"
  local name = ("%s://%s"):format((config.options or config.defaults).pr_buffer_name, pr.number or "current")
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  local lines, line_highlights = build_lines(pr)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  for _, highlight in ipairs(line_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end

  setup_keymaps(buf, pr)
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
  M._last_buf = buf
  enable_markdown_renderer(buf)
  return buf
end

return M
