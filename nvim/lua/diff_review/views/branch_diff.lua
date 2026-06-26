--- Loads a branch-vs-working-tree diff into the active status buffer, building the head
--- lines and sections and re-rendering them through the status render core.
---@class DiffReviewBranchDiffModule
local M = {}

local git_backend = require("diff_review.git.git_backend")
local notifications = require("diff_review.infra.notifications")

-- status_render edge kept lazy to avoid a load-time cycle.
local function status_render() return require("diff_review.views.status.status_render") end
-- section_builder edge kept lazy to avoid a load-time cycle.
local function section_builder() return require("diff_review.views.status.section_builder") end
local session = require("diff_review.session")


---@param branch string
---@param file? string repo-relative path when diffing a single file
---@return DiffReviewStatusHeadLine[]
function M.head_lines(branch, file)
  local lines = {
    {
      segments = {
        { ("%-8s"):format("Diff:"), "DiffReviewStatusLabel" },
        { branch, "DiffReviewStatusBranch" },
        { " -> working tree", "DiffReviewStatusHint" },
      },
    },
  }
  if file then
    lines[#lines + 1] = {
      segments = {
        { ("%-8s"):format("File:"), "DiffReviewStatusLabel" },
        { file, "DiffReviewStatusPath" },
      },
    }
  end
  return lines
end

---@param cwd string
---@param branch string
---@param diff_text? string
---@param file? string
---@return DiffReviewStatusSection[]
function M.sections(cwd, branch, diff_text, file)
  local provider_key = "diff:" .. branch .. (file and (":" .. file) or "")
  local sections = section_builder().sections_from_diff(cwd, {
    title = "Changes vs " .. branch,
    section_name = provider_key .. ":changes",
    default_status = "",
    name = provider_key .. ":changes",
    file_key_prefix = provider_key,
    file_entry_kind = "pr_file",
    hunk_entry_kind = "pr_hunk",
  }, diff_text)
  return sections
end

---@param branch string
---@param cwd string
---@param buf integer
---@param diff_text? string
---@param file? string
function M.render(branch, cwd, buf, diff_text, file)
  local status = session.status
  if not (status and status.buf == buf) then return end
  status.head_lines = M.head_lines(branch, file)
  status.sections = M.sections(cwd, branch, diff_text, file)
  status.fancy_rows = {}
  status_render().status_render_loaded(buf, nil, nil, { reuse_sections = true }, status.head_lines, status.sections)
end

---@param branch string
---@param cwd string
---@param buf integer
---@param file? string
function M.load(branch, cwd, buf, file)
  local status = session.status
  if not (status and status.buf == buf) then return end
  status.branch_diff_request_id = (status.branch_diff_request_id or 0) + 1
  local request_id = status.branch_diff_request_id
  local command = git_backend.git_diff_command(cwd, { branch })
  if file then
    vim.list_extend(command, { "--", file })
  end
  git_backend.systemlist_async(command, function(output, code, stderr)
    local latest_status = session.states and session.states[buf] or nil
    if not (
      latest_status
      and latest_status.branch_diff_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    session.status = latest_status
    if code ~= 0 then
      local message = vim.trim(stderr or "")
      notifications.error("Git diff failed: " .. (message ~= "" and message or ("git exited " .. code)), "GitBranchDiff")
      return
    end
    M.render(branch, cwd, buf, table.concat(output or {}, "\n"), file)
  end)
end

return M
