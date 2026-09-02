--- Opens read-only buffers showing a file as it existed at a historical git revision,
--- resolving the revision that holds the pre-change content for a diff entry and caching
--- one buffer per (path, revision) so repeated opens reuse the same view.
---
--- Names buffers `GitFileRevision://<path>@<rev>` and marks them with a red winbar, so a
--- historical revision is never mistaken for an editable working-tree file.
---@class DiffReviewFileRevisionModule
---@field buffers table<string, integer> cached revision buffers keyed by display name
local M = { buffers = {} }

local paths = require("diff_review.infra.paths")
local git_backend = require("diff_review.git.git_backend")

--- Resolves the Git revision and relative path containing pre-change file content for a diff entry.
---@param entry DiffReviewStatusEntry Diff status entry descriptor.
---@param status table? Active review state table.
---@return string? rev Git revision string or nil.
---@return string? path Repository-relative file path string or nil.
function M.target(entry, status)
  if not (entry.hunk and entry.file and status and status.cwd) then return nil end
  local relpath = paths.repo_relative(entry.file.filename, status.cwd)
  if not relpath then return nil end
  local view_kind = status.view_kind or "status"
  if view_kind == "diff" and status.diff_branch then
    return status.diff_branch, relpath
  end
  if view_kind == "status" and entry.kind == "hunk" then
    if entry.hunk.staged then
      return "HEAD", entry.hunk.git_original_file or relpath
    end
    return ":0", relpath
  end
  return nil
end

--- Resolves revision short SHA and opens historical file revision buffer.
---@param opts { rev: string, path: string, cwd: string, line: integer?, on_error: fun(message: string) } Revision open options table.
function M.open(opts)
  local label_rev = opts.rev == ":0" and "HEAD" or opts.rev
  git_backend.systemlist_async({ "git", "-C", opts.cwd, "rev-parse", "--short", label_rev }, function(label_lines, label_code)
    local label = label_code == 0 and vim.trim(tostring(label_lines and label_lines[1] or "")) or ""
    if label == "" then label = opts.rev end
    M.show(opts, label)
  end)
end

--- Fetches file text via git show and populates a read-only revision buffer.
---@param opts { rev: string, path: string, cwd: string, line: integer?, on_error: fun(message: string) } Revision show options table.
---@param label string Short SHA label string for buffer naming.
function M.show(opts, label)
  local command = { "git", "-C", opts.cwd, "show", ("%s:%s"):format(opts.rev, opts.path) }
  git_backend.systemlist_async(command, function(lines, code, output)
    if code ~= 0 then
      opts.on_error(vim.trim(tostring(output or "")))
      return
    end
    local name = ("GitFileRevision://%s@%s"):format(opts.path, label)
    local buf = M.buffers[name]
    if not (buf and vim.api.nvim_buf_is_valid(buf)) then
      buf = vim.api.nvim_create_buf(true, false)
      -- "nowrite", not "nofile": buffer pickers (e.g. snacks) hide nofile
      -- scratch buffers, and this buffer should stay reachable like a file.
      vim.bo[buf].buftype = "nowrite"
      vim.bo[buf].bufhidden = "hide"
      vim.bo[buf].swapfile = false
      if not pcall(vim.api.nvim_buf_set_name, buf, name) then
        pcall(vim.api.nvim_buf_set_name, buf, ("%s#%d"):format(name, buf))
      end
      vim.keymap.set("n", "q", function()
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end, { buffer = buf, silent = true, nowait = true, desc = "Close file revision" })
      -- Sticky header: a red winbar marks the buffer as a historical revision.
      -- Window-local, so it must be applied/cleared as windows show the
      -- buffer; dropbar skips windows whose winbar is already set.
      local header = ("%%#DiffReviewFileRevisionHeader# %s @ %s — read-only revision %%*"):format(
        opts.path:gsub("%%", "%%%%"),
        label:gsub("%%", "%%%%")
      )
      vim.api.nvim_create_autocmd("BufWinEnter", {
        buffer = buf,
        callback = function()
          vim.wo.winbar = header
        end,
      })
      vim.api.nvim_create_autocmd("BufWinLeave", {
        buffer = buf,
        callback = function()
          vim.wo.winbar = ""
        end,
      })
      M.buffers[name] = buf
    end
    vim.bo[buf].readonly = false
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
    vim.bo[buf].readonly = true
    local ft = require("diff_review.infra.util").detect_filetype(opts.path, lines)
    if ft ~= "" and vim.bo[buf].filetype ~= ft then
      vim.bo[buf].filetype = ft
    end
    vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
    if opts.line then
      local target = math.min(math.max(opts.line, 1), vim.api.nvim_buf_line_count(buf))
      pcall(vim.api.nvim_win_set_cursor, 0, { target, 0 })
      vim.cmd("normal! zz")
    end
  end)
end

return M
