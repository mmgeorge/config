local M = {}
local paths = require("forge.infra.paths")

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


function M.open(options)
  return require("forge.source_document").open({ workspace = options.cwd, revision = options.rev,
    path = options.path, line = options.line, on_error = options.on_error })
end

M.show = M.open

return M
