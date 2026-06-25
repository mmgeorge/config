--- Parses the conventional-commit `<type>(scope)!:` prefix of a commit subject and
--- splits the subject into colorized segments, so the type prefix highlights
--- consistently across Head/Merge/Push, PR, About, and commit-list rows.
---@class DiffReviewConventionalCommitModule
local M = {}

---@param subject string?
---@return integer? type_end_col zero-based exclusive byte column within subject
function M.type_end(subject)
  subject = tostring(subject or "")
  local commit_type = subject:match("^([a-z][a-z0-9-]*)")
  if not commit_type then return nil end

  local cursor = #commit_type + 1
  local next_char = subject:sub(cursor, cursor)
  if next_char == "!" then
    return subject:sub(cursor + 1, cursor + 2) == ": " and #commit_type or nil
  end
  if next_char == ":" then
    return subject:sub(cursor + 1, cursor + 1) == " " and #commit_type or nil
  end
  if next_char ~= "(" then return nil end

  local close_scope = subject:find(")", cursor + 1, true)
  if not close_scope or close_scope == cursor + 1 then return nil end
  local scope = subject:sub(cursor + 1, close_scope - 1)
  if scope:find("[%c%s]") then return nil end
  local after_scope = subject:sub(close_scope + 1, close_scope + 1)
  if after_scope == "!" then
    return subject:sub(close_scope + 2, close_scope + 3) == ": " and #commit_type or nil
  end
  if after_scope == ":" then
    return subject:sub(close_scope + 2, close_scope + 2) == " " and #commit_type or nil
  end
  return nil
end

---@param subject string
---@param hl_group string
---@return table[]
function M.subject_segments(subject, hl_group)
  local conventional_type_end = M.type_end(subject)
  if not conventional_type_end then return { { subject, hl_group } } end
  return {
    { subject:sub(1, conventional_type_end), "DiffReviewStatusCommitType" },
    { subject:sub(conventional_type_end + 1), hl_group },
  }
end

return M
