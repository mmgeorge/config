--- Builds the stable string identity keys for status entries (sections, files, hunks) across the
--- GitStatus, commit, and provider-backed views, so fold state, caches, and actions can index a
--- single canonical key per entry regardless of which view produced it.

---@class DiffReviewStatusKeysModule
local M = {}

--- Build a hunk identity key from its section, file, and diff hash (or "file" when diffless).
---@param section_name string
---@param filename string
---@param diff string?
---@return string
function M.hunk_key(section_name, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("hunk:%s:%s:%s"):format(section_name, filename, hash)
end

---@param section_name string
---@param filename string
---@return string
function M.file_key(section_name, filename)
  return ("file:%s:%s"):format(section_name, filename)
end

---@param section_name string
---@return string
function M.section_key(section_name)
  return "section:" .. section_name
end

---@param oid string
---@return string
function M.commit_key(oid)
  return "commit:" .. oid
end

---@param oid string
---@param filename string
---@return string
function M.commit_file_key(oid, filename)
  return ("commit-file:%s:%s"):format(oid, filename)
end

---@param oid string
---@param filename string
---@param diff string?
---@return string
function M.commit_hunk_key(oid, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("commit-hunk:%s:%s:%s"):format(oid, filename, hash)
end

---@param provider_key string
---@param filename string
---@return string
function M.provider_file_key(provider_key, filename)
  return ("provider-file:%s:%s"):format(provider_key, filename)
end

---@param provider_key string
---@param filename string
---@param diff string?
---@return string
function M.provider_hunk_key(provider_key, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("provider-hunk:%s:%s:%s"):format(provider_key, filename, hash)
end

--- Resolve a command's primary (first) bound key for display.
--- Lazy-require keymaps so key resolution never depends on module load order.
---@param command_id string
---@return string
function M.primary_key(command_id)
  return require("diff_review.keymaps").status_keys_for(command_id)[1] or ""
end

---@param keys string[]
---@return string
function M.key_text(keys)
  return table.concat(keys, ", ")
end

return M
