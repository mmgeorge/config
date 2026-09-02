--- Builds the stable string identity keys for status entries (sections, files, hunks) across the
--- GitStatus, commit, and provider-backed views, so fold state, caches, and actions can index a
--- single canonical key per entry regardless of which view produced it.

---@class DiffReviewStatusKeysModule
local M = {}

--- Builds a hunk identity key string from section name, filename, and diff hash.
---@param section_name string Status section name string.
---@param filename string Absolute or relative file path string.
---@param diff string? Optional unified diff body text.
---@return string key Canonical hunk identity key string.
function M.hunk_key(section_name, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("hunk:%s:%s:%s"):format(section_name, filename, hash)
end

--- Builds a file identity key string from section name and filename.
---@param section_name string Status section name string.
---@param filename string File path string.
---@return string key Canonical file identity key string.
function M.file_key(section_name, filename)
  return ("file:%s:%s"):format(section_name, filename)
end

--- Builds a section identity key string from section name.
---@param section_name string Status section name string.
---@return string key Canonical section identity key string.
function M.section_key(section_name)
  return "section:" .. section_name
end

--- Builds a commit identity key string from commit object ID.
---@param oid string Commit object identifier.
---@return string key Canonical commit identity key string.
function M.commit_key(oid)
  return "commit:" .. oid
end

--- Builds a commit file identity key string from commit OID and filename.
---@param oid string Commit object identifier.
---@param filename string File path string.
---@return string key Canonical commit file identity key string.
function M.commit_file_key(oid, filename)
  return ("commit-file:%s:%s"):format(oid, filename)
end

--- Builds a commit hunk identity key string from commit OID, filename, and diff hash.
---@param oid string Commit object identifier.
---@param filename string File path string.
---@param diff string? Optional unified diff body text.
---@return string key Canonical commit hunk identity key string.
function M.commit_hunk_key(oid, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("commit-hunk:%s:%s:%s"):format(oid, filename, hash)
end

--- Builds a provider file identity key string from provider key and filename.
---@param provider_key string Provider unique identifier string.
---@param filename string File path string.
---@return string key Canonical provider file identity key string.
function M.provider_file_key(provider_key, filename)
  return ("provider-file:%s:%s"):format(provider_key, filename)
end

--- Builds a provider hunk identity key string from provider key, filename, and diff hash.
---@param provider_key string Provider unique identifier string.
---@param filename string File path string.
---@param diff string? Optional unified diff body text.
---@return string key Canonical provider hunk identity key string.
function M.provider_hunk_key(provider_key, filename, diff)
  local hash = diff and vim.fn.sha256(diff) or "file"
  return ("provider-hunk:%s:%s:%s"):format(provider_key, filename, hash)
end

--- Resolves primary key sequence bound to a status view command for presentation.
---@param command_id string Command identifier string.
---@return string key Primary key binding string or empty string.
function M.primary_key(command_id)
  return require("diff_review.shared.keymaps").status_keys_for(command_id)[1] or ""
end

--- Formats an array of key sequences into a comma-separated display string.
---@param keys string[] Array of key binding strings.
---@return string text Formatted comma-separated string.
function M.key_text(keys)
  return table.concat(keys, ", ")
end

return M
