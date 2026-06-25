--- Resolves repo-relative paths and normalizes absolute paths, shared by the git
--- command layer and file-revision/source resolution.
---@class DiffReviewPathsModule
local M = {}

---@param path string
---@return string
function M.normalize_path(path)
  return tostring(vim.fs.normalize(vim.fn.fnamemodify(path, ":p"))):gsub("\\", "/"):gsub("/+$", "")
end

---@param path string
---@return string
function M.normalize_path_text(path)
  return tostring(path or ""):gsub("\\", "/"):gsub("/+$", "")
end

---@param cwd string
---@param relpath string
---@return string
function M.repo_file_path(cwd, relpath)
  local path = cwd:gsub("[/\\]+$", "") .. "/" .. relpath
  if package.config:sub(1, 1) ~= "\\" and cwd:match("^%a:[/\\]") then
    return path
  end
  return vim.fn.fnamemodify(path, ":p")
end

---@param absolute string
---@param root_path string
---@param case_insensitive boolean
---@param original_filename string
---@return string? relpath
---@return string? err
function M.repo_relative_normalized(absolute, root_path, case_insensitive, original_filename)
  local compare_absolute = absolute
  local compare_root = root_path
  if case_insensitive then
    compare_absolute = compare_absolute:lower()
    compare_root = compare_root:lower()
  end
  if compare_absolute == compare_root then
    return ".", nil
  end
  local prefix = compare_root .. "/"
  if compare_absolute:sub(1, #prefix) ~= prefix then
    return nil, ("Path is outside the git root: %s"):format(original_filename)
  end
  return absolute:sub(#root_path + 2), nil
end

---@param filename string
---@param root string
---@return string? relpath
---@return string? err
function M.repo_relative(filename, root)
  return M.repo_relative_normalized(
    M.normalize_path(filename),
    M.normalize_path(root),
    package.config:sub(1, 1) == "\\",
    filename
  )
end

---@param filename string
---@param root string
---@param case_insensitive boolean?
---@return string? relpath
---@return string? err
function M.repo_relative_for_test(filename, root, case_insensitive)
  return M.repo_relative_normalized(M.normalize_path_text(filename), M.normalize_path_text(root), case_insensitive == true, filename)
end

return M
