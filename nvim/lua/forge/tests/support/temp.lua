-- Temporary directories that the current case deletes when it ends, whether or not it failed.
-- Call it inside a case body: it registers the deletion with `MiniTest.finally`, which a hook runs
-- as soon as the hook returns.
-- Evidence that must outlive a failing run belongs in `evidence_directory.lua` instead.
--
--   local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
--   local workspace = temp.directory("workspace")
local temp = {}

---Appends `label` to a fresh temporary name, after a "-" unless the label begins with a space,
---"-", ".", or "_", so a case can choose the exact suffix a path under test must carry.
---@param label? string
---@return string
local function temporary_name(label)
  if not label then return vim.fn.tempname() end
  local separator = label:find("^[ %-%._]") and "" or "-"
  return vim.fn.tempname() .. separator .. label
end

---@param label? string a suffix that names the directory's role in diagnostics
---@return string path an absolute, existing, empty directory
function temp.directory(label)
  local path = temporary_name(label)
  assert(vim.fn.mkdir(path, "p") == 1, "could not create temporary directory " .. path)
  path = vim.fs.normalize(vim.uv.fs_realpath(path) or path)
  MiniTest.finally(function() pcall(vim.fn.delete, path, "rf") end)
  return path
end

---@param label? string
---@param lines? string[] contents to write
---@return string path a temporary file deleted when the case ends
function temp.file(label, lines)
  local path = vim.fs.normalize(temporary_name(label))
  if lines then assert(vim.fn.writefile(lines, path) == 0, "could not write " .. path) end
  MiniTest.finally(function() pcall(vim.fn.delete, path) end)
  return path
end

return temp
