local M = {}

local data_path_for_test = nil

local function preference_path()
  return data_path_for_test
    or vim.fs.joinpath(vim.fn.stdpath("data"), "diff-review", "harness", "backend.json")
end

---@param backend_map table<string, DiffReviewHarnessBackendConfig>
---@param fallback string
---@return string
function M.load(backend_map, fallback)
  local path = preference_path()
  if vim.fn.filereadable(path) ~= 1 then return fallback end
  local read_ok, line_list = pcall(vim.fn.readfile, path)
  if not read_ok then return fallback end
  local decode_ok, payload = pcall(vim.json.decode, table.concat(line_list, "\n"))
  if not decode_ok or type(payload) ~= "table" or payload.version ~= 1 then return fallback end
  local backend = type(payload.backend) == "string" and payload.backend or nil
  if not backend or not backend_map[backend] then return fallback end
  return backend
end

---@param backend string
---@return boolean ok
---@return string? error_message
function M.save(backend)
  local path = preference_path()
  local directory = vim.fs.dirname(path)
  local mkdir_ok, mkdir_error = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok then return false, "create backend preference directory: " .. tostring(mkdir_error) end
  local encode_ok, content = pcall(vim.json.encode, { version = 1, backend = backend })
  if not encode_ok then return false, "encode backend preference: " .. tostring(content) end
  local write_ok, write_error = pcall(vim.fn.writefile, { content }, path)
  if not write_ok then return false, "write backend preference: " .. tostring(write_error) end
  return true, nil
end

---@param path? string
function M._set_path_for_test(path) data_path_for_test = path end

return M
