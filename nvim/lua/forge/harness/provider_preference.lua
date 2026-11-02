local M = {}

local data_path_for_test = nil

local function preference_path()
  return data_path_for_test
    or vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness", "provider.json")
end

---@param provider_map table<string, ForgeHarnessProviderConfig>
---@param fallback string
---@return string
function M.load(provider_map, fallback)
  local path = preference_path()
  if not data_path_for_test and vim.fn.filereadable(path) ~= 1 then
    path = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness", "backend.json")
  end
  if vim.fn.filereadable(path) ~= 1 then return fallback end
  local read_ok, line_list = pcall(vim.fn.readfile, path)
  if not read_ok then return fallback end
  local decode_ok, payload = pcall(vim.json.decode, table.concat(line_list, "\n"))
  if not decode_ok or type(payload) ~= "table" or payload.version ~= 1 then return fallback end
  local selected = payload.provider or payload.backend
  local provider = type(selected) == "string" and selected or nil
  if not provider or not provider_map[provider] then return fallback end
  return provider
end

---@param provider string
---@return boolean ok
---@return string? error_message
function M.save(provider)
  local path = preference_path()
  local directory = vim.fs.dirname(path)
  local mkdir_ok, mkdir_error = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok then return false, "create provider preference directory: " .. tostring(mkdir_error) end
  local encode_ok, content = pcall(vim.json.encode, { version = 1, provider = provider })
  if not encode_ok then return false, "encode provider preference: " .. tostring(content) end
  local write_ok, write_error = pcall(vim.fn.writefile, { content }, path)
  if not write_ok then return false, "write provider preference: " .. tostring(write_error) end
  return true, nil
end

---@param path? string
function M._set_path_for_test(path) data_path_for_test = path end

return M
