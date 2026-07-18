local module = {}

---@return string
function module.data_dir()
  return vim.fs.joinpath(vim.fn.stdpath("data"), "markdown-math")
end

---@return string
function module.tool_dir()
  local module_dir = vim.fs.dirname(debug.getinfo(1, "S").source:sub(2))
  local config_dir = vim.fs.dirname(vim.fs.dirname(module_dir))
  return vim.fs.joinpath(config_dir, "tools", "markdown-math")
end

---@return string
function module.executable_path()
  local executable = vim.fn.has("win32") == 1 and "utftex.cmd" or "utftex"
  return vim.fs.joinpath(module.data_dir(), "node_modules", ".bin", executable)
end

---@return string[]
function module.install_command()
  return {
    vim.fn.exepath("npm"),
    "install",
    "--prefix",
    module.data_dir(),
    "--install-links",
    "--omit=dev",
    "--no-audit",
    "--no-fund",
    module.tool_dir(),
  }
end

return module
