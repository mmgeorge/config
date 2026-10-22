local root = vim.fn.getcwd()
local directory = vim.fn.tempname()
local package_directory = directory .. "/packages/core"
vim.fn.mkdir(package_directory, "p")
package_directory = assert(vim.uv.fs_realpath(package_directory))
local previous_snacks = _G.Snacks
_G.Snacks = { git = { get_root = function() return directory end } }

local config = dofile(root .. "/nvim/lua/plugins/snacks.lua")
config[1].init()
vim.fn.chdir(package_directory)
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_win_set_buf(0, buffer)
assert(vim.fn.getcwd() == package_directory, "BufEnter replaced the package-local editor cwd")
vim.api.nvim_exec_autocmds("BufEnter", { buffer = buffer })
assert(vim.fn.getcwd() == package_directory, "re-entering a buffer reset cwd to the Git root")

vim.fn.chdir(root)
_G.Snacks = previous_snacks
vim.fn.delete(directory, "rf")
print("editor_cwd: package-local cwd survives buffer entry")
