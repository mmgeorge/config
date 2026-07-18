vim.loader.enable(false)

local dependency = require("markdown_math.dependency")

local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

local function assert_true(value, message)
  if not value then fail(message) end
end

local executable = dependency.executable_path()
assert_true(vim.fn.executable(executable) == 1, "the managed utftex executable must be installed")
assert_true(
  vim.uv.fs_stat(vim.fs.joinpath(dependency.tool_dir(), "package.json")) ~= nil,
  "the wrapper package must resolve relative to its source module"
)
local install_command = dependency.install_command()
assert_true(vim.list_contains(install_command, "--install-links"), "the local wrapper must install as a package tree")
assert_true(install_command[#install_command] == dependency.tool_dir(), "the install command must target the wrapper package")

local formula = table.concat({
  "L_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega}",
  "f_r(x,\\omega_i,\\omega_o)\\, L_i(x,\\omega_i)\\, \\cos\\theta_i\\,d\\omega_i",
}, " ")
local result = vim.system({ executable }, { stdin = formula, text = true }):wait()
assert_true(result.code == 0, "managed utftex conversion must succeed: " .. vim.trim(result.stderr or ""))
local normalized_output = (result.stdout or ""):gsub("\r", ""):gsub("\n+$", "")
local output = vim.split(normalized_output, "\n", { plain = true })
assert_true(#output == 4, "managed utftex conversion must preserve the integral's four-row layout")
assert_true(output[1]:find("⌠", 1, true) ~= nil, "managed utftex output must contain the integral top")
assert_true(output[1]:find("^%s+⌠") ~= nil, "managed utftex output must preserve the integral's horizontal alignment")
assert_true(output[4]:find("Ω", 1, true) ~= nil, "managed utftex output must contain the lower bound")

io.write("markdown_math_dependency OK\n")
vim.cmd("qa!")
