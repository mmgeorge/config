-- The scripted `gh` stand-in from `forge-github/tests/fixtures/gh.rs`, compiled once per source
-- revision into Neovim's cache directory and reused by later runs. A case puts the returned
-- directory first on the host's PATH so the real host launches the stand-in as `gh`.
--
--   local gh_fixture = dofile("nvim/lua/github/tests/support/gh_fixture.lua")
--   local gh_directory = gh_fixture.directory()

local gh_fixture = {}

local source_path = "nvim/rust/forge/crates/forge-github/tests/fixtures/gh.rs"
local toolchain = "1.94.0"
local compile_timeout_ms = 120000

---@return string directory a directory whose `gh` executable is the compiled stand-in
function gh_fixture.directory()
  local source = table.concat(vim.fn.readfile(source_path, "b"), "\n")
  local revision = vim.fn.sha256(toolchain .. "\0" .. source):sub(1, 16)
  local directory = vim.fs.joinpath(vim.fn.stdpath("cache"), "forge-gh-fixture", revision)
  local executable = vim.fs.joinpath(directory, vim.fn.has("win32") == 1 and "gh.exe" or "gh")
  if vim.fn.executable(executable) == 1 then return directory end
  -- Compile to a private name and rename it into place, so a concurrent run never launches a
  -- partly written executable.
  vim.fn.mkdir(directory, "p")
  local pending = ("%s.pending-%d"):format(executable, vim.fn.getpid())
  local result = vim.system({ "rustup", "run", toolchain, "rustc", "--edition=2024", source_path, "-o", pending },
    { text = true, timeout = compile_timeout_ms }):wait()
  assert(result.code == 0, "gh fixture compilation failed: " .. tostring(result.stderr))
  if not vim.uv.fs_rename(pending, executable) then pcall(vim.fn.delete, pending) end
  assert(vim.fn.executable(executable) == 1, "gh fixture is not executable: " .. executable)
  return directory
end

return gh_fixture
