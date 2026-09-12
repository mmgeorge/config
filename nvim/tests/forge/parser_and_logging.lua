vim.loader.enable(false)

local parse = require("forge.render.diff_parse")
local perf = require("forge.infra.perf")
local cache_root = vim.fn.tempname()
local original_stdpath = vim.fn.stdpath
local original_notify = vim.notify
local warning_list = {}

local ok, failure = xpcall(function()
  local patch = table.concat({
    "diff --git a/test.txt b/test.txt",
    "--- a/test.txt",
    "+++ b/test.txt",
    "@@ -1,2 +1,2 @@",
    " context",
    "-before",
    "+after",
    "",
  }, "\n")
  local hunk = parse.parse_unified_diff(patch)[1].hunks[1]
  local expected = vim.deepcopy(hunk)
  parse.parse_hunk_body(hunk)
  assert(vim.deep_equal(hunk, expected), "repeated parsing changed derived hunk rows or counts")
  assert(hunk.added == 1 and hunk.removed == 1 and #hunk.lines == 3)

  vim.fn.mkdir(cache_root, "p")
  vim.fn.stdpath = function(kind)
    if kind == "cache" then return cache_root end
    return original_stdpath(kind)
  end
  vim.notify = function(message) warning_list[#warning_list + 1] = message end
  perf.setup({ diff = { enabled = true } })
  local log_path = vim.fs.joinpath(cache_root, "forge", "diff-perf.log")
  assert(perf.log_path("diff") == log_path, "default log does not use the Forge cache directory")
  perf.event("diff", "first", {})
  assert(vim.wait(2000, function()
    local stat = vim.uv.fs_stat(log_path)
    return stat ~= nil and stat.size > 0
  end, 10), "first log write did not create its parent directory")
  perf.event("diff", "second", {})
  assert(vim.wait(2000, function()
    return #vim.fn.readfile(log_path) == 2
  end, 10), "second write did not append to the existing log")
  assert(#warning_list == 0, table.concat(warning_list, "\n"))
end, debug.traceback)

vim.fn.stdpath = original_stdpath
vim.notify = original_notify
vim.fn.delete(cache_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
