vim.loader.enable(false)

local perf = require("diff_review.infra.perf")

local function assert_equals(actual, expected, message)
  if actual ~= expected then error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2) end
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local test_root = vim.fn.tempname()
local diff_log_path = vim.fs.joinpath(test_root, "diff.jsonl")
local harness_log_path = vim.fs.joinpath(test_root, "harness.jsonl")

---@param path string
---@return string[]
local function log_lines(path)
  if not vim.uv.fs_stat(path) then return {} end
  return vim.fn.readfile(path)
end

local test_success, failure = pcall(function()
  vim.fn.mkdir(test_root, "p")

  perf.configure_from_diff_review_options({
    diff_logging = true,
    harness_logging = false,
    diff_log_path = diff_log_path,
    harness_log_path = harness_log_path,
  })
  assert_true(perf.enabled("diff"), "diff logging should enable its scope")
  assert_equals(perf.enabled("harness"), false, "harness logging should remain disabled")

  perf.event("diff", "status.render", { source = "test" })
  perf.event("harness", "harness.fork", { source = "test" })
  assert_true(vim.wait(1000, function() return #log_lines(diff_log_path) > 0 end, 10), "diff log did not flush")
  assert_equals(vim.uv.fs_stat(harness_log_path), nil, "disabled Harness scope wrote a log")

  local diff_record = vim.json.decode(log_lines(diff_log_path)[1])
  assert_equals(diff_record.scope, "diff", "diff record scope mismatch")
  assert_equals(diff_record.event, "status.render", "diff record event mismatch")

  perf.configure_from_diff_review_options({
    diff_logging = false,
    harness_logging = true,
    diff_log_path = diff_log_path,
    harness_log_path = harness_log_path,
  })
  perf.event("diff", "status.render_again", { source = "test" })
  perf.event("harness", "harness.fork", { source = "test" })
  assert_true(vim.wait(1000, function() return #log_lines(harness_log_path) > 0 end, 10), "Harness log did not flush")

  local harness_record = vim.json.decode(log_lines(harness_log_path)[1])
  assert_equals(harness_record.scope, "harness", "Harness record scope mismatch")
  assert_equals(harness_record.event, "harness.fork", "Harness record event mismatch")
  assert_equals(#log_lines(diff_log_path), 1, "disabled diff scope appended a record")
end)

pcall(vim.fn.delete, test_root, "rf")

if not test_success then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("Perf scopes passed: diff and Harness write independently")
  vim.cmd("qa!")
end
