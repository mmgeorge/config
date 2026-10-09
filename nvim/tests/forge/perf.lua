vim.loader.enable(false)

local perf = require("forge.infra.perf")

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

  perf.configure_from_forge_options({
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

  perf.configure_from_forge_options({
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
  local first, middle, last = perf.trace("harness", "test.callback", { request_id = "request", body = "private" }, function()
    return "first", nil, "last"
  end)
  assert_equals(first, "first")
  assert_equals(middle, nil)
  assert_equals(last, "last", "tracing lost a result after nil")
  local succeeded, callback_error = pcall(perf.trace, "harness", "test.failure", {}, function()
    error("expected callback failure")
  end)
  assert_true(not succeeded and callback_error:find("expected callback failure", 1, true), "tracing swallowed a callback failure")
  vim.uv.sleep(450)
  local records = {}
  assert_true(vim.wait(1500, function()
    for _, line in ipairs(log_lines(harness_log_path)) do
      local record = vim.json.decode(line)
      records[record.event .. ":" .. tostring(record.phase)] = record
    end
    return records["ui.loop_lag:nil"] ~= nil and records["test.failure:end"] ~= nil
  end, 10), "UI delay or callback traces did not reach the async log")
  local begin, finish = records["test.callback:begin"], records["test.callback:end"]
  assert_true(begin and finish and begin.span_id == finish.span_id, "callback trace boundaries lost correlation")
  assert_true(finish.elapsed_ms >= 0 and finish.status == "ok")
  assert_equals(records["test.failure:end"].status, "error")
  assert_equals(begin.body, nil, "callback tracing retained private content")
  assert_true(records["ui.loop_lag:nil"].elapsed_ms >= 100, "UI delay reported below its threshold")
  perf.setup({ harness = { enabled = false } })
  local called = false
  perf.trace("harness", "test.disabled", {}, function() called = true end)
  assert_true(called, "disabled tracing skipped its callback")
  perf.setup({ harness = { enabled = true } })
  local cyclic = { source = string.rep("x", 1024 * 1024), body = "private payload", prompt = "private prompt", authorization = "private credential" }
  cyclic.nested = cyclic
  local bounded = perf.payload(cyclic)
  assert_equals(#bounded.source, 256)
  assert_equals(bounded.body, nil)
  assert_equals(bounded.prompt, nil)
  assert_equals(bounded.authorization, nil)
  assert_equals(bounded.nested, nil, "routine logging retained recursive payload")
  for _ = 1, 10000 do perf.event("harness", "bounded", cyclic) end
  local bytes = 0
  for _, line in ipairs(perf.queue.harness or {}) do bytes = bytes + #line + 1 end
  assert_true(bytes <= 256 * 1024, "pending logging queue exceeded its bound")
  perf.flush("harness")
  assert_true(vim.wait(1000, function() return #(perf.queue.harness or {}) == 0 end, 10))

  vim.fn.writefile({ string.rep("x", 15 * 1024 * 1024) }, harness_log_path)
  perf.event("harness", "harness.log_rollover", { source = "test" })
  assert_true(vim.wait(1000, function()
    local lines = log_lines(harness_log_path)
    local decoded = #lines == 1 and { pcall(vim.json.decode, lines[1]) } or nil
    return decoded and decoded[1] and decoded[2].event == "harness.log_rollover"
  end, 10), "Harness log did not replace data beyond the per-scope retention limit")
end)

perf.setup({ harness = { enabled = false }, diff = { enabled = false } })
pcall(vim.fn.delete, test_root, "rf")

if not test_success then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("Perf scopes passed: diff and Harness write independently")
  vim.cmd("qa!")
end
