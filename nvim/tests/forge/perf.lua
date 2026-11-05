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
  perf.event("harness", "ui.document.slices", { slices = 3, maximum_ms = 54,
    maximum_prepare_ms = 4, maximum_commit_ms = 53 })
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
  assert_equals(records["ui.document.slices:nil"].maximum_prepare_ms, 4)
  assert_equals(records["ui.document.slices:nil"].maximum_commit_ms, 53)
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
  assert_equals(#(perf.queue.harness or {}), 4096, "pending logging queue exceeded its record bound")
  assert_true(perf.rejected_record_count.harness >= 5904, "saturation was not accounted")
  perf.flush("harness")
  assert_true(vim.wait(1000, function() return #(perf.queue.harness or {}) == 0 end, 10))

  assert_true(vim.wait(1500, function()
    local found = 0
    for _, line in ipairs(log_lines(harness_log_path)) do
      local decoded = vim.json.decode(line)
      if decoded.event == "bounded" then found = found + 1 end
    end
    return found == 4096
  end, 10), "accepted diagnostic records did not flush before rotation")
  perf._test_file_limit_bytes = 2048
  local fd = assert(vim.uv.fs_open(harness_log_path, "w", 438))
  assert(vim.uv.fs_ftruncate(fd, perf._test_file_limit_bytes))
  assert(vim.uv.fs_close(fd))
  perf.event("harness", "harness.log_rollover", { source = "test" })
  assert_true(vim.wait(1000, function()
    local lines = log_lines(harness_log_path)
    for _, line in ipairs(lines) do
      local decoded, record = pcall(vim.json.decode, line)
      if decoded and record.event == "harness.log_rollover" then return true end
    end
    return false
  end, 10), "Harness log did not rotate data beyond the per-scope retention limit")
  assert_true(vim.uv.fs_stat(harness_log_path .. ".1") ~= nil, "rotation erased the previous segment")
  local chunk_path = vim.fs.joinpath(test_root, "chunks.jsonl")
  perf.setup({ harness = { log_path = chunk_path } })
  perf._test_file_limit_bytes = 2048
  local operation_id = string.rep("\206\187\240\159\153\130", 600)
  perf.event("harness", "chunked.record", { operation_id = operation_id })
  perf.event("harness", "after.chunk", { operation_id = "next" })
  perf.flush("harness")
  assert_true(vim.wait(1500, function()
    for _, line in ipairs(log_lines(chunk_path)) do
      local value = vim.json.decode(line)
      if value.event == "after.chunk" then return true end
    end
    return false
  end, 10), "chunked records did not finish writing")
  local parts, expected_sequence, record_id, part_count = {}, 0, nil, nil
  local found_successor = false
  for generation = 10, 0, -1 do
    local path = generation == 0 and chunk_path or chunk_path .. "." .. generation
    local stat = vim.uv.fs_stat(path)
    if stat then
      assert_true(stat.size <= 2048, "physical diagnostic file exceeded injected budget")
      for _, line in ipairs(log_lines(path)) do
        local record = vim.json.decode(line)
        if record.trace_chunk then
          local chunk = record.trace_chunk
          record_id = record_id or chunk.id
          assert_equals(chunk.id, record_id)
          assert_equals(chunk.sequence, expected_sequence)
          expected_sequence = expected_sequence + 1
          part_count = chunk.part_count
          parts[#parts + 1] = chunk.payload_hex:gsub("..", function(pair) return string.char(tonumber(pair, 16)) end)
        elseif record.event == "after.chunk" then found_successor = true end
      end
    end
  end
  assert_equals(expected_sequence, part_count, "chunk rotation lost part of the logical record")
  assert_equals(vim.json.decode(table.concat(parts)).operation_id, operation_id)
  assert_true(found_successor, "batch splitting lost the following ordinary record")
  perf._test_file_limit_bytes = nil

end)

perf._test_file_limit_bytes = nil
perf.setup({ harness = { enabled = false }, diff = { enabled = false } })
pcall(vim.fn.delete, test_root, "rf")

if not test_success then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("Perf scopes passed: diff and Harness write independently")
  vim.cmd("qa!")
end
