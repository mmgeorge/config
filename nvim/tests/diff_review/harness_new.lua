vim.loader.enable(false)

local builder = require("diff_review.harness.builder")
local client = require("diff_review.harness.client")
local diff_review = require("diff_review")
local session = require("diff_review.session")

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. vim.inspect(expected) .. ", got " .. vim.inspect(actual), 2)
  end
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local test_root = vim.fn.tempname()
local crate_root = vim.fs.joinpath(test_root, "crate")
vim.fn.mkdir(vim.fs.joinpath(crate_root, "src"), "p")
vim.fn.writefile({ "[package]", 'name = "diff-review-harness"', 'version = "0.1.0"' },
  vim.fs.joinpath(crate_root, "Cargo.toml"))
vim.fn.writefile({ "fn main() {}" }, vim.fs.joinpath(crate_root, "src", "main.rs"))

local initialize_count = 0
local session_new_count = 0
local session_sequence = 0
local initialize_request

local function snapshot(name)
  session_sequence = session_sequence + 1
  return {
    session = {
      id = "session-" .. session_sequence,
      name = name or "",
      workspace = vim.fn.getcwd(),
      backend = "mock",
      model = "mock-model",
      provider_label = "Mock CLI",
      resolved_model = "mock-model",
      effort = "low",
      execution_mode = "read",
      native_fork = false,
      native_compact = true,
    },
    interaction = {},
    timeline = {},
    capability = { native_fork = false, native_compact = true },
    no_checkpoint = false,
    artifact = {},
    approval = {},
    agent = { definition = {}, run = {}, turn = {} },
    prompt_history = {},
  }
end

local function emit(options, message)
  options.stdout(nil, vim.json.encode(message) .. "\n")
end

local function fake_launcher(_, options, _)
  local process = {}
  function process:write(payload)
    local request = vim.json.decode(vim.trim(payload))
    if request.method == "initialize" then
      initialize_count = initialize_count + 1
      initialize_request = request
      emit(options, { id = request.id, result = snapshot(request.params.new_session_name) })
    elseif request.method == "session.new" then
      session_new_count = session_new_count + 1
      if request.params.name == "fail" then
        emit(options, { id = request.id, error = { message = "mock new-session failure" } })
      else
        emit(options, { id = request.id, result = snapshot(request.params.name) })
      end
    elseif request.method == "shutdown" then
      emit(options, { id = request.id, result = { shutdown = true } })
    else
      error("unexpected Harness request: " .. request.method)
    end
  end
  function process:kill() end
  return process
end

local test_success, failure = pcall(function()
  diff_review.setup({ perf_logging = false, harness = { backend = "mock" } })
  builder._set_crate_dir_for_test(crate_root)
  builder._set_artifact_root_for_test(vim.fs.joinpath(test_root, "artifacts"))
  builder._set_runner_for_test(function(_, _, callback)
    vim.fn.mkdir(vim.fs.dirname(builder.release_binary_path()), "p")
    vim.fn.writefile({ "fake" }, builder.release_binary_path())
    callback({ code = 0, stdout = "", stderr = "" })
  end)
  client._set_launcher_for_test(fake_launcher)

  local baseline_tab_count = vim.fn.tabpagenr("$")
  require("diff_review.views.harness").new_session()
  assert_equals(vim.fn.tabpagenr("$"), baseline_tab_count + 1,
    "cold HarnessNew should create exactly one timeline tab")
  assert_true(vim.wait(1000, function()
    return session.harness.session and session.harness.session.id == "session-1"
  end, 10), "cold HarnessNew should initialize the requested session")
  assert_equals(initialize_count, 1, "cold HarnessNew should initialize one broker")
  assert_equals(session_new_count, 0, "cold HarnessNew should not create an orphan session after initialization")
  assert_equals(initialize_request.params.new_session_name, "",
    "cold HarnessNew should explicitly request a fresh unnamed session")

  local cold_tab = session.harness.timeline_tab
  require("diff_review.views.harness").new_session("warm start")
  assert_equals(vim.fn.tabpagenr("$"), baseline_tab_count + 2,
    "warm HarnessNew should create one additional timeline tab")
  assert_true(vim.wait(1000, function()
    return session.harness.session and session.harness.session.name == "warm start"
  end, 10), "warm HarnessNew should complete through session.new")
  assert_equals(initialize_count, 1, "warm HarnessNew should reuse the running broker")
  assert_equals(session_new_count, 1, "warm HarnessNew should create one broker session")
  assert_true(vim.api.nvim_tabpage_is_valid(cold_tab), "warm HarnessNew should preserve the prior timeline tab")

  vim.cmd("tabclose!")
  vim.cmd("tabclose!")
  assert_equals(vim.fn.tabpagenr("$"), baseline_tab_count, "closing Harness timelines should restore the baseline")
  require("diff_review.views.harness").new_session("after close")
  assert_equals(vim.fn.tabpagenr("$"), baseline_tab_count + 1,
    "HarnessNew after closing every timeline should create exactly one tab")
  assert_true(vim.wait(1000, function()
    return session.harness.session and session.harness.session.name == "after close"
  end, 10), "HarnessNew should remain usable after every timeline closes")
  assert_equals(session_new_count, 2, "HarnessNew after tab closure should reuse the running broker")

  local successful_tab = session.harness.timeline_tab
  require("diff_review.views.harness").new_session("fail")
  assert_equals(vim.fn.tabpagenr("$"), baseline_tab_count + 2,
    "a failed HarnessNew should retain exactly one provisional tab")
  assert_true(vim.wait(1000, function()
    local line_list = vim.api.nvim_buf_get_lines(session.harness.transcript_buf, 0, -1, false)
    return vim.tbl_contains(line_list, "  New session failed: mock new-session failure")
  end, 10), "a failed HarnessNew should explain the failure in its provisional timeline")
  assert_true(vim.api.nvim_tabpage_is_valid(successful_tab),
    "a failed HarnessNew should preserve the previously active timeline")
end)

client._reset_for_test()
builder._reset_for_test()
pcall(vim.fn.delete, test_root, "rf")

if not test_success then
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buffer) then vim.bo[buffer].modified = false end
  end
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("HarnessNew workflows passed: cold, warm, after closing every timeline, and failure")
  vim.cmd("qa!")
end
