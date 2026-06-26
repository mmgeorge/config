vim.loader.enable(false)

local git_data = require("diff_review.git.git_data")
local session = require("diff_review.session")
local status_render = require("diff_review.views.status.status_render")
local syntax_engine = require("diff_review.render.syntax_engine")

local original_compute_diff_syntax_async = git_data.compute_diff_syntax_async
local original_request_current_model_render = status_render.status_request_current_model_render
local original_status = session.status
local test_root = vim.fs.normalize(vim.fn.getcwd() .. "/.diffreview-syntax-rerender-test")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function diff_text(suffix)
  return table.concat({
    "diff --git a/src/main.rs b/src/main.rs",
    "index 1111111..2222222 100644",
    "--- a/src/main.rs",
    "+++ b/src/main.rs",
    "@@ -1 +1 @@",
    "-old_" .. suffix,
    "+new_" .. suffix,
  }, "\n")
end

local function install_immediate_syntax_worker()
  local compute_count = 0
  git_data.compute_diff_syntax_async = function(_, _, callback)
    compute_count = compute_count + 1
    callback({ fake_syntax = true })
  end
  return function()
    return compute_count
  end
end

local function install_held_syntax_worker()
  local held_callbacks = {}
  git_data.compute_diff_syntax_async = function(_, _, callback)
    held_callbacks[#held_callbacks + 1] = callback
  end
  return held_callbacks
end

local function run()
  vim.fn.delete(test_root, "rf")
  assert_true(vim.fn.mkdir(test_root, "p") == 1, "mkdir failed")
  local filename = test_root .. "/main.rs"
  assert_true(vim.fn.writefile({ "new" }, filename) == 0, "writefile failed")

  local status_buf = vim.api.nvim_create_buf(false, true)
  session.status = { buf = status_buf }

  local request_count = 0
  local last_request_opts = nil
  status_render.status_request_current_model_render = function(opts)
    request_count = request_count + 1
    last_request_opts = opts
  end

  syntax_engine.clear_diff_syntax_cache()
  local compute_count = install_immediate_syntax_worker()
  local callback_count = 0
  local syntax, pending = syntax_engine.cached_diff_syntax(filename, diff_text("explicit"), "new", "explicit", function(received_syntax)
    callback_count = callback_count + 1
    assert_true(received_syntax ~= nil, "callback did not receive syntax")
  end)
  assert_true(syntax == nil and pending == true, "explicit callback request did not start pending syntax")
  assert_true(compute_count() == 1, "explicit callback request did not compute syntax")
  assert_true(callback_count == 1, "explicit callback did not run")
  assert_true(request_count == 0, "explicit callback also requested passive rerender")

  syntax_engine.clear_diff_syntax_cache()
  request_count = 0
  last_request_opts = nil
  compute_count = install_immediate_syntax_worker()
  syntax, pending = syntax_engine.cached_diff_syntax(filename, diff_text("passive"), "new", "passive", nil)
  assert_true(syntax == nil and pending == true, "passive request did not start pending syntax")
  assert_true(compute_count() == 1, "passive request did not compute syntax")
  assert_true(request_count == 1, "passive request did not request rerender")
  assert_true(last_request_opts and last_request_opts.clear_fancy_rows == false, "passive rerender cleared fancy rows")
  assert_true(last_request_opts and last_request_opts.skip_operations == true, "passive rerender ignored operation queue")

  syntax_engine.clear_diff_syntax_cache()
  request_count = 0
  local held_callbacks = install_held_syntax_worker()
  callback_count = 0
  syntax_engine.cached_diff_syntax(filename, diff_text("joined"), "new", "joined-passive", nil)
  syntax_engine.cached_diff_syntax(filename, diff_text("joined"), "new", "joined-explicit", function(received_syntax)
    callback_count = callback_count + 1
    assert_true(received_syntax ~= nil, "joined callback did not receive syntax")
  end)
  assert_true(#held_callbacks == 1, "joined request started more than one syntax worker")
  held_callbacks[1]({ fake_syntax = true })
  assert_true(callback_count == 1, "joined callback did not run")
  assert_true(request_count == 0, "joined callback also requested passive rerender")

  syntax_engine.clear_diff_syntax_cache()
  request_count = 0
  compute_count = install_immediate_syntax_worker()
  callback_count = 0
  syntax, pending = syntax_engine.cached_old_file_syntax(filename, diff_text("old-file"), "old-file", function(received_syntax)
    callback_count = callback_count + 1
    assert_true(received_syntax ~= nil, "old-file callback did not receive syntax")
  end)
  assert_true(syntax == nil and pending == true, "old-file callback request did not start pending syntax")
  assert_true(compute_count() == 1, "old-file callback request did not compute syntax")
  assert_true(callback_count == 1, "old-file callback did not run")
  assert_true(request_count == 0, "old-file callback also requested passive rerender")

  if vim.api.nvim_buf_is_valid(status_buf) then
    vim.api.nvim_buf_delete(status_buf, { force = true })
  end
end

local run_ok, run_err = xpcall(run, debug.traceback)
vim.fn.delete(test_root, "rf")
syntax_engine.clear_diff_syntax_cache()
git_data.compute_diff_syntax_async = original_compute_diff_syntax_async
status_render.status_request_current_model_render = original_request_current_model_render
session.status = original_status
if not run_ok then
  vim.api.nvim_err_writeln(run_err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
