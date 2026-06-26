vim.loader.enable(false)

local diff_review = require("diff_review")
local git_data = require("diff_review.git.git_data")
local syntax_engine = require("diff_review.render.syntax_engine")
local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local test_root = vim.fs.normalize(original_cwd .. "/.diffreview-treesitter-context-test")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function cleanup()
  pcall(vim.cmd, "silent! bwipeout!")
  syntax_engine.clear_treesitter_source_buffers()
  vim.fn.delete(test_root, "rf")
end

local function run()
  vim.fn.delete(test_root, "rf")
  assert_true(vim.fn.mkdir(test_root, "p") == 1, "mkdir failed")
  local filename = test_root .. "/engine.rs"
  assert_true(vim.fn.writefile({
    "pub struct Engine {",
    "    world: ecs::World,",
    "    last_draw: time::Instant,",
    "}",
    "",
    "impl Engine {",
    "    pub fn new() -> Self {",
    "        Self {",
    "            last_draw: time::Instant::now(),",
    "        }",
    "    }",
    "}",
  }, filename) == 0, "writefile failed")

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].swapfile = false
  vim.api.nvim_buf_set_name(buf, filename)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.fn.readfile(filename))
  vim.api.nvim_set_current_buf(buf)
  vim.bo[buf].filetype = "rust"

  local struct_context = nil
  git_data.compute_hunk_context_async(filename, 3, function(context)
    struct_context = context
  end)
  wait_for(function() return struct_context ~= nil end, "struct context did not resolve")
  assert_true(type(struct_context) == "table", "struct context was not structured")
  assert_true(struct_context.label == "Engine", "wrong struct label: " .. tostring(struct_context.label))
  assert_true(struct_context.start_text == "pub struct Engine {", "wrong struct start: " .. tostring(struct_context.start_text))
  assert_true(struct_context.end_text == "}", "wrong struct end: " .. tostring(struct_context.end_text))

  local function_context = nil
  git_data.compute_hunk_context_async(filename, 9, function(context)
    function_context = context
  end)
  wait_for(function() return function_context ~= nil end, "function context did not resolve")
  assert_true(type(function_context) == "table", "function context was not structured")
  assert_true(function_context.label == "Engine.new", "wrong function label: " .. tostring(function_context.label))
  assert_true(function_context.start_text == "    pub fn new() -> Self {", "wrong function start: " .. tostring(function_context.start_text))
  assert_true(function_context.end_text:find("    }", 1, true) ~= nil, "wrong function end: " .. tostring(function_context.end_text))
  assert_true(#function_context.start_segments > 1, "function start did not produce syntax segments")
  local saw_keyword = false
  for _, segment in ipairs(function_context.start_segments) do
    if segment.hl_group == "@keyword" or segment.hl_group == "@keyword.function" then
      saw_keyword = true
      break
    end
  end
  assert_true(saw_keyword, "function start did not include keyword syntax highlight")

  vim.api.nvim_buf_delete(buf, { force = true })
  local filetype_events = {}
  local filetype_autocmd = vim.api.nvim_create_autocmd("FileType", {
    callback = function(event)
      filetype_events[#filetype_events + 1] = {
        buf = event.buf,
        filetype = vim.bo[event.buf].filetype,
      }
    end,
  })
  local unloaded_context = nil
  git_data.compute_hunk_context_async(filename, 3, function(context)
    unloaded_context = context
  end)
  wait_for(function() return unloaded_context ~= nil end, "unloaded file context did not resolve")
  vim.api.nvim_del_autocmd(filetype_autocmd)
  assert_true(type(unloaded_context) == "table", "unloaded file context was not structured")
  assert_true(unloaded_context.label == "Engine", "wrong unloaded label: " .. tostring(unloaded_context.label))
  assert_true(unloaded_context.start_text == "pub struct Engine {", "wrong unloaded start: " .. tostring(unloaded_context.start_text))
  assert_true(unloaded_context.end_text == "}", "wrong unloaded end: " .. tostring(unloaded_context.end_text))
  assert_true(#filetype_events == 0, "syntax source buffer fired FileType: " .. vim.inspect(filetype_events))

  local scratch_buf = syntax_engine.treesitter_source_buffer(filename)
  assert_true(scratch_buf ~= nil, "syntax source scratch buffer was not cached")
  assert_true(vim.bo[scratch_buf].filetype == "", "syntax source scratch buffer set filetype")
  assert_true(vim.b[scratch_buf].diff_review_syntax_filetype == "rust", "syntax source scratch buffer missed parser filetype")

  local diff_filetype_events = {}
  local diff_filetype_autocmd = vim.api.nvim_create_autocmd("FileType", {
    callback = function(event)
      diff_filetype_events[#diff_filetype_events + 1] = {
        buf = event.buf,
        filetype = vim.bo[event.buf].filetype,
      }
    end,
  })
  local diff_syntax = nil
  git_data.compute_diff_syntax_async(filename, vim.fn.readfile(filename), function(syntax)
    diff_syntax = syntax or false
  end)
  wait_for(function() return diff_syntax ~= nil end, "diff syntax did not resolve")
  vim.api.nvim_del_autocmd(diff_filetype_autocmd)
  assert_true(diff_syntax ~= false, "diff syntax failed to parse")
  assert_true(#diff_filetype_events == 0, "diff syntax buffer fired FileType: " .. vim.inspect(diff_filetype_events))
  assert_true(vim.bo[diff_syntax.buf].filetype == "", "diff syntax scratch buffer set filetype")
  assert_true(vim.b[diff_syntax.buf].diff_review_syntax_filetype == "rust", "diff syntax scratch buffer missed parser filetype")
  pcall(vim.api.nvim_buf_delete, diff_syntax.buf, { force = true })
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
