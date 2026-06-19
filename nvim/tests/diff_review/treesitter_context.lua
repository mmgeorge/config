vim.loader.enable(false)

local diff_review = require("diff_review")
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
  diff_review.compute_hunk_context_async(filename, 3, function(context)
    struct_context = context
  end)
  wait_for(function() return struct_context ~= nil end, "struct context did not resolve")
  assert_true(type(struct_context) == "table", "struct context was not structured")
  assert_true(struct_context.label == "Engine", "wrong struct label: " .. tostring(struct_context.label))
  assert_true(struct_context.start_text == "pub struct Engine {", "wrong struct start: " .. tostring(struct_context.start_text))
  assert_true(struct_context.end_text == "}", "wrong struct end: " .. tostring(struct_context.end_text))

  local function_context = nil
  diff_review.compute_hunk_context_async(filename, 9, function(context)
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
  local unloaded_context = nil
  diff_review.compute_hunk_context_async(filename, 3, function(context)
    unloaded_context = context
  end)
  wait_for(function() return unloaded_context ~= nil end, "unloaded file context did not resolve")
  assert_true(type(unloaded_context) == "table", "unloaded file context was not structured")
  assert_true(unloaded_context.label == "Engine", "wrong unloaded label: " .. tostring(unloaded_context.label))
  assert_true(unloaded_context.start_text == "pub struct Engine {", "wrong unloaded start: " .. tostring(unloaded_context.start_text))
  assert_true(unloaded_context.end_text == "}", "wrong unloaded end: " .. tostring(unloaded_context.end_text))
end

local ok, err = xpcall(run, debug.traceback)
cleanup()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
