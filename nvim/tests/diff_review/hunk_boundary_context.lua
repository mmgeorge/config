vim.loader.enable(false)

local diff_review = require("diff_review")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-boundary-context-test")
local calls = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local diff_text = table.concat({
  "diff --git a/src/engine.rs b/src/engine.rs",
  "index 1111111..2222222 100644",
  "--- a/src/engine.rs",
  "+++ b/src/engine.rs",
  "@@ -11,2 +11,2 @@",
  "-    let stderr_layer = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
  "+    let stderr_laye = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
  "     let writer = LOG_WRITER.get_or_init(|| SwappableWriter::new(open_log()));",
  "@@ -12,2 +12,2 @@",
  "-    let writer = LOG_WRITER.get_or_init(|| SwappableWriter::new(open_log()));",
  "+    let writer = LOG_WRITER.get_or_int(|| SwappableWriter::new(open_log()));",
  "   }",
}, "\n")

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  calls[#calls + 1] = { kind = "systemlist", key = command_key(command) }
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "boundary test" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tsrc/engine.rs" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    return vim.split(diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then
    return {}, 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system(command, input)
  calls[#calls + 1] = { kind = "system", key = command_key(command), input = input }
  return "", 0
end

function backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = backend.system(command, input)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 5)
end

local function contains_line(lines, pattern)
  for _, line in ipairs(lines) do
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

local function count_lines(lines, pattern)
  local count = 0
  for _, line in ipairs(lines) do
    if line:find(pattern, 1, true) then count = count + 1 end
  end
  return count
end

local function buffer_contains(buf, pattern)
  return contains_line(vim.api.nvim_buf_get_lines(buf, 0, -1, false), pattern)
end

local function buffer_dump(buf)
  return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
end

local function find_row(buf, pattern)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(pattern, 1, true) then return index end
  end
  error("missing row: " .. pattern .. "\n" .. table.concat(lines, "\n"), 2)
end

local function line_has_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group or tostring(details.hl_group):find("^" .. vim.pesc(hl_group) .. "%.") then return true end
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        if group == hl_group or tostring(group):find("^" .. vim.pesc(hl_group) .. "%.") then return true end
      end
    end
  end
  return false
end

local function line_highlights(buf, row)
  local groups = {}
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        groups[#groups + 1] = tostring(group)
      end
    elseif details.hl_group then
      groups[#groups + 1] = tostring(details.hl_group)
    end
  end
  return table.concat(groups, ",")
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function system_call_count()
  local count = 0
  for _, call in ipairs(calls) do
    if call.kind == "system" then count = count + 1 end
  end
  return count
end

local function run()
  vim.fn.delete(root, "rf")
  assert_true(vim.fn.mkdir(root .. "/src", "p") == 1, "mkdir failed")
  assert_true(vim.fn.writefile({
    "pub struct Engine {",
    "  world: ecs::World,",
    "  start: time::Instant,",
    "  last_draw: time::Instant,",
    "  since_start: time::Duration,",
    "  since_last_draw: time::Duration,",
    "}",
    "",
    "impl Engine {",
    "  pub fn new(bridge: Bridge) -> Self {",
    "    let stderr_layer = tracing_subscriber::fmt::layer().with_writer(std::io::stderr);",
    "    let writer = LOG_WRITER.get_or_init(|| SwappableWriter::new(open_log()));",
    "  }",
    "}",
  }, root .. "/src/engine.rs") == 0, "writefile failed")
  diff_review.set_git_backend(backend)

  diff_review.setup()
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "engine.rs +2 -2") end, "status did not render file\n" .. buffer_dump(buf))

  trigger_normal_mapping("<Tab>", find_row(buf, "engine.rs"))
  wait_for(function()
    return buffer_contains(buf, "@@ +1 -1") or buffer_contains(buf, "@@ Engine.new +1 -1")
  end, "compact hunk did not render\n" .. buffer_dump(buf))
  local delete_row = find_row(buf, "let stderr_layer = tracing_subscriber")
  local delete_line = vim.api.nvim_buf_get_lines(buf, delete_row - 1, delete_row, false)[1]
  assert_true(delete_line:find("^%s*11%s+%s+%- ", 1) ~= nil, "delete row did not use local gutter: " .. delete_line)
  local add_row = find_row(buf, "let stderr_laye = tracing_subscriber")
  local add_line = vim.api.nvim_buf_get_lines(buf, add_row - 1, add_row, false)[1]
  assert_true(add_line:find("^%s+%s+11%s+%+ ", 1) ~= nil, "add row did not use local gutter: " .. add_line)
  wait_for(function()
    local row = find_row(buf, "let stderr_layer = tracing_subscriber")
    return line_has_highlight(buf, row, "@keyword")
  end, "delete hunk body row did not get Tree-sitter keyword highlight: " .. line_highlights(buf, delete_row))
  wait_for(function()
    local row = find_row(buf, "let stderr_laye = tracing_subscriber")
    return line_has_highlight(buf, row, "@keyword")
  end, "add hunk body row did not get Tree-sitter keyword highlight: " .. line_highlights(buf, add_row))
  wait_for(function()
    for _, value in pairs(diff_review._ts_context_cache or {}) do
      if type(value) == "table" and value.label == "Engine.new" then return true end
    end
    return false
  end, "Tree-sitter context was not cached\n" .. buffer_dump(buf))
  wait_for(function() return buffer_contains(buf, "pub fn new(bridge: Bridge) -> Self {") end, "opening boundary did not render\n" .. buffer_dump(buf))
  local boundary_row = find_row(buf, "pub fn new(bridge: Bridge) -> Self {")
  local boundary_line = vim.api.nvim_buf_get_lines(buf, boundary_row - 1, boundary_row, false)[1]
  assert_true(boundary_line:find("^%s*10%s+10%s+pub fn new", 1) ~= nil, "boundary line did not use diff gutter: " .. boundary_line)
  assert_true(line_has_highlight(buf, boundary_row, "@keyword"), "boundary row did not get keyword syntax highlight: " .. line_highlights(buf, boundary_row))
  assert_true(line_has_highlight(buf, boundary_row, "@type"), "boundary row did not get type syntax highlight: " .. line_highlights(buf, boundary_row))
  wait_for(function() return buffer_contains(buf, "...") end, "ellipsis boundary did not render\n" .. buffer_dump(buf))
  local ellipsis_line = vim.api.nvim_buf_get_lines(buf, find_row(buf, "...") - 1, find_row(buf, "..."), false)[1]
  assert_true(ellipsis_line:find("^%.%.%.") == nil, "ellipsis rendered at column zero: " .. ellipsis_line)
  wait_for(function() return buffer_contains(buf, "}") end, "closing boundary did not render\n" .. buffer_dump(buf))
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    return count_lines(lines, "pub fn new(bridge: Bridge) -> Self {") == 1
      and count_lines(lines, "  }") == 1
  end, "neighboring same-scope hunks repeated boundary context\n" .. buffer_dump(buf))
  local closing_line = vim.api.nvim_buf_get_lines(buf, find_row(buf, "  }") - 1, find_row(buf, "  }"), false)[1]
  assert_true(closing_line:find("^%s*13%s+13%s+}", 1) ~= nil, "closing boundary did not use diff gutter: " .. closing_line)
  wait_for(function() return buffer_contains(buf, "@@ Engine.new +1 -1") end, "compact hunk header did not get context label\n" .. buffer_dump(buf))

  local before = system_call_count()
  trigger_normal_mapping("S", boundary_row)
  vim.wait(50)
  assert_true(system_call_count() == before, "boundary row triggered a stage action")

  trigger_normal_mapping("<Tab>", find_row(buf, "@@ Engine.new +1 -1"))
  local collapsed_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  assert_true(contains_line(collapsed_lines, "@@ Engine.new +1 -1"), "collapsed hunk header missing")
  assert_true(not contains_line(collapsed_lines, "  pub fn new(bridge: Bridge) -> Self {"), "collapsed hunk showed opening boundary")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
vim.fn.delete(root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
