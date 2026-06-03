vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local original_notify = vim.notify

local root = "D:/diffreview-flow-root"
local calls = {}
local deletes = {}
local state = {}
local held_systemlist_async = nil

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(kind, command, input)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

local function reset_calls()
  calls = {}
  deletes = {}
end

local function reset_state(next_state)
  state = {
    modified = next_state.modified or {},
    staged_modified = next_state.staged_modified or {},
    untracked = next_state.untracked or {},
    staged_added = next_state.staged_added or {},
    ignored = next_state.ignored or {},
  }
  reset_calls()
end

local function sorted_keys(map)
  local keys = {}
  for key in pairs(map) do
    keys[#keys + 1] = key
  end
  table.sort(keys)
  return keys
end

local function modified_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1 +1 @@",
    "-old",
    "+new",
  }, "\n")
end

local function added_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "index 0000000..3333333",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1 @@",
    "+new",
  }, "\n")
end

local function joined_diff(files, builder)
  local diffs = {}
  for _, relpath in ipairs(sorted_keys(files)) do
    diffs[#diffs + 1] = builder(relpath)
  end
  return table.concat(diffs, "\n")
end

local function name_status(files, status)
  local lines = {}
  for _, relpath in ipairs(sorted_keys(files)) do
    lines[#lines + 1] = status .. "\t" .. relpath
  end
  return lines
end

local function input_relpath(input)
  return tostring(input or ""):match("diff %-%-git a/([^%s]+) b/")
end

local function output_lines(text)
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)

  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { "abc1234" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { "status flow test" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return sorted_keys(state.untracked), 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return name_status(state.modified, "M"), 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    local lines = name_status(state.staged_modified, "M")
    vim.list_extend(lines, name_status(state.staged_added, "A"))
    table.sort(lines)
    return lines, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    return output_lines(joined_diff(state.modified, modified_diff)), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then
    local text = joined_diff(state.staged_modified, modified_diff)
    local added = joined_diff(state.staged_added, added_diff)
    if text ~= "" and added ~= "" then text = text .. "\n" .. added else text = text .. added end
    return output_lines(text), 0
  end

  return {}, 1
end

function backend.systemlist_async(command, cb)
  record("systemlist_async", command)
  if held_systemlist_async then
    held_systemlist_async[#held_systemlist_async + 1] = function()
      local output, code = backend.systemlist(command)
      cb(output, code)
    end
    return
  end
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system(command, input)
  record("system", command, input)
  local key = command_key(command)
  local relpath = command[#command]

  if key:find("\tadd\t-u\t--\t", 1, true) then
    if state.modified[relpath] then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
      return "", 0
    end
    return "add -u failed for " .. relpath, 1
  end

  if key:find("\tadd\t--\t", 1, true) then
    if state.ignored[relpath] then
      return "The following paths are ignored by one of your .gitignore files: " .. relpath, 1
    end
    if state.untracked[relpath] then
      state.untracked[relpath] = nil
      state.staged_added[relpath] = true
      return "", 0
    end
    if state.modified[relpath] then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
      return "", 0
    end
    return "add failed for " .. relpath, 1
  end

  if key:find("\trestore\t--staged\t--\t", 1, true) then
    if state.staged_modified[relpath] then
      state.staged_modified[relpath] = nil
      state.modified[relpath] = true
      return "", 0
    end
    if state.staged_added[relpath] then
      return "error: pathspec '" .. relpath .. "' did not match any file(s) known to git", 1
    end
    return "restore --staged failed for " .. relpath, 1
  end

  if key:find("\trm\t--cached\t--ignore-unmatch\t--\t", 1, true) then
    if state.staged_added[relpath] then
      state.staged_added[relpath] = nil
      state.untracked[relpath] = true
    end
    return "", 0
  end

  if key:find("\tcheckout\tHEAD\t--\t", 1, true) then
    if state.modified[relpath] then
      state.modified[relpath] = nil
      return "", 0
    end
    if state.staged_modified[relpath] then
      state.staged_modified[relpath] = nil
      return "", 0
    end
    return "checkout failed for " .. relpath, 1
  end

  if key:find("\tapply\t", 1, true) then
    relpath = input_relpath(input)
    if not relpath then return "missing patch file header", 1 end
    local cached = key:find("\t--cached", 1, true) ~= nil
    local reverse = key:find("\t--reverse", 1, true) ~= nil
    local index = key:find("\t--index", 1, true) ~= nil
    if cached and reverse then
      state.staged_modified[relpath] = nil
      state.modified[relpath] = true
    elseif cached then
      state.modified[relpath] = nil
      state.staged_modified[relpath] = true
    elseif reverse and index then
      state.staged_modified[relpath] = nil
    elseif reverse then
      state.modified[relpath] = nil
    end
    return "", 0
  end

  return "unexpected command: " .. key, 1
end

function backend.system_async(command, input, cb)
  record("system_async", command, input)
  vim.defer_fn(function()
    local output, code = backend.system(command, input)
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 10)
end

function backend.delete(path)
  deletes[#deletes + 1] = path
  local relpath = path:gsub("\\", "/"):gsub("^" .. vim.pesc(root) .. "/", "")
  state.untracked[relpath] = nil
  return 0
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function count_lines_containing(buf, needle)
  local count = 0
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then count = count + 1 end
  end
  return count
end

local function find_row(buf, needle)
  for index, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row " .. needle .. "\n" .. table.concat(status_lines(buf), "\n"), 2)
end

local function find_row_after(buf, needle, start_row)
  local lines = status_lines(buf)
  for index = start_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. start_row .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function find_hunk_row_after_file(buf, file)
  local lines = status_lines(buf)
  local file_row = find_row(buf, file)
  for index = file_row + 1, #lines do
    if lines[index]:find("@@", 1, true) then return index end
    if lines[index]:find("%.txt ", 1) or lines[index]:find("%.rs ", 1) then break end
  end
  error("missing hunk row after " .. file .. "\n" .. table.concat(lines, "\n"), 2)
end

local function cursor_is_on_hunk_after_file(buf, file)
  local ok_hunk, hunk_row = pcall(find_hunk_row_after_file, buf, file)
  if not ok_hunk then return false end
  return vim.api.nvim_win_get_cursor(0)[1] == hunk_row
end

local function cursor_line_text(buf)
  return status_lines(buf)[vim.api.nvim_win_get_cursor(0)[1]] or ""
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function trigger_visual_mapping(key, first_row, second_row)
  vim.api.nvim_win_set_cursor(0, { second_row, 0 })
  vim.fn.setpos("'<", { 0, first_row, 1, 0 })
  vim.fn.setpos("'>", { 0, second_row, 1, 0 })
  local mapping = vim.fn.maparg(key, "x", false, true)
  assert_true(type(mapping.callback) == "function", "missing visual mapping for " .. key)
  mapping.callback()
end

local function confirm_yes()
  local mapping = vim.fn.maparg("y", "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing confirm yes mapping")
  mapping.callback()
end

local function saw_system_call(expected_key)
  for _, call in ipairs(calls) do
    if call.kind == "system" and call.key == expected_key then return true end
  end
  return false
end

local function saw_system_call_containing(needle)
  for _, call in ipairs(calls) do
    if call.kind == "system" and call.key:find(needle, 1, true) then return true end
  end
  return false
end

local function count_calls(kind, needle)
  local count = 0
  for _, call in ipairs(calls) do
    if call.kind == kind and (not needle or call.key:find(needle, 1, true)) then
      count = count + 1
    end
  end
  return count
end

local function calls_text()
  local lines = {}
  for _, call in ipairs(calls) do
    lines[#lines + 1] = ("%s: %s"):format(call.kind, call.key)
  end
  return table.concat(lines, "\n")
end

local function hold_systemlist_async()
  held_systemlist_async = {}
end

local function release_systemlist_async()
  local callbacks = held_systemlist_async or {}
  held_systemlist_async = nil
  for _, callback in ipairs(callbacks) do
    vim.defer_fn(callback, 0)
  end
end

local function render_and_wait(buf, needle)
  diff_review.render_status(buf)
  wait_for(function() return buffer_contains(buf, needle) end, "status did not render " .. needle)
end

local function assert_path_helpers()
  local relpath, err = diff_review._repo_relative_for_test("D:\\Repo\\App\\src\\main.rs", "d:/repo/app", true)
  assert_true(relpath == "src/main.rs", "windows drive/backslash path failed: " .. tostring(relpath or err))

  relpath, err = diff_review._repo_relative_for_test("D:/Repo/App/src/main.rs", "D:/Repo/App", true)
  assert_true(relpath == "src/main.rs", "windows slash path failed: " .. tostring(relpath or err))

  relpath, err = diff_review._repo_relative_for_test("/home/matt/project/src/lib.lua", "/home/matt/project", false)
  assert_true(relpath == "src/lib.lua", "linux path failed: " .. tostring(relpath or err))

  relpath, err = diff_review._repo_relative_for_test("/Users/matt/Project/src/init.lua", "/Users/matt/Project", false)
  assert_true(relpath == "src/init.lua", "macos path failed: " .. tostring(relpath or err))

  relpath = diff_review._repo_relative_for_test("/Users/matt/Project/src/init.lua", "/Users/matt/project", false)
  assert_true(relpath == nil, "case-sensitive unix-style path should reject mismatched root")
end

local function run()
  assert_path_helpers()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  vim.notify = function() end
  diff_review.setup()
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()

  reset_state({ modified = { ["mod.txt"] = true } })
  render_and_wait(buf, "mod.txt +1 -1")
  trigger_normal_mapping("S", find_row(buf, "mod.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tmod.txt")
  end, "tracked stage did not run git add -u")
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "tracked stage did not reconcile")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "mod.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tmod.txt")
  end, "tracked unstage did not run restore --staged")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (1)") end, "tracked unstage did not reconcile")

  reset_state({
    modified = {
      ["folded-stage-a.txt"] = true,
      ["folded-stage-b.txt"] = true,
      ["folded-stage-c.txt"] = true,
    },
  })
  render_and_wait(buf, "folded-stage-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "folded-stage-b.txt"))
  assert_true(
    cursor_line_text(buf):find("folded-stage-c.txt", 1, true) ~= nil,
    "folded file stage did not move cursor to the next file before reconcile\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return state.staged_modified["folded-stage-b.txt"] == true
  end, "folded file stage did not finish")
  wait_for(function()
    return cursor_line_text(buf):find("folded-stage-c.txt", 1, true) ~= nil
  end, "folded file stage did not keep cursor on next file after reconcile\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({
    modified = {
      ["visual-stage-a.txt"] = true,
      ["visual-stage-b.txt"] = true,
      ["visual-stage-c.txt"] = true,
      ["visual-stage-d.txt"] = true,
    },
  })
  render_and_wait(buf, "visual-stage-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "visual-stage-d.txt"))
  wait_for(function()
    return cursor_line_text(buf):find("visual-stage-c.txt", 1, true) ~= nil
  end, "single file stage did not establish a next-file target")
  local visual_first_row = find_row(buf, "visual-stage-a.txt")
  local visual_second_row = find_row(buf, "visual-stage-b.txt")
  trigger_visual_mapping("S", visual_first_row, visual_second_row)
  assert_true(
    vim.api.nvim_win_get_cursor(0)[1] == visual_second_row,
    "visual stage moved cursor row during optimistic render\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return state.staged_modified["visual-stage-a.txt"]
      and state.staged_modified["visual-stage-b.txt"]
      and state.staged_modified["visual-stage-d.txt"]
  end, "visual stage queue did not finish")
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "visual stage queue did not reconcile")
  assert_true(
    vim.api.nvim_win_get_cursor(0)[1] == visual_second_row,
    "visual stage inherited the previous action target after reconcile\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ modified = { ["hunk-stage.txt"] = true } })
  render_and_wait(buf, "hunk-stage.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "hunk-stage.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "hunk row did not render")
  trigger_normal_mapping("S", find_row(buf, "@@ +1 -1"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--whitespace=nowarn\t-")
  end, "hunk stage did not run cached apply")
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "hunk stage did not reconcile")
  reset_calls()
  trigger_normal_mapping("<Tab>", find_row(buf, "hunk-stage.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "staged hunk row did not render")
  trigger_normal_mapping("U", find_row(buf, "@@ +1 -1"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--reverse\t--whitespace=nowarn\t-")
  end, "hunk unstage did not run reverse cached apply")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (1)") end, "hunk unstage did not reconcile")

  reset_state({ modified = { ["cursor-stage-a.txt"] = true, ["cursor-stage-b.txt"] = true } })
  render_and_wait(buf, "cursor-stage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-stage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-stage-b.txt"))
  reset_calls()
  trigger_normal_mapping("S", find_row_after(buf, "@@ +1 -1", find_row(buf, "cursor-stage-a.txt")))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--whitespace=nowarn\t-")
  end, "cursor hunk stage did not run cached apply")
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "cursor-stage-b.txt")
  end, "cursor did not move to next hunk after staging\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["rapid-stage-a.txt"] = true, ["rapid-stage-b.txt"] = true } })
  render_and_wait(buf, "rapid-stage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-stage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "rapid-stage-b.txt"))
  reset_calls()
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-stage-a.txt"))
  wait_for(function()
    return state.staged_modified["rapid-stage-a.txt"] == true
  end, "first rapid hunk stage did not finish")
  vim.wait(20)
  assert_true(
    count_calls("systemlist_async", "\tdiff") == 0,
    "rapid first hunk stage reconciled before debounce\n" .. table.concat(status_lines(buf), "\n")
  )
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "rapid-stage-b.txt"))
  wait_for(function()
    return state.staged_modified["rapid-stage-b.txt"] == true
  end, "second rapid hunk stage did not finish")
  vim.wait(20)
  assert_true(
    count_calls("systemlist_async", "\tdiff") == 0,
    "rapid queued hunk stages reconciled before debounce\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "rapid hunk stages did not reconcile after debounce")

  local pending_context_callbacks = {}
  local original_compute_hunk_context_async = diff_review.compute_hunk_context_async
  diff_review.compute_hunk_context_async = function(filename, line, cb)
    pending_context_callbacks[#pending_context_callbacks + 1] = {
      filename = filename,
      line = line,
      cb = cb,
    }
  end
  diff_review._ts_context_cache = {}
  reset_state({ modified = { ["context-stage-a.txt"] = true, ["context-stage-b.txt"] = true } })
  render_and_wait(buf, "context-stage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "context-stage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "context-stage-b.txt"))
  wait_for(function() return #pending_context_callbacks > 0 end, "status render did not request hunk context")
  reset_calls()
  trigger_normal_mapping("S", find_hunk_row_after_file(buf, "context-stage-a.txt"))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--whitespace=nowarn\t-")
  end, "context cursor hunk stage did not run cached apply")
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "context-stage-b.txt")
  end, "cursor did not move to next hunk before delayed context callback\n" .. table.concat(status_lines(buf), "\n"))
  for _, request in ipairs(pending_context_callbacks) do
    if request.filename:find("context%-stage%-a%.txt") then
      request.cb("DelayedContext")
    end
  end
  vim.wait(50)
  assert_true(
    cursor_is_on_hunk_after_file(buf, "context-stage-b.txt"),
    "delayed hunk context rerender stole cursor\n" .. table.concat(status_lines(buf), "\n")
  )
  diff_review.compute_hunk_context_async = original_compute_hunk_context_async
  diff_review._ts_context_cache = {}

  reset_state({ modified = { ["refresh-cursor-a.txt"] = true, ["refresh-cursor-b.txt"] = true } })
  render_and_wait(buf, "refresh-cursor-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "refresh-cursor-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "refresh-cursor-b.txt"))
  vim.api.nvim_win_set_cursor(0, { find_hunk_row_after_file(buf, "refresh-cursor-b.txt"), 0 })
  diff_review.render_status(buf)
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "refresh-cursor-b.txt")
  end, "untargeted status refresh did not preserve cursor\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["late-cursor-a.txt"] = true, ["late-cursor-b.txt"] = true } })
  render_and_wait(buf, "late-cursor-a.txt +1 -1")
  local late_cursor_a_row = find_row(buf, "late-cursor-a.txt")
  local late_cursor_b_row = find_row(buf, "late-cursor-b.txt")
  vim.api.nvim_win_set_cursor(0, { late_cursor_a_row, 0 })
  hold_systemlist_async()
  diff_review.render_status(buf)
  wait_for(function()
    return held_systemlist_async and #held_systemlist_async > 0
  end, "status refresh did not start held async git calls")
  vim.api.nvim_win_set_cursor(0, { late_cursor_b_row, 0 })
  release_systemlist_async()
  wait_for(function()
    return cursor_line_text(buf):find("late-cursor-b.txt", 1, true) ~= nil
  end, "late async status refresh restored the old cursor instead of the latest cursor\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ staged_modified = { ["cursor-unstage-a.txt"] = true, ["cursor-unstage-b.txt"] = true } })
  render_and_wait(buf, "cursor-unstage-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-unstage-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-unstage-b.txt"))
  reset_calls()
  trigger_normal_mapping("U", find_row_after(buf, "@@ +1 -1", find_row(buf, "cursor-unstage-a.txt")))
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--reverse\t--whitespace=nowarn\t-")
  end, "cursor hunk unstage did not run reverse cached apply")
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "cursor-unstage-b.txt")
  end, "cursor did not move to next hunk after unstaging\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["header-stage-a.txt"] = true, ["header-stage-b.txt"] = true } })
  render_and_wait(buf, "header-stage-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "Unstaged changes (2)"))
  assert_true(
    cursor_line_text(buf):find("Staged changes (2)", 1, true) ~= nil,
    "stage-all from section header did not move cursor to destination header\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return state.staged_modified["header-stage-a.txt"] and state.staged_modified["header-stage-b.txt"]
  end, "stage-all from section header did not finish")
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "stage-all from section header did not reconcile")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "stage-all from section header jumped to a hunk after reconcile\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ modified = { ["codex/config.toml"] = true }, ignored = { ["codex/config.toml"] = true } })
  render_and_wait(buf, "codex")
  reset_calls()
  trigger_normal_mapping("S", find_row(buf, "codex"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tcodex/config.toml")
  end, "ignored tracked file did not stage with git add -u")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tadd\t--\tcodex/config.toml"),
    "ignored tracked file used plain git add\n" .. calls_text()
  )
  wait_for(function()
    return state.staged_modified["codex/config.toml"] == true
  end, "ignored tracked file did not stage")

  reset_state({ staged_modified = { ["header-unstage-a.txt"] = true, ["header-unstage-b.txt"] = true } })
  render_and_wait(buf, "header-unstage-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "Staged changes (2)"))
  assert_true(
    cursor_line_text(buf):find("Unstaged changes (2)", 1, true) ~= nil,
    "unstage-all from section header did not keep cursor on the header\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(vim.wait(3000, function()
    return state.modified["header-unstage-a.txt"] and state.modified["header-unstage-b.txt"]
  end, 10), "unstage-all from section header did not finish\ncalls:\n" .. calls_text())
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "unstage-all from section header did not reconcile")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "unstage-all from section header jumped to a hunk after reconcile\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({
    modified = { ["header-section-existing.txt"] = true },
    staged_modified = { ["header-section-a.txt"] = true, ["header-section-b.txt"] = true },
  })
  render_and_wait(buf, "header-section-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "Staged changes (2)"))
  assert_true(
    cursor_line_text(buf):find("Unstaged changes (3)", 1, true) ~= nil,
    "unstage-all from section header with existing destination did not target destination header\n"
      .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return state.modified["header-section-a.txt"] and state.modified["header-section-b.txt"]
  end, "unstage-all from section header with existing destination did not finish")
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "unstage-all from section header with existing destination did not reconcile")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "unstage-all from section header with existing destination jumped to a hunk after reconcile\n"
      .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ staged_modified = { ["header-file-unstage.txt"] = true } })
  render_and_wait(buf, "header-file-unstage.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "header-file-unstage.txt +1 -1"))
  assert_true(
    cursor_line_text(buf):find("header-file-unstage.txt +1 -1", 1, true) ~= nil,
    "unstage from file header did not keep cursor on the file row\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return state.modified["header-file-unstage.txt"] == true
  end, "unstage from file header did not finish")
  wait_for(function()
    return count_calls("systemlist_async", "\tdiff") > 0
  end, "unstage from file header did not reconcile")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "unstage from file header jumped to a hunk after reconcile\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ modified = { ["merge-file.txt"] = true }, staged_modified = { ["merge-file.txt"] = true } })
  render_and_wait(buf, "merge-file.txt +1 -1")
  trigger_normal_mapping("U", find_row_after(buf, "merge-file.txt", find_row(buf, "merge-file.txt")))
  assert_true(
    cursor_line_text(buf):find("merge-file.txt", 1, true) ~= nil and cursor_line_text(buf):find("@@", 1, true) == nil,
    "unstage from staged file header merged into existing file but cursor landed on a hunk\n"
      .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    count_lines_containing(buf, "merge-file.txt") == 1,
    "optimistic file unstage rendered duplicate file headings\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tmerge-file.txt")
  end, "merge file unstage did not run restore --staged")

  reset_state({ modified = { ["merge-hunk.txt"] = true }, staged_modified = { ["merge-hunk.txt"] = true } })
  render_and_wait(buf, "merge-hunk.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "merge-hunk.txt"))
  local staged_merge_hunk_file_row = find_row_after(buf, "merge-hunk.txt", find_row(buf, "merge-hunk.txt"))
  trigger_normal_mapping("<Tab>", staged_merge_hunk_file_row)
  reset_calls()
  trigger_normal_mapping("U", find_row_after(buf, "@@ +1 -1", staged_merge_hunk_file_row))
  assert_true(
    count_lines_containing(buf, "merge-hunk.txt") == 1,
    "optimistic hunk unstage rendered duplicate file headings\n" .. table.concat(status_lines(buf), "\n")
  )
  assert_true(
    count_lines_containing(buf, "@@ +1 -1") == 1,
    "optimistic hunk unstage rendered duplicate identical hunks\n" .. table.concat(status_lines(buf), "\n")
  )
  wait_for(function()
    return saw_system_call_containing("\tapply\t--cached\t--reverse\t--whitespace=nowarn\t-")
  end, "merge hunk unstage did not run reverse cached apply")

  reset_state({ untracked = { ["new.txt"] = true } })
  render_and_wait(buf, "new.txt new")
  trigger_normal_mapping("S", find_row(buf, "new.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tadd\t--\tnew.txt")
  end, "untracked stage did not use plain git add")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\tadd\t-u\t--\tnew.txt"),
    "untracked stage used git add -u"
  )
  wait_for(function() return buffer_contains(buf, "Staged changes (1)") end, "untracked optimistic stage did not render")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "new.txt"))
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\trm\t--cached\t--ignore-unmatch\t--\tnew.txt")
  end, "staged addition unstage did not run rm --cached")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tnew.txt"),
    "staged addition unstage used restore --staged"
  )
  wait_for(function() return buffer_contains(buf, "Untracked files (1)") end, "staged addition did not return to untracked")

  reset_state({ untracked = { ["ignore-u.txt"] = true } })
  render_and_wait(buf, "ignore-u.txt new")
  reset_calls()
  trigger_normal_mapping("U", find_row(buf, "ignore-u.txt"))
  vim.wait(50)
  assert_true(#calls == 0, "U on untracked file should not run git")

  reset_state({ untracked = { ["delete-untracked.txt"] = true } })
  render_and_wait(buf, "delete-untracked.txt new")
  trigger_normal_mapping("j", find_row(buf, "delete-untracked.txt"))
  confirm_yes()
  wait_for(function() return #deletes == 1 end, "discard untracked did not delete the file")
  wait_for(function() return not buffer_contains(buf, "delete-untracked.txt") end, "discard untracked did not refresh")

  reset_state({ modified = { ["discard-modified.txt"] = true } })
  render_and_wait(buf, "discard-modified.txt +1 -1")
  trigger_normal_mapping("j", find_row(buf, "discard-modified.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-modified.txt")
  end, "discard tracked file did not run checkout")
  wait_for(function() return not buffer_contains(buf, "discard-modified.txt") end, "discard tracked file did not refresh")

  reset_state({ modified = { ["discard-hunk.txt"] = true } })
  render_and_wait(buf, "discard-hunk.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "discard-hunk.txt"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "discard hunk row did not render")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "@@ +1 -1"))
  confirm_yes()
  wait_for(function()
    return saw_system_call_containing("\tapply\t--reverse\t--whitespace=nowarn\t-")
  end, "discard hunk did not run reverse apply")
  wait_for(function() return not buffer_contains(buf, "discard-hunk.txt") end, "discard hunk did not refresh")

  reset_state({ modified = { ["cursor-discard-a.txt"] = true, ["cursor-discard-b.txt"] = true } })
  render_and_wait(buf, "cursor-discard-a.txt +1 -1")
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-discard-a.txt"))
  trigger_normal_mapping("<Tab>", find_row(buf, "cursor-discard-b.txt"))
  reset_calls()
  trigger_normal_mapping("j", find_row_after(buf, "@@ +1 -1", find_row(buf, "cursor-discard-a.txt")))
  confirm_yes()
  wait_for(function()
    return saw_system_call_containing("\tapply\t--reverse\t--whitespace=nowarn\t-")
  end, "cursor hunk discard did not run reverse apply")
  wait_for(function()
    return cursor_is_on_hunk_after_file(buf, "cursor-discard-b.txt")
  end, "cursor did not move to next hunk after discarding\n" .. table.concat(status_lines(buf), "\n"))

  reset_state({ modified = { ["discard-header-a.txt"] = true, ["discard-header-b.txt"] = true } })
  render_and_wait(buf, "discard-header-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "Unstaged changes (2)"))
  confirm_yes()
  wait_for(function()
    return not buffer_contains(buf, "discard-header-a.txt") and not buffer_contains(buf, "discard-header-b.txt")
  end, "discard from section header did not remove files")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "discard from section header jumped to a hunk after refresh\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ modified = { ["discard-file-header-a.txt"] = true, ["discard-file-header-b.txt"] = true } })
  render_and_wait(buf, "discard-file-header-a.txt +1 -1")
  reset_calls()
  trigger_normal_mapping("j", find_row(buf, "discard-file-header-a.txt +1 -1"))
  confirm_yes()
  wait_for(function()
    return not buffer_contains(buf, "discard-file-header-a.txt") and buffer_contains(buf, "discard-file-header-b.txt")
  end, "discard from file header did not remove only that file")
  assert_true(
    cursor_line_text(buf):find("@@", 1, true) == nil,
    "discard from file header jumped to a hunk after refresh\n" .. table.concat(status_lines(buf), "\n")
  )

  reset_state({ staged_added = { ["discard-added.txt"] = true } })
  render_and_wait(buf, "discard-added.txt +1 -0")
  trigger_normal_mapping("j", find_row(buf, "discard-added.txt"))
  confirm_yes()
  wait_for(function()
    return saw_system_call("git\t-C\t" .. root .. "\tcheckout\tHEAD\t--\tdiscard-added.txt")
      and saw_system_call("git\t-C\t" .. root .. "\trm\t--cached\t--ignore-unmatch\t--\tdiscard-added.txt")
      and #deletes == 1
  end, "discard staged addition did not unstage with rm --cached and delete")
  assert_true(
    not saw_system_call("git\t-C\t" .. root .. "\trestore\t--staged\t--\tdiscard-added.txt"),
    "discard staged addition used restore --staged"
  )
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
