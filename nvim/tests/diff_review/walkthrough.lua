vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local walkthrough = require("diff_review.walkthrough")

local root = "D:/diffreview-flow-root"
local head_sha = string.rep("a", 40)

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local original_notify = vim.notify
local captured_notifications = {}
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = {
    message = tostring(message),
    level = level,
    opts = opts,
  }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

local function modified_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1,3 +1,3 @@",
    " alpha " .. relpath,
    "-old " .. relpath,
    "+NEW " .. relpath,
    " omega " .. relpath,
  }, "\n")
end

--- Same shape with a second hunk further down (new-file lines 10-12).
local function two_hunk_diff(relpath)
  return table.concat({
    modified_diff(relpath),
    "@@ -10,3 +10,3 @@",
    " alpha2 " .. relpath,
    "-old2 " .. relpath,
    "+NEW2 " .. relpath,
    " omega2 " .. relpath,
  }, "\n")
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { "abc1234" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\tHEAD" then
    return { head_sha }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { "walkthrough test" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return { "M\ta.txt", "M\tb.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return { "M\tc.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    return vim.split(two_hunk_diff("a.txt") .. "\n" .. modified_diff("b.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then
    return vim.split(modified_diff("c.txt"), "\n", { plain = true }), 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 3)
end

function backend.system(command)
  return "unexpected command: " .. command_key(command), 1
end

function backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = backend.system(command)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 3)
end

function backend.delete()
  return 0
end

---@type table<string, string?>
local fixtures = {}

---@type DiffReviewWalkthroughReader
local function fixture_reader(path)
  return fixtures[path]
end

local function wait_for(condition, message)
  assert_true(vim.wait(2000, condition, 10), message)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function trigger_buf_mapping(buf, key)
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

--- Find a floating window whose buffer contains needle; returns win, buf.
local function find_float_with(needle)
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    local win_config = vim.api.nvim_win_get_config(win)
    if win_config.relative ~= "" then
      local float_buf = vim.api.nvim_win_get_buf(win)
      if buffer_contains(float_buf, needle) then
        return win, float_buf
      end
    end
  end
  return nil, nil
end

local function close_all_floats()
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_config(win).relative ~= "" then
      pcall(vim.api.nvim_win_close, win, true)
    end
  end
end

local function walkthrough_extmark_count(buf)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, {})
  return #marks
end

--- Concatenated text of the inline comment box (virt_lines extmarks).
local function box_text(buf)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  local parts = {}
  for _, mark in ipairs(marks) do
    for _, virt_line in ipairs(mark[4].virt_lines or {}) do
      for _, chunk in ipairs(virt_line) do
        parts[#parts + 1] = chunk[1]
      end
    end
  end
  return table.concat(parts, "\n")
end

local function box_contains(buf, needle)
  return box_text(buf):find(needle, 1, true) ~= nil
end

---@param doc table
local function set_walkthrough_doc(doc)
  fixtures[root .. "/.walkthrough.json"] = vim.json.encode(doc)
end

local function valid_doc()
  return {
    version = 2,
    overview = "Adds NEW lines to a.txt and b.txt as a walkthrough fixture. The structured parts drive both the summary graph and Part N.M comment labels.",
    root = "Update walkthrough fixture files.",
    commit = head_sha,
    parts = {
      {
        title = "Update a.txt through the first part.",
        groups = {
          {
            title = "Fixture edits",
            items = {
              {
                marker = "*",
                title = "a.txt rewrite",
                note = "rewrites the second line for the first fixture file",
                steps = {
                  {
                    title = "First change",
                    file = "a.txt",
                    start = { line = 2, col = 1 },
                    ["end"] = { line = 2, col = 9 },
                    comment = "The a.txt second line was rewritten to NEW.",
                  },
                },
              },
            },
          },
        },
      },
      {
        title = "Update b.txt through the second part.",
        groups = {
          {
            title = "Fixture edits",
            items = {
              {
                marker = "*",
                title = "b.txt rewrite",
                note = "repeats the rewrite so navigation crosses parts",
                steps = {
                  {
                    file = "b.txt",
                    start = { line = 2, col = 1 },
                    ["end"] = { line = 2, col = 9 },
                    comment = "Same rewrite applied for symmetry.",
                  },
                },
              },
            },
          },
        },
      },
    },
  }
end

local function open_status()
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  assert_true(vim.bo[buf].filetype == "GitStatus", "status buffer did not open")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2)") end, "status did not render")
  return buf
end

local function start_walkthrough(buf)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  local summary_buf
  wait_for(function()
    local _, float_buf = find_float_with("walkthrough fixture")
    summary_buf = float_buf
    return float_buf ~= nil
  end, "summary popup did not open")
  return summary_buf
end

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  walkthrough.set_reader(fixture_reader)
  diff_review.setup({ about_auto_generate = false })

  -- ── happy path: summary, exact step, region highlight, comment float ──────
  set_walkthrough_doc(valid_doc())
  local buf = open_status()
  local original_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)

  local summary_buf = start_walkthrough(buf)
  assert_true(not buffer_contains(summary_buf, "WARNING"), "fresh walkthrough should not warn")
  local diagram_line = "├── Update a.txt through the first part."
  for _, line in ipairs(vim.api.nvim_buf_get_lines(summary_buf, 0, -1, false)) do
    if line == diagram_line then
      diagram_line = nil
      break
    end
  end
  assert_true(diagram_line == nil, "summary diagram line was not preserved verbatim")
  assert_true(buffer_contains(summary_buf, "Major changes:"), "derived summary missing major changes heading")
  assert_true(buffer_contains(summary_buf, "Legend: + new, * modified, ~ removed/split, > ownership moved"), "derived summary missing legend")
  trigger_buf_mapping(summary_buf, "y")

  wait_for(function() return buffer_contains(buf, "NEW a.txt") end, "step 1 did not expand a.txt hunks")
  local step_row = find_row(buf, "NEW a.txt")
  local cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == step_row, ("cursor not on step row (expected %d, got %d)"):format(step_row, cursor_row))
  assert_true(walkthrough_extmark_count(buf) > 0, "region extmarks missing")
  assert_true(box_contains(buf, "rewritten to NEW"), "inline comment box missing")
  assert_true(box_contains(buf, "Part 1.1 - First change"), "inline box heading missing")
  assert_true(box_contains(buf, "Update a.txt through the first part."), "part context missing")
  assert_true(box_contains(buf, "Fixture edits / *a.txt rewrite"), "group/item context missing")
  assert_true(box_contains(buf, " a.txt "), "file basename missing from the box header")

  -- ── navigation: forward to step 2, back, back to summary, quit ────────────
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "symmetry") end, "step 2 box did not render")
  assert_true(box_contains(buf, "Part 2.1 - b.txt rewrite"), "step 2 part label missing")
  assert_true(box_contains(buf, " b.txt "), "step 2 basename missing from the box header")
  local step2_row = find_row(buf, "NEW b.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == step2_row, "cursor not on step 2 row")

  trigger_buf_mapping(buf, "z")
  wait_for(function() return box_contains(buf, "rewritten to NEW") end, "z did not return to step 1")

  trigger_buf_mapping(buf, "z")
  local resume_buf
  wait_for(function()
    local _, float_buf = find_float_with("walkthrough fixture")
    resume_buf = float_buf
    return float_buf ~= nil
  end, "z past step 1 did not reopen the summary")
  trigger_buf_mapping(resume_buf, "q")
  wait_for(function() return walkthrough_extmark_count(buf) == 0 end, "quit did not clear extmarks")
  local restored_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)
  assert_true(restored_q_desc == original_q_desc, "q mapping not restored: " .. tostring(restored_q_desc))
  local z_map = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("z", "n", false, true)
  end)
  assert_true(z_map.buffer ~= 1, "z should not remain buffer-mapped after quit")

  -- ── completion past the last step ──────────────────────────────────────────
  summary_buf = start_walkthrough(buf)
  trigger_buf_mapping(summary_buf, "y")
  wait_for(function() return box_contains(buf, "rewritten to NEW") end, "restart did not show step 1")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "symmetry") end, "restart did not reach step 2")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return saw_notification_containing("Walkthrough complete") end, "no completion notification")
  assert_true(walkthrough_extmark_count(buf) == 0, "completion did not clear extmarks")
  close_all_floats()

  -- ── document staleness warning ─────────────────────────────────────────────
  local stale_doc = valid_doc()
  stale_doc.commit = string.rep("b", 40)
  set_walkthrough_doc(stale_doc)
  summary_buf = start_walkthrough(buf)
  assert_true(buffer_contains(summary_buf, "WARNING"), "stale walkthrough should warn in the summary")
  trigger_buf_mapping(summary_buf, "q")
  close_all_floats()

  -- ── step staleness: nearest line + missing file ───────────────────────────
  local degraded = valid_doc()
  degraded.parts = {
    {
      title = "Resolve stale walkthrough targets.",
      groups = {
        {
          title = "Stale targets",
          items = {
            {
              marker = "*",
              title = "Stale line reference",
              note = "falls back to the nearest rendered line",
              steps = {
                {
                  file = "a.txt",
                  start = { line = 999, col = 1 },
                  ["end"] = { line = 999, col = 1 },
                  comment = "Stale line reference.",
                },
              },
            },
            {
              marker = "*",
              title = "Missing file reference",
              note = "surfaces a missing-file note instead of failing",
              steps = {
                {
                  file = "gone.txt",
                  start = { line = 1, col = 1 },
                  ["end"] = { line = 1, col = 1 },
                  comment = "File missing from the diff.",
                },
              },
            },
          },
        },
      },
    },
  }
  set_walkthrough_doc(degraded)
  summary_buf = start_walkthrough(buf)
  trigger_buf_mapping(summary_buf, "y")
  wait_for(function() return box_contains(buf, "position approximated") end, "nearest match note missing")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "not in current diff") end, "missing file note missing")
  trigger_buf_mapping(buf, "q")
  close_all_floats()

  -- ── partially staged file + region split across hunks ─────────────────────
  local staged_doc = valid_doc()
  staged_doc.parts = {
    {
      title = "Resolve staged and split walkthrough regions.",
      groups = {
        {
          title = "Fixture diff sections",
          items = {
            {
              marker = "*",
              title = "Staged region",
              note = "anchors a step that only appears in the staged section",
              steps = {
                {
                  title = "Staged region",
                  file = "c.txt",
                  start = { line = 2, col = 1 },
                  ["end"] = { line = 2, col = 5 },
                  comment = "Lives only in the staged section.",
                },
              },
            },
            {
              marker = "*",
              title = "Split region",
              note = "anchors at the first rendered row inside a split range",
              steps = {
                {
                  title = "Split region",
                  file = "a.txt",
                  start = { line = 5, col = 1 },
                  ["end"] = { line = 11, col = 5 },
                  comment = "Region starts between hunks and ends inside the second hunk.",
                },
              },
            },
          },
        },
      },
    },
  }
  set_walkthrough_doc(staged_doc)
  summary_buf = start_walkthrough(buf)
  trigger_buf_mapping(summary_buf, "y")
  wait_for(function() return box_contains(buf, "staged section") end, "staged step box did not render")
  local staged_row = find_row(buf, "NEW c.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == staged_row, ("cursor not on the staged hunk row (expected %d, got %d)"):format(staged_row, cursor_row))
  assert_true(not box_contains(buf, "stale"), "staged region must not be flagged stale")

  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "ends inside the second hunk") end, "split-region step box did not render")
  local second_hunk_row = find_row(buf, "alpha2 a.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(
    cursor_row == second_hunk_row,
    ("split region did not anchor at the first rendered row inside the region (expected %d, got %d)"):format(second_hunk_row, cursor_row)
  )
  assert_true(not box_contains(buf, "approximated"), "split region must not be flagged approximated")
  trigger_buf_mapping(buf, "q")
  close_all_floats()

  -- ── re-render while active re-applies decorations ──────────────────────────
  set_walkthrough_doc(valid_doc())
  summary_buf = start_walkthrough(buf)
  trigger_buf_mapping(summary_buf, "y")
  wait_for(function() return walkthrough_extmark_count(buf) > 0 end, "no extmarks before re-render")
  diff_review.render_status(buf, nil, nil, { reuse_sections = true })
  wait_for(function() return walkthrough_extmark_count(buf) > 0 end, "re-render dropped walkthrough extmarks")
  wait_for(function() return box_contains(buf, "rewritten to NEW") end, "re-render dropped the inline comment box")
  trigger_buf_mapping(buf, "q")
  close_all_floats()

  -- ── error paths: missing file and invalid JSON ─────────────────────────────
  fixtures[root .. "/.walkthrough.json"] = nil
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("No .walkthrough.json") end, "missing file notification absent")
  local z_after = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("z", "n", false, true)
  end)
  assert_true(z_after.buffer ~= 1, "missing walkthrough must not enter the mode")

  fixtures[root .. "/.walkthrough.json"] = "{ not json"
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("not valid JSON") end, "invalid JSON notification absent")

  fixtures[root .. "/.walkthrough.json"] = vim.json.encode({ version = 1, summary = "x", commit = "zz", steps = {} })
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("expected 2") end, "v1 rejection notification absent")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review.reset_git_backend()
gh.reset_backend()
walkthrough.reset_reader()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
