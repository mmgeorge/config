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

local function long_region_diff(relpath)
  local lines = {
    two_hunk_diff(relpath),
    "@@ -20,36 +20,36 @@",
    " long context 01 " .. relpath,
    "-old long 02 " .. relpath,
    "+NEW long 02 " .. relpath,
  }
  for index = 3, 36 do
    lines[#lines + 1] = (" long context %02d %s"):format(index, relpath)
  end
  return table.concat(lines, "\n")
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
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(long_region_diff("a.txt") .. "\n" .. modified_diff("b.txt"), "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
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

local function file_entry_matches(entry, suffix)
  local file = entry and entry.file or nil
  if not file then return false end
  suffix = suffix:gsub("\\", "/")
  for _, value in ipairs({ file.relpath, file.filename }) do
    local path = tostring(value or ""):gsub("\\", "/")
    if path == suffix or path:sub(-(#suffix + 1)) == "/" .. suffix then return true end
  end
  return false
end

local function buffer_has_highlight(buf, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  for _, mark in ipairs(marks) do
    if mark[4].hl_group == hl_group then return true end
  end
  return false
end

local function buffer_has_highlight_for_text(buf, line_needle, text, hl_group)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for row, line in ipairs(lines) do
    if line:find(line_needle, 1, true) then
      local start_byte = line:find(text, 1, true)
      if not start_byte then return false end
      local start_col = start_byte - 1
      local end_col = start_col + #text
      local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
      for _, mark in ipairs(marks) do
        local details = mark[4] or {}
        if mark[2] == row - 1
            and details.hl_group == hl_group
            and mark[3] <= start_col
            and (details.end_col or mark[3]) >= end_col then
          return true
        end
      end
      return false
    end
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

local function find_line(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  return lines[find_row(buf, needle)]
end

local function find_line_after(buf, needle, after_row)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for row_index = after_row + 1, #lines do
    if lines[row_index]:find(needle, 1, true) then return lines[row_index] end
  end
  error("missing row after " .. after_row .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function first_text_col(line)
  local first_text_byte = line:find("%S")
  return first_text_byte and (first_text_byte - 1) or 0
end

local function display_col_before(line, needle)
  local start_byte = line:find(needle, 1, true)
  assert_true(start_byte ~= nil, "missing text: " .. needle)
  return vim.fn.strdisplaywidth(line:sub(1, start_byte - 1))
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
  local count = 0
  for _, namespace in ipairs({ walkthrough._ns, walkthrough._active_ns }) do
    local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, 0, -1, {})
    count = count + #marks
  end
  return count
end

local function row_has_line_highlight(buf, row, hl_group)
  for _, namespace in ipairs({ walkthrough._ns, walkthrough._active_ns }) do
    local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, { row - 1, 0 }, { row - 1, -1 }, { details = true })
    for _, mark in ipairs(marks) do
      if (mark[4] or {}).line_hl_group == hl_group then return true end
    end
  end
  return false
end

local function first_row_with_line_highlight(buf, hl_group)
  for _, namespace in ipairs({ walkthrough._ns, walkthrough._active_ns }) do
    local marks = vim.api.nvim_buf_get_extmarks(buf, namespace, 0, -1, { details = true })
    for _, mark in ipairs(marks) do
      if (mark[4] or {}).line_hl_group == hl_group then return mark[2] + 1 end
    end
  end
  return nil
end

--- Concatenated text of the inline comment box (virt_lines extmarks).
local function box_text(buf)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  local chunks = {}
  for _, mark in ipairs(marks) do
    for _, virt_line in ipairs(mark[4].virt_lines or {}) do
      for _, chunk in ipairs(virt_line) do
        chunks[#chunks + 1] = chunk[1]
      end
    end
  end
  return table.concat(chunks, "\n")
end

local function comment_box_mark(buf)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  for _, mark in ipairs(marks) do
    if mark[4].virt_lines then return mark end
  end
  return nil
end

local function comment_box_mark_text(mark)
  local chunks = {}
  for _, virt_line in ipairs(mark and mark[4] and mark[4].virt_lines or {}) do
    for _, chunk in ipairs(virt_line) do
      chunks[#chunks + 1] = chunk[1]
    end
  end
  return table.concat(chunks, "\n")
end

local function comment_box_mark_containing(buf, needle)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  for _, mark in ipairs(marks) do
    if mark[4].virt_lines and comment_box_mark_text(mark):find(needle, 1, true) then return mark end
  end
  return nil
end

local function comment_box_inner_width(mark)
  local virt_lines = mark and mark[4] and mark[4].virt_lines or {}
  local bottom = virt_lines[#virt_lines]
  local width = 0
  for _, chunk in ipairs(bottom or {}) do
    width = width + vim.fn.strdisplaywidth(chunk[1])
  end
  return math.max(width - 4, 0)
end

local function comment_box_header_text(mark)
  local virt_lines = mark and mark[4] and mark[4].virt_lines or {}
  local chunks = {}
  for _, chunk in ipairs(virt_lines[1] or {}) do
    chunks[#chunks + 1] = chunk[1]
  end
  return table.concat(chunks, "")
end

local function box_contains(buf, needle)
  return box_text(buf):find(needle, 1, true) ~= nil
end

local function box_has_highlight_for_text(buf, text, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  for _, mark in ipairs(marks) do
    for _, virt_line in ipairs(mark[4].virt_lines or {}) do
      for _, chunk in ipairs(virt_line) do
        if chunk[1]:find(text, 1, true) and chunk[2] == hl_group then return true end
      end
    end
  end
  return false
end

local function segment_line_text(row)
  local chunks = {}
  for _, segment in ipairs(row.segments or {}) do
    chunks[#chunks + 1] = segment[1] or ""
  end
  return table.concat(chunks)
end

local function expected_comment_start_screen_row(line_count, win_height)
  local one_third_from_bottom = win_height - math.floor(win_height / 3)
  local latest_start_with_box_visible = math.max(1, win_height - line_count + 1)
  return math.max(1, math.min(one_third_from_bottom, latest_start_with_box_visible))
end

local function expected_comment_topline(start_row, anchor_row, line_count, win_height)
  local target_screen_row = expected_comment_start_screen_row(line_count, win_height)
  return math.max(1, math.min(start_row, anchor_row + 2 - target_screen_row))
end

local function expected_comment_screen_row(start_row, anchor_row, line_count, win_height)
  local topline = expected_comment_topline(start_row, anchor_row, line_count, win_height)
  return anchor_row - topline + 2
end

---@param doc table
local function set_walkthrough_doc(doc)
  fixtures[root .. "/.walkthrough.json"] = vim.json.encode(doc)
end

local function valid_doc()
  return {
    version = 7,
    overview = "Update walkthrough fixture files. Before, the fixture rows used the old text. Now, the structured tasks drive both the summary graph and Task N.M-total comment labels.",
    root = "Update walkthrough fixture files.",
    commit = head_sha,
    tasks = {
      {
        title = "Update a.txt through the first task.",
        justification = "Reviewers need the fixture story before individual file rewrites.",
        groups = {
          {
            type = "File",
            title = "Fixture edits",
            subtasks = {
              {
                title = "Rewrite the first fixture file.",
                justification = "The first fixture row carries the opening example for walkthrough rendering.",
                items = {
                  {
                    action = "Update",
                    type = "Struct",
                    subtype = "Resource",
                    title = "a.txt rewrite",
                    note = "rewrite the second line for the first fixture file",
                    steps = {
                      {
                        title = "Rewrite the fixture line to NEW.",
                        file = "a.txt",
                        start = { line = 2, col = 1 },
                        ["end"] = { line = 2, col = 9 },
                        comment = "The fixture row previously used OLD, so the walkthrough had no fresh changed target. Rewriting it to NEW gives the renderer a concrete changed line to anchor.",
                        callout = {
                          kind = "deviation",
                          text = "This fixture intentionally uses a deviation callout so the renderer proves high-priority review context is visible.",
                        },
                      },
                      {
                        title = "Check the total marker on the next comment.",
                        file = "a.txt",
                        start = { line = 2, col = 1 },
                        ["end"] = { line = 2, col = 9 },
                        comment = "The first task now has a second walkthrough comment, so the inline header can show both the step number and the task total.",
                      },
                    },
                  },
                },
              },
            },
          },
        },
      },
      {
        title = "Update b.txt through the second task.",
        groups = {
          {
            type = "File",
            title = "Fixture edits",
            subtasks = {
              {
                title = "Rewrite the second fixture file.",
                items = {
                  {
                    action = "Add",
                    type = "Function",
                    title = "b.txt rewrite",
                    note = "repeat the rewrite so navigation crosses tasks",
                    steps = {
                      {
                        title = "Mirror the rewrite in the second fixture.",
                        file = "b.txt",
                        start = { line = 2, col = 1 },
                        ["end"] = { line = 2, col = 9 },
                        comment = "The second fixture previously did not exercise cross-task navigation. Applying the same rewrite verifies that forward navigation reaches another task.",
                      },
                    },
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
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1 and vim.wo[win].wrap == false, "GitStatus window should not soft-wrap diff rows")
  assert_true(vim.wo[win].linebreak == false, "GitStatus window should not set linebreak for diff rows")
  assert_true(vim.wo[win].breakindent == false, "GitStatus window should not set breakindent for diff rows")
  return buf
end

local function start_walkthrough(buf)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function()
    return buffer_contains(buf, "Overview:")
  end, "walkthrough did not start")
  local _, float_buf = find_float_with("walkthrough fixture")
  assert_true(float_buf == nil, "walkthrough should not open a summary popup")
  return buf
end

local function walkthrough_file_row(buf, suffix, key_fragment)
  local status = diff_review._status_states and diff_review._status_states[buf] or diff_review._status
  local seen = {}
  for row, entry in pairs(status and status.entries or {}) do
    if entry.kind == "file" and file_entry_matches(entry, suffix) then
      local key = tostring(entry.file and entry.file.walkthrough_key_prefix or "")
      if not key_fragment or key:find(key_fragment, 1, true) then return row end
    end
    if entry.kind == "file" and entry.file then
      seen[#seen + 1] = ("%s|%s"):format(tostring(entry.file.relpath or entry.file.filename), tostring(entry.file.walkthrough_key_prefix))
    end
  end
  error(("missing walkthrough file row for %s (wanted %s; saw %s)"):format(
    suffix,
    tostring(key_fragment),
    table.concat(seen, ", ")
  ), 2)
end

local function walkthrough_hunk_row(buf, suffix, key_fragment)
  local status = diff_review._status_states and diff_review._status_states[buf] or diff_review._status
  local seen = {}
  for row, entry in pairs(status and status.entries or {}) do
    if entry.kind == "hunk" and file_entry_matches(entry, suffix) then
      local key = tostring(entry.file and entry.file.walkthrough_key_prefix or "")
      if not key_fragment or key:find(key_fragment, 1, true) then return row end
    end
    if entry.kind == "hunk" and entry.file then
      seen[#seen + 1] = ("%s|%s"):format(tostring(entry.file.relpath or entry.file.filename), tostring(entry.file.walkthrough_key_prefix))
    end
  end
  error(("missing walkthrough hunk row for %s (wanted %s; saw %s)"):format(
    suffix,
    tostring(key_fragment),
    table.concat(seen, ", ")
  ), 2)
end

local function set_cursor_row(buf, row)
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "status window missing")
  vim.api.nvim_win_set_cursor(win, { row, 0 })
end

local function ensure_walkthrough_file_expanded(buf, suffix, key_fragment, visible_text)
  if visible_text and buffer_contains(buf, visible_text) then return end
  set_cursor_row(buf, walkthrough_file_row(buf, suffix, key_fragment))
  trigger_buf_mapping(buf, "<Tab>")
  if visible_text then
    wait_for(function() return buffer_contains(buf, visible_text) end,
      "walkthrough file did not expand: " .. suffix)
  end
end

local function ensure_walkthrough_file_folded(buf, suffix, key_fragment, hidden_text)
  if hidden_text and not buffer_contains(buf, hidden_text) then return end
  set_cursor_row(buf, walkthrough_file_row(buf, suffix, key_fragment))
  trigger_buf_mapping(buf, "<Tab>")
  if hidden_text then
    wait_for(function() return not buffer_contains(buf, hidden_text) end,
      "walkthrough file did not fold: " .. suffix)
  end
end

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  walkthrough.set_reader(fixture_reader)
  diff_review.setup({ about_auto_generate = false })

  -- ── compact task presentation table ────────────────────────────────────────
  local compact_rows = walkthrough.task_presentation_rows({
    title = "Let particle spawns select their render path.",
    justification = "Spawn data needs to choose the visual path before render handoff.",
    groups = {
      {
        type = "Module",
        title = "PhysicsPlugin",
        subtasks = {
          {
            title = "Make particle render modes explicit at spawn time.",
            items = {
              {
                action = "Add",
                type = "Enum",
                title = "ParticleRenderMode",
                note = "select billboard or model rendering",
                steps = {},
              },
              {
                action = "Update",
                type = "Struct",
                title = "ParticleSpawn",
                note = "carry render mode with spawn data",
                steps = {},
              },
            },
          },
        },
      },
      {
        type = "Module",
        title = "ParticleDrop3d",
        subtasks = {
          {
            title = "Exercise the model-backed path in the 3D drop scene.",
            items = {
              {
                action = "Update",
                type = "Function",
                title = "ParticleDrop3d::setup()",
                note = "spawn SmoothSphere model particles",
                steps = {},
              },
            },
          },
        },
      },
    },
  })
  assert_true(segment_line_text(compact_rows[1]) == "", "compact table should start after a blank spacer")
  assert_true(segment_line_text(compact_rows[2])
      == "- Add    enum   ParticleRenderMode to select billboard or model rendering",
    "compact table first item row did not align action/type columns")
  assert_true(segment_line_text(compact_rows[3])
      == "- Modify struct ParticleSpawn to carry render mode with spawn data",
    "compact table continuation row should render as a bullet")
  assert_true(segment_line_text(compact_rows[4])
      == "- Modify fn     ParticleDrop3d::setup() to spawn SmoothSphere model particles",
    "compact table should flatten later group items into the same bullet list")
  local compact_text = table.concat({
    segment_line_text(compact_rows[1]),
    segment_line_text(compact_rows[2]),
    segment_line_text(compact_rows[3]),
    segment_line_text(compact_rows[4]),
  }, "\n")
  assert_true(not compact_text:find("└─", 1, true), "compact table should not render tree glyphs")
  assert_true(not compact_text:find("PhysicsPlugin", 1, true), "compact table should not render group headings")
  for _, row in ipairs(compact_rows) do
    assert_true(segment_line_text(row) ~= "ParticleDrop3d", "compact table should not render later group headings")
  end
  assert_true(compact_rows[2].segments[2][2] == "DiffReviewWalkthroughActionAdd",
    "compact table action should use action highlight")
  assert_true(compact_rows[2].segments[4][2] == "DiffReviewWalkthroughActionAdd",
    "compact table type should use action highlight")

  -- ── happy path: presentation, exact step, region highlight, comment box ───
  set_walkthrough_doc(valid_doc())
  local buf = open_status()
  local original_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)
  local task1_title =
    "1. Update a.txt through the first task. Reviewers need the fixture story before individual file rewrites."

  vim.o.columns = 80
  start_walkthrough(buf)
  wait_for(function() return buffer_contains(buf, "Overview:") end, "narrow walkthrough presentation overview missing")
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1 and vim.wo[win].wrap == false, "GitStatus walkthrough should keep diff rows unwrapped")
  assert_true(vim.wo[win].linebreak == false, "GitStatus walkthrough should not enable window linebreak")
  assert_true(vim.wo[win].breakindent == false, "GitStatus walkthrough should not enable window breakindent")
  local narrow_wininfo = vim.fn.getwininfo(win)[1]
  local narrow_width = math.max(20,
    vim.api.nvim_win_get_width(win) - (tonumber(narrow_wininfo and narrow_wininfo.textoff) or 0) - 1)
  local narrow_heading_lines = walkthrough.wrap_text(task1_title, narrow_width)
  assert_true(#narrow_heading_lines > 1, "walkthrough task heading should hard-wrap in narrow status window")
  for _, heading_line in ipairs(narrow_heading_lines) do
    assert_true(buffer_contains(buf, heading_line),
      ("walkthrough task heading wrapped line missing from buffer: %s"):format(heading_line))
  end
  assert_true(not buffer_contains(buf, task1_title), "narrow walkthrough should not keep long task heading on one row")
  trigger_buf_mapping(buf, "q")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2):") end,
    "narrow walkthrough quit should restore the flat GitStatus presentation")

  vim.o.columns = 120
  start_walkthrough(buf)
  wait_for(function() return buffer_contains(buf, "Overview:") end, "walkthrough presentation overview missing")
  win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1 and vim.wo[win].wrap == false, "GitStatus walkthrough should keep diff rows unwrapped")
  assert_true(vim.wo[win].linebreak == false, "GitStatus walkthrough should not enable window linebreak")
  assert_true(vim.wo[win].breakindent == false, "GitStatus walkthrough should not enable window breakindent")
  assert_true(buffer_contains(buf, task1_title), "walkthrough presentation task 1 missing")
  local overview_row = find_row(buf, "Overview:")
  local first_task_row = find_row(buf, "1. Update a.txt through the first task.")
  local overview_lines = vim.api.nvim_buf_get_lines(buf, overview_row, first_task_row - 1, false)
  local nonblank_overview_lines = 0
  local wininfo = vim.fn.getwininfo(win)[1]
  local overview_width = math.max(20, vim.api.nvim_win_get_width(win) - (tonumber(wininfo and wininfo.textoff) or 0) - 1)
  for _, line in ipairs(overview_lines) do
    if vim.trim(line) ~= "" then
      nonblank_overview_lines = nonblank_overview_lines + 1
      assert_true(vim.fn.strdisplaywidth(line) <= overview_width,
        ("walkthrough overview line exceeds status width %d: %s"):format(overview_width, line))
    end
  end
  assert_true(nonblank_overview_lines > 1, "walkthrough overview should wrap into multiple lines")
  assert_true(buffer_contains(buf, "2. Update b.txt through the second task."),
    "walkthrough presentation task 2 missing")
  assert_true(buffer_contains(buf, "Unstaged Changes (1):"),
    "walkthrough presentation should group matching unstaged files per task")
  assert_true(not buffer_contains(buf, "Unstaged changes (2):"),
    "walkthrough presentation should replace the flat unstaged section")
  local task_file_entry = nil
  local second_task_file_entry = nil
  for _, entry in pairs(diff_review._status.entries or {}) do
    if entry.kind == "file"
      and file_entry_matches(entry, "a.txt")
      and entry.file.walkthrough_action_hunks_only
      and tostring(entry.file.walkthrough_key_prefix or ""):find("walkthrough:task:1", 1, true) then
      task_file_entry = entry
    elseif entry.kind == "file"
      and file_entry_matches(entry, "b.txt")
      and entry.file.walkthrough_action_hunks_only
      and tostring(entry.file.walkthrough_key_prefix or ""):find("walkthrough:task:2", 1, true) then
      second_task_file_entry = entry
    end
  end
  assert_true(task_file_entry ~= nil, "walkthrough task file row should mark partial hunk actions")
  assert_true(second_task_file_entry ~= nil, "walkthrough task 2 file row missing")
  local action_entries = diff_review._status_action_entries_for_file(task_file_entry.file)
  assert_true(#action_entries == 1 and action_entries[1].kind == "hunk",
    "partial walkthrough file row should expand to hunk action entries")
  assert_true(not saw_notification_containing("Walkthrough generated against"), "fresh walkthrough should not warn")
  assert_true(not buffer_contains(buf, "Major changes:"),
    "walkthrough presentation should not show redundant major changes heading")
  for _, rendered_line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    assert_true(rendered_line ~= " Reviewers need the fixture story before individual file rewrites.",
      "walkthrough presentation should not render task justification on a separate row")
  end
  assert_true(not buffer_contains(buf, " file Fixture edits"), "walkthrough presentation should not show group type row")
  assert_true(not buffer_contains(buf, "    └─ Rewrite the first fixture file."),
    "walkthrough presentation should not show subtask tree rows")
  assert_true(not buffer_contains(buf, "Fixture edits"), "walkthrough presentation should not show group headings")
  assert_true(buffer_contains(buf, "- Modify Resource a.txt rewrite to rewrite"),
    "walkthrough presentation compact table row missing display verb")
  assert_true(buffer_contains(buf, "- Add fn b.txt rewrite to repeat"),
    "walkthrough presentation compact add row missing")
  assert_true(buffer_contains(buf, "the second line"),
    "walkthrough presentation item inline note prefix missing")
  local action_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughActionUpdate" })
  assert_true(action_hl.italic == true, "walkthrough action highlight should be italic")
  local type_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughType" })
  assert_true(type_hl.fg == 0x5bff94, "walkthrough group type highlight should use config green")
  local title_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughItemTitle" })
  assert_true(title_hl.bold == true and title_hl.fg == 0xffffff, "walkthrough item title should be bold white")
  local justification_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughJustification" })
  assert_true(justification_hl.fg == 0xe5c07b and justification_hl.italic == true,
    "walkthrough justification should be yellow italic")
  assert_true(not buffer_contains(buf, "Legend:"), "walkthrough presentation should not show an action legend")
  assert_true(comment_box_mark(buf) == nil, "folded walkthrough files should not render comment boxes")
  ensure_walkthrough_file_expanded(buf, "a.txt", "walkthrough:task:1:unstaged", "NEW a.txt")
  wait_for(function() return box_contains(buf, "task total") end,
    "expanding a walkthrough file should render its comment blocks")
  assert_true(not box_contains(buf, "forward navigation"),
    "folded b.txt should not render a comment block yet")
  local a_hunk_row = walkthrough_hunk_row(buf, "a.txt", "walkthrough:task:1:unstaged")
  set_cursor_row(buf, a_hunk_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return not buffer_contains(buf, "NEW a.txt") end,
    "folding a walkthrough hunk should hide its diff rows")
  wait_for(function() return not box_contains(buf, "concrete changed line") end,
    "folding a walkthrough hunk should clear its comment blocks")
  set_cursor_row(buf, a_hunk_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return buffer_contains(buf, "NEW a.txt") end,
    "re-expanding a walkthrough hunk should restore its diff rows")
  wait_for(function() return box_contains(buf, "concrete changed line") end,
    "re-expanding a walkthrough hunk should recreate its comment blocks")
  ensure_walkthrough_file_folded(buf, "a.txt", "walkthrough:task:1:unstaged", "NEW a.txt")
  wait_for(function() return not box_contains(buf, "concrete changed line") end,
    "folding a walkthrough file should clear its comment blocks")
  ensure_walkthrough_file_expanded(buf, "a.txt", "walkthrough:task:1:unstaged", "NEW a.txt")
  wait_for(function() return box_contains(buf, "concrete changed line") end,
    "re-expanding a walkthrough file should recreate its comment blocks")

  local step_row = find_row(buf, "NEW a.txt")
  wait_for(function()
    return vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1] == step_row
  end, "step 1 did not move to the changed a.txt row")
  local cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == step_row, ("cursor not on step row (expected %d, got %d)"):format(step_row, cursor_row))
  assert_true(walkthrough_extmark_count(buf) > 0, "comment box extmarks missing")
  assert_true(first_row_with_line_highlight(buf, "DiffReviewWalkthroughRegionAdd") == nil,
    "walkthrough should not highlight the selected diff region")
  assert_true(not row_has_line_highlight(buf, step_row, "DiffReviewWalkthroughRegion"),
    "added walkthrough row should not use generic blue region background")
  assert_true(box_contains(buf, "concrete changed line"), "inline comment box missing")
  assert_true(box_contains(buf, "Deviation:"), "inline comment box should render the callout kind")
  assert_true(box_contains(buf, "high-priority review context"), "inline comment box should render callout text")
  assert_true(box_has_highlight_for_text(buf, "Deviation:", "DiffReviewWalkthroughCalloutDeviation"),
    "inline callout kind should use deviation highlight")
  local deviation_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughCalloutDeviation" })
  assert_true(deviation_hl.fg == 0xff5555 and deviation_hl.bold == true,
    "deviation callout should be red and bold")
  local box_mark = comment_box_mark_containing(buf, "concrete changed line")
  assert_true(box_mark ~= nil, "inline comment box mark missing")
  assert_true(box_mark[2] == step_row - 1, "inline comment box should anchor below the selected row")
  assert_true(box_mark[4].virt_lines_above ~= true, "inline comment box should render below the selected row")
  local win = vim.fn.bufwinid(buf)
  local view = vim.api.nvim_win_call(win, function() return vim.fn.winsaveview() end)
  local expected_topline = expected_comment_topline(step_row, box_mark[2] + 1, #box_mark[4].virt_lines,
    vim.api.nvim_win_get_height(win))
  assert_true(view.topline == expected_topline,
    ("walkthrough view not focused near lower comment box (expected topline %d, got %d)"):format(
      expected_topline, view.topline))
  local expected_box_screen_row = expected_comment_screen_row(step_row, box_mark[2] + 1, #box_mark[4].virt_lines,
    vim.api.nvim_win_get_height(win))
  local box_screen_row = (box_mark[2] + 1) - view.topline + 2
  assert_true(box_screen_row == expected_box_screen_row,
    ("inline comment box should start one third from the bottom (expected screen row %d, got %d)"):format(
      expected_box_screen_row, box_screen_row))
  assert_true(box_contains(buf, "1.1-2 Rewrite the fixture line to NEW."), "inline box heading missing")
  assert_true(not box_contains(buf, "Task 1.1-2 Rewrite the fixture line to NEW."),
    "inline box heading should not include Task")
  assert_true(not box_contains(buf, "1.1 - Rewrite the fixture line to NEW."),
    "inline box heading should not use the old title separator")
  assert_true(box_has_highlight_for_text(buf, "1.1-2 Rewrite the fixture line to NEW.",
    "DiffReviewWalkthroughItemTitle"),
    "inline box heading should be bold white")
  assert_true(comment_box_inner_width(box_mark) == 84, "inline comment box should use the widened max width")
  local box_header = comment_box_header_text(box_mark)
  assert_true(box_header:find("Rewrite the fixture line to NEW.", 1, true) ~= nil,
    "inline box header should show only the step title")
  assert_true(box_header:find("Rewrite the first fixture file.", 1, true) == nil,
    "inline box header should not show subtask context")
  assert_true(box_header:find(" a.txt ", 1, true) == nil, "inline box header should not show file basename")
  assert_true(not box_contains(buf, "Update a.txt through the first task."), "inline box should not show task context")
  assert_true(not box_contains(buf, "└─ Rewrite the first fixture file."), "inline box should not show subtask graph")
  assert_true(not box_contains(buf, "Fixture edits / Rewrite the first fixture file."),
    "inline box should not show group/subtask breadcrumb")
  assert_true(not box_contains(buf, "Modify Resource a.txt rewrite"), "inline box should not show item context")
  assert_true(not box_contains(buf, "rewrite the second line for the first fixture file"),
    "inline box should not show item note")
  assert_true(not box_contains(buf, "[z] back"), "inline box should not show command footer")

  -- ── navigation: forward through steps, back, back to summary, quit ─────────
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "task total") end, "step 2 box did not render")
  assert_true(box_contains(buf, "1.2-2 Check the total marker on the next comment."),
    "step 2 task total label missing")
  assert_true(box_contains(buf, "Deviation:"), "step 1 callout should stay visible while on step 2")
  local task1_step2_row = find_row(buf, "NEW a.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == task1_step2_row, "cursor not on task 1 step 2 row")

  ensure_walkthrough_file_expanded(buf, "b.txt", "walkthrough:task:2:unstaged", "NEW b.txt")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "forward navigation") end, "step 3 box did not render")
  assert_true(box_contains(buf, "2.1-1 Mirror the rewrite in the second fixture."), "step 3 task label missing")
  assert_true(not box_contains(buf, "Task 2.1-1 Mirror the rewrite in the second fixture."),
    "step 2 task label should not include Task")
  local step3_box_mark = comment_box_mark_containing(buf, "forward navigation")
  assert_true(comment_box_header_text(step3_box_mark):find("Rewrite the second fixture file.", 1, true) == nil,
    "step 3 header should not show subtask context")
  local step3_row = find_row(buf, "NEW b.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == step3_row, "cursor not on step 3 row")

  trigger_buf_mapping(buf, "z")
  wait_for(function() return box_contains(buf, "task total") end, "z did not return to step 2")

  trigger_buf_mapping(buf, "z")
  wait_for(function() return box_contains(buf, "concrete changed line") end, "z did not return to step 1")

  trigger_buf_mapping(buf, "z")
  wait_for(function() return box_contains(buf, "concrete changed line") end,
    "z at step 1 should stay on the first step")
  trigger_buf_mapping(buf, "q")
  wait_for(function() return walkthrough_extmark_count(buf) == 0 end, "quit did not clear extmarks")
  local restored_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)
  assert_true(restored_q_desc == original_q_desc, "q mapping not restored: " .. tostring(restored_q_desc))
  local z_map = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("z", "n", false, true)
  end)
  assert_true(z_map.buffer ~= 1, "z should not remain buffer-mapped after quit")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2):") end,
    "quit should restore the flat GitStatus presentation")
  assert_true(not buffer_contains(buf, "Overview:"), "quit should clear walkthrough presentation")

  -- ── completion past the last step ──────────────────────────────────────────
  start_walkthrough(buf)
  wait_for(function() return box_contains(buf, "concrete changed line") end, "restart did not show step 1")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "task total") end, "restart did not reach step 2")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "forward navigation") end, "restart did not reach step 3")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return saw_notification_containing("Walkthrough complete") end, "no completion notification")
  assert_true(walkthrough_extmark_count(buf) == 0, "completion did not clear extmarks")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2):") end,
    "completion should restore the flat GitStatus presentation")
  close_all_floats()

  -- ── task file rows sort by walkthrough block order ─────────────────────────
  local ordered_doc = valid_doc()
  ordered_doc.tasks = {
    {
      title = "Order file rows by walkthrough comments.",
      groups = {
        {
          type = "File",
          title = "Ordered fixtures",
          subtasks = {
            {
              title = "Put b before a.",
              items = {
                {
                  action = "Update",
                  type = "Test",
                  title = "b.txt first",
                  note = "drive presentation order from comment blocks",
                  steps = {
                    {
                      title = "Visit b.txt first.",
                      file = "b.txt",
                      start = { line = 2, col = 1 },
                      ["end"] = { line = 2, col = 9 },
                      comment = "The first walkthrough block belongs to b.txt.",
                    },
                  },
                },
                {
                  action = "Update",
                  type = "Test",
                  title = "a.txt second",
                  note = "keep later files lower in the task section",
                  steps = {
                    {
                      title = "Visit a.txt second.",
                      file = "a.txt",
                      start = { line = 2, col = 1 },
                      ["end"] = { line = 2, col = 9 },
                      comment = "The second walkthrough block belongs to a.txt.",
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  }
  set_walkthrough_doc(ordered_doc)
  start_walkthrough(buf)
  local ordered_a_row = nil
  local ordered_b_row = nil
  for row, entry in pairs(diff_review._status.entries or {}) do
    if entry.kind == "file"
        and tostring(entry.file and entry.file.walkthrough_key_prefix or ""):find("walkthrough:task:1:unstaged", 1, true) then
      if file_entry_matches(entry, "a.txt") then
        ordered_a_row = row
      elseif file_entry_matches(entry, "b.txt") then
        ordered_b_row = row
      end
    end
  end
  assert_true(ordered_a_row ~= nil and ordered_b_row ~= nil,
    "ordered walkthrough task should render both fixture file rows")
  assert_true(ordered_b_row < ordered_a_row, "b.txt should render before a.txt by walkthrough step order")
  trigger_buf_mapping(buf, "q")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2):") end,
    "ordered walkthrough quit should restore the flat GitStatus presentation")
  close_all_floats()

  -- ── document staleness warning ─────────────────────────────────────────────
  local stale_doc = valid_doc()
  stale_doc.commit = string.rep("b", 40)
  set_walkthrough_doc(stale_doc)
  start_walkthrough(buf)
  wait_for(function() return saw_notification_containing("Walkthrough generated against") end,
    "stale walkthrough should warn via notification")
  trigger_buf_mapping(buf, "q")
  wait_for(function() return buffer_contains(buf, "Unstaged changes (2):") end,
    "closing stale walkthrough should restore the flat GitStatus presentation")
  close_all_floats()

  -- ── step staleness: nearest line + missing file ───────────────────────────
  local degraded = valid_doc()
  degraded.tasks = {
    {
      title = "Resolve stale walkthrough targets.",
      groups = {
        {
          type = "File",
          title = "Stale targets",
          subtasks = {
            {
              title = "Exercise degraded target resolution.",
              items = {
                {
                  action = "Update",
                  type = "Test",
                  title = "Stale line reference",
                  note = "fall back to the nearest rendered line",
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
                  action = "Update",
                  type = "Test",
                  title = "Missing file reference",
                  note = "surface a missing-file note instead of failing",
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
      },
    },
  }
  set_walkthrough_doc(degraded)
  start_walkthrough(buf)
  ensure_walkthrough_file_expanded(buf, "a.txt", nil, "NEW a.txt")
  wait_for(function() return box_contains(buf, "position approximated") end, "nearest match note missing")
  trigger_buf_mapping(buf, "y")
  assert_true(not box_contains(buf, "not in current diff"),
    "missing walkthrough targets should not render comment boxes without visible diff rows")
  trigger_buf_mapping(buf, "q")
  close_all_floats()

  -- ── partially staged file + region split across hunks ─────────────────────
  local staged_doc = valid_doc()
  staged_doc.tasks = {
    {
      title = "Resolve staged and split walkthrough regions.",
      groups = {
        {
          type = "File",
          title = "Fixture diff sections",
          subtasks = {
            {
              title = "Resolve nontrivial rendered regions.",
              items = {
                {
                  action = "Update",
                  type = "Test",
                  title = "Staged region",
                  note = "anchor the staged-only section",
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
                  action = "Update",
                  type = "Test",
                  title = "Split region",
                  note = "anchor the first rendered split row",
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
                {
                  action = "Update",
                  type = "Test",
                  title = "Long region",
                  note = "keep start visible for long regions",
                  steps = {
                    {
                      title = "Long region",
                      file = "a.txt",
                      start = { line = 20, col = 1 },
                      ["end"] = { line = 55, col = 5 },
                      comment = "Long selected region keeps its start visible.",
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  }
  set_walkthrough_doc(staged_doc)
  start_walkthrough(buf)
  ensure_walkthrough_file_expanded(buf, "c.txt", "walkthrough:task:1:staged", "NEW c.txt")
  wait_for(function() return box_contains(buf, "staged section") end, "staged step box did not render")
  local staged_row = find_row(buf, "NEW c.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == staged_row, ("cursor not on the staged hunk row (expected %d, got %d)"):format(staged_row, cursor_row))
  assert_true(not box_contains(buf, "stale"), "staged region must not be flagged stale")

  ensure_walkthrough_file_expanded(buf, "a.txt", "walkthrough:task:1:unstaged", "NEW2 a.txt")
  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "ends inside the second hunk") end, "split-region step box did not render")
  local second_hunk_row = find_row(buf, "NEW2 a.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(
    cursor_row == second_hunk_row,
    ("split region did not anchor at the first rendered row inside the region (expected %d, got %d)"):format(second_hunk_row, cursor_row)
  )
  assert_true(not box_contains(buf, "approximated"), "split region must not be flagged approximated")

  trigger_buf_mapping(buf, "y")
  wait_for(function() return box_contains(buf, "Long selected region keeps its start visible") end,
    "long-region step box did not render")
  local long_start_row = find_row(buf, "NEW long 02 a.txt")
  cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(
    cursor_row == long_start_row,
    ("long region did not anchor at the first rendered changed row (expected %d, got %d)"):format(long_start_row, cursor_row)
  )
  local long_box_mark = comment_box_mark_containing(buf, "Long selected region keeps its start visible")
  assert_true(long_box_mark ~= nil, "long-region comment box mark missing")
  local long_anchor_row = long_box_mark[2] + 1
  assert_true(long_anchor_row >= long_start_row,
    ("long-region box should anchor at or after the rendered start row (start %d, anchor %d)"):format(
      long_start_row, long_anchor_row))
  win = vim.fn.bufwinid(buf)
  view = vim.api.nvim_win_call(win, function() return vim.fn.winsaveview() end)
  local long_win_height = vim.api.nvim_win_get_height(win)
  local long_expected_topline = expected_comment_topline(long_start_row, long_anchor_row,
    #long_box_mark[4].virt_lines, long_win_height)
  assert_true(view.topline == long_expected_topline,
    ("long-region view not focused near lower comment box (expected topline %d, got %d)"):format(
      long_expected_topline, view.topline))
  assert_true(view.topline <= long_start_row and long_start_row < view.topline + long_win_height,
    ("long-region start row should stay visible (topline %d, start %d, height %d)"):format(
      view.topline, long_start_row, long_win_height))
  local long_expected_box_screen_row = expected_comment_screen_row(long_start_row, long_anchor_row,
    #long_box_mark[4].virt_lines, long_win_height)
  local long_box_screen_row = long_anchor_row - view.topline + 2
  assert_true(long_box_screen_row == long_expected_box_screen_row,
    ("long-region comment box should start one third from the bottom (expected screen row %d, got %d)"):format(
      long_expected_box_screen_row, long_box_screen_row))
  trigger_buf_mapping(buf, "q")
  close_all_floats()

  -- ── re-render while active re-applies decorations ──────────────────────────
  set_walkthrough_doc(valid_doc())
  start_walkthrough(buf)
  ensure_walkthrough_file_expanded(buf, "a.txt", "walkthrough:task:1:unstaged", "NEW a.txt")
  wait_for(function() return walkthrough_extmark_count(buf) > 0 end, "no extmarks before re-render")
  diff_review.render_status(buf, nil, nil, { reuse_sections = true })
  wait_for(function() return walkthrough_extmark_count(buf) > 0 end, "re-render dropped walkthrough extmarks")
  wait_for(function() return box_contains(buf, "concrete changed line") end, "re-render dropped the inline comment box")
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
  wait_for(function() return saw_notification_containing("expected 7") end, "v1 rejection notification absent")

  local invalid_group_type = valid_doc()
  invalid_group_type.tasks[1].groups[1].type = "Method"
  set_walkthrough_doc(invalid_group_type)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("group 1: missing or invalid \"type\"") end,
    "invalid group type notification absent")

  local invalid_item_type = valid_doc()
  invalid_item_type.tasks[1].groups[1].subtasks[1].items[1].type = "File"
  set_walkthrough_doc(invalid_item_type)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("item 1: missing or invalid \"type\"") end,
    "invalid item type notification absent")

  local invalid_item_subtype = valid_doc()
  invalid_item_subtype.tasks[1].groups[1].subtasks[1].items[1].subtype = ""
  set_walkthrough_doc(invalid_item_subtype)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("item 1: invalid \"subtype\"") end,
    "invalid item subtype notification absent")

  local invalid_item_note = valid_doc()
  invalid_item_note.tasks[1].groups[1].subtasks[1].items[1].note = string.rep("x", 51)
  set_walkthrough_doc(invalid_item_note)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"note\" must be 50 characters or less") end,
    "invalid item note length notification absent")

  local invalid_item_children = valid_doc()
  invalid_item_children.tasks[1].groups[1].subtasks[1].items[1].children = {}
  set_walkthrough_doc(invalid_item_children)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"children\" is not supported") end,
    "invalid item children notification absent")

  local missing_item_steps = valid_doc()
  missing_item_steps.tasks[1].groups[1].subtasks[1].items[1].steps = nil
  set_walkthrough_doc(missing_item_steps)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("missing or empty \"steps\"") end,
    "missing item steps notification absent")

  local invalid_callout_kind = valid_doc()
  invalid_callout_kind.tasks[1].groups[1].subtasks[1].items[1].steps[1].callout.kind = "note"
  set_walkthrough_doc(invalid_callout_kind)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("callout has invalid \"kind\"") end,
    "invalid callout kind notification absent")

  local plural_callouts = valid_doc()
  plural_callouts.tasks[1].groups[1].subtasks[1].items[1].steps[1].callouts = {}
  set_walkthrough_doc(plural_callouts)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"callouts\" is not supported") end,
    "plural callouts notification absent")

  local too_long_callout = valid_doc()
  too_long_callout.tasks[1].groups[1].subtasks[1].items[1].steps[1].callout.text = string.rep("x", 181)
  set_walkthrough_doc(too_long_callout)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("callout \"text\" must be 180 characters or less") end,
    "too-long callout notification absent")

  local invalid_task_justification = valid_doc()
  invalid_task_justification.tasks[1].justification = ""
  set_walkthrough_doc(invalid_task_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("task 1: invalid \"justification\"") end,
    "invalid task justification notification absent")

  local too_long_task_justification = valid_doc()
  too_long_task_justification.tasks[1].justification = string.rep("x", 81)
  set_walkthrough_doc(too_long_task_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"justification\" must be 80 characters or less") end,
    "too-long task justification notification absent")

  local invalid_subtask_justification = valid_doc()
  invalid_subtask_justification.tasks[1].groups[1].subtasks[1].justification = ""
  set_walkthrough_doc(invalid_subtask_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("subtask 1: invalid \"justification\"") end,
    "invalid subtask justification notification absent")

  local too_long_subtask_justification = valid_doc()
  too_long_subtask_justification.tasks[1].groups[1].subtasks[1].justification = string.rep("x", 81)
  set_walkthrough_doc(too_long_subtask_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"justification\" must be 80 characters or less") end,
    "too-long subtask justification notification absent")
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
