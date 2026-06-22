vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local walkthrough = require("diff_review.walkthrough")

local root = "D:/diffreview-flow-root"
local head_sha = string.rep("a", 40)
local untracked_files = {}

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

local function new_file_diff(relpath)
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "new file mode 100644",
    "--- /dev/null",
    "+++ b/" .. relpath,
    "@@ -0,0 +1,4 @@",
    "+line one " .. relpath,
    "+line two " .. relpath,
    "+line three " .. relpath,
    "+line four " .. relpath,
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
    return untracked_files, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return { "M\ta.txt", "M\tb.txt", "A\tnew.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return { "M\tc.txt" }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(
      long_region_diff("a.txt") .. "\n" .. modified_diff("b.txt") .. "\n" .. new_file_diff("new.txt"),
      "\n",
      { plain = true }
    ), 0
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

local function buffer_has_status_highlight_for_text(buf, line_needle, text, hl_group)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for row, line in ipairs(lines) do
    if line:find(line_needle, 1, true) then
      local start_byte = line:find(text, 1, true)
      if not start_byte then return false end
      local start_col = start_byte - 1
      local end_col = start_col + #text
      for _, highlight in ipairs(diff_review._status and diff_review._status.highlights or {}) do
        if highlight.line == row
            and highlight.hl_group == hl_group
            and highlight.start_col <= start_col
            and highlight.end_col >= end_col then
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

local function assert_row_before(buf, first_needle, second_needle, message)
  local first_row = find_row(buf, first_needle)
  local second_row = find_row(buf, second_needle)
  assert_true(first_row < second_row, message .. (" (%s row %d, %s row %d)"):format(
    first_needle,
    first_row,
    second_needle,
    second_row
  ))
end

local function find_line(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  return lines[find_row(buf, needle)]
end

local function find_list_row(lines, needle)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
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

local function current_win_view(buf)
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "buffer has no window: " .. tostring(buf))
  return vim.api.nvim_win_call(win, function()
    return vim.fn.winsaveview()
  end)
end

local function set_cursor_with_view(buf, row, col, topline)
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "buffer has no window: " .. tostring(buf))
  vim.api.nvim_win_call(win, function()
    vim.api.nvim_win_set_cursor(0, { row, col or 0 })
    vim.fn.winrestview({
      lnum = row,
      col = col or 0,
      curswant = col or 0,
      leftcol = 0,
      topline = math.max(1, topline or row),
    })
  end)
  return current_win_view(buf)
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

local function row_has_line_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    if (mark[4] or {}).line_hl_group == hl_group then return true end
  end
  return false
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

local function comment_box_mark_containing(buf, needle)
  local marks = vim.api.nvim_buf_get_extmarks(buf, walkthrough._ns, 0, -1, { details = true })
  for _, mark in ipairs(marks) do
    local chunks = {}
    for _, virt_line in ipairs(mark[4].virt_lines or {}) do
      for _, chunk in ipairs(virt_line) do
        chunks[#chunks + 1] = chunk[1]
      end
    end
    if table.concat(chunks, "\n"):find(needle, 1, true) then return mark end
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
    version = 10,
    flow = {
      {
        text = "Walkthrough fixture JSON",
        children = {
          {
            text = "DiffReviewWalkthrough parser",
            children = {
              {
                text = "status summary rows",
                children = {
                  { text = "Inventory counts" },
                  { text = "Inline comment boxes" },
                },
              },
            },
          },
        },
      },
    },
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
                changes = {
                  {
                    action = "Modify",
                    kind = "Struct",
                    role = "Cache",
                    target = "a.txt rewrite",
                    note = "rewrite the second line for the first fixture file",
                    annotations = {
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
                changes = {
                  {
                    action = "Add",
                    kind = "Function",
                    target = "b.txt rewrite",
                    note = "repeat the rewrite so navigation crosses tasks",
                    annotations = {
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
  wait_for(function() return buffer_contains(buf, "Unstaged changes") end, "status did not render")
  return buf
end

local function start_walkthrough(buf)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return buffer_contains(buf, "Walkthrough:") end, "walkthrough section did not render")
  return buf
end

local function toggle_row(buf, needle)
  local row = find_row(buf, needle)
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "status window missing")
  vim.api.nvim_win_set_cursor(win, { row, 0 })
  trigger_buf_mapping(buf, "<Tab>")
end

local function expand_row_if_needed(buf, row_needle, visible_needle)
  if buffer_contains(buf, visible_needle) then return end
  toggle_row(buf, row_needle)
  wait_for(function() return buffer_contains(buf, visible_needle) end, row_needle .. " did not expand")
end

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  walkthrough.set_reader(fixture_reader)
  diff_review.setup({ about_auto_generate = false })

  local wide_flow = walkthrough._flow_summary_lines_for_test(valid_doc().flow, 120)
  assert_true(wide_flow[1] == "Walkthrough fixture JSON → DiffReviewWalkthrough parser → status summary rows",
    "wide flow should compact the single-child chain before rendering branches")
  assert_true(wide_flow[2] == "  ├→ Inventory counts",
    "wide flow should render the first branch without an inline connector gap")
  assert_true(wide_flow[3] == "  └→ Inline comment boxes",
    "wide flow should render sibling branches in a compact branch block")

  local narrow_flow = walkthrough._flow_summary_lines_for_test(valid_doc().flow, 40)
  assert_true(narrow_flow[1] == "• Walkthrough fixture JSON",
    "narrow flow should fall back to the vertical root line instead of overflowing")
  assert_true(narrow_flow[2] == "    → DiffReviewWalkthrough parser",
    "narrow flow should keep vertical chain continuation")
  assert_true(narrow_flow[4] == "    ├→ Inventory counts",
    "narrow flow should keep vertical branch markers")

  local particle_flow = {
    {
      text = "ParticleSpawn",
      children = {
        {
          text = "ParticleRenderMode",
          children = {
            {
              text = "ParticleStorage",
              children = {
                {
                  text = "ParticleInstanceSources",
                  children = {
                    { text = "ParticleBillboardRenderSystem" },
                    {
                      text = "ModelRenderSystem",
                      children = {
                        {
                          text = "ParticlePbRenderPipeline",
                          children = {
                            { text = "shared PBR shader path" },
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
      },
    },
  }
  local particle_lines = walkthrough._flow_summary_lines_for_test(particle_flow, 100)
  assert_true(#particle_lines == 3, "particle flow should use a compact detached branch without overflowing")
  for _, line in ipairs(particle_lines) do
    assert_true(vim.fn.strdisplaywidth(line) <= 100, "particle flow line should fit available width: " .. line)
  end
  assert_true(particle_lines[1] == "ParticleSpawn → ParticleRenderMode → ParticleStorage → ParticleInstanceSources",
    "particle flow should keep the trunk horizontal")
  assert_true(particle_lines[2] == "  ├→ ParticleBillboardRenderSystem",
    "particle flow should render the first branch without trunk overflow")
  assert_true(particle_lines[3] == "  └→ ModelRenderSystem → ParticlePbRenderPipeline → shared PBR shader path",
    "particle flow should keep the model renderer chain horizontal")

  local branched_particle_flow = {
    {
      text = "ParticleSpawn",
      children = {
        particle_flow[1].children[1],
        {
          text = "ParticleFastRenderMode",
          children = {
            {
              text = "FastParticleStorage",
              children = {
                { text = "FastParticleRenderSystem" },
              },
            },
          },
        },
      },
    },
  }
  local branched_particle_lines = walkthrough._flow_summary_lines_for_test(branched_particle_flow, 100)
  assert_true(branched_particle_lines[1] == "ParticleSpawn",
    "branched particle flow should not inline the first root branch")
  assert_true(branched_particle_lines[2] ==
      "  ├→ ParticleRenderMode → ParticleStorage → ParticleInstanceSources",
    "branched particle flow should render the first branch as a compact block")
  assert_true(branched_particle_lines[5] ==
      "  └→ ParticleFastRenderMode → FastParticleStorage → FastParticleRenderSystem",
    "branched particle flow should render the second root branch without a large connector gap")

  -- ── integrated status summary + lazy inline comment boxes ─────────────────
  set_walkthrough_doc(valid_doc())
  local buf = open_status()
  local original_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)

  vim.o.columns = 120
  local summary_buf = start_walkthrough(buf)
  local float_win = find_float_with("walkthrough fixture")
  assert_true(float_win == nil, "walkthrough should not open a summary popup")
  assert_true(not buffer_contains(summary_buf, "WARNING"), "fresh walkthrough should not warn")
  assert_true(not buffer_contains(summary_buf, "Major changes:"), "summary should not show redundant major changes heading")
  assert_true(not buffer_contains(summary_buf, "├─ Update a.txt through the first task."), "summary should not show redundant top-level graph")
  assert_true(buffer_contains(summary_buf,
    "Walkthrough fixture JSON → DiffReviewWalkthrough parser → status summary rows"),
    "summary should render the compact flow graph chain")
  assert_true(buffer_contains(summary_buf, "├→ Inventory counts"),
    "summary should render a branch flow graph edge")
  assert_true(buffer_contains(summary_buf, "└→ Inline comment boxes"),
    "summary should render the final branch flow graph edge")
  wait_for(function() return buffer_contains(summary_buf, "Inventory:") end,
    "walkthrough inventory did not render")
  assert_row_before(summary_buf, "Walkthrough fixture JSON → DiffReviewWalkthrough parser",
    "1. Update a.txt through the first task.", "flow graph should render before walkthrough tasks")
  assert_row_before(summary_buf, "1. Update a.txt through the first task.", "Inventory:",
    "walkthrough tasks should render before inventory")
  assert_row_before(summary_buf, "Inventory:", "Unstaged changes",
    "inventory should render before status change sections")
  local inventory_file_row = "files +1 0 ~3"
  assert_true(buffer_contains(summary_buf, inventory_file_row),
    "walkthrough inventory should count changed files")
  assert_true(buffer_has_status_highlight_for_text(summary_buf, inventory_file_row,
    "+1", "DiffReviewAddRange"), "inventory added count should be green")
  assert_true(buffer_has_status_highlight_for_text(summary_buf, inventory_file_row,
    "~3", "DiffReviewModifyRange"), "inventory modified count should use modify highlight")
  local display_rows = walkthrough._status_inventory_display_rows_for_test({
    { label = "function", added = 37, removed = 0, modified = 27 },
    { label = "struct", added = 16, removed = 0, modified = 6 },
    { label = "enum", added = 2, removed = 0, modified = 0 },
    { label = "type", added = 2, removed = 0, modified = 1 },
    { label = "module", added = 6, removed = 0, modified = 0 },
    { label = "files", added = 13, removed = 0, modified = 21 },
    { label = "docs", added = 2, removed = 0, modified = 1 },
    { label = "plans", added = 2, removed = 0, modified = 0 },
  })
  assert_true(display_rows[1].text == "function +37 0 ~27  type     +2  0 ~1   docs  +2 0 ~1",
    "inventory should render docs in a third column")
  assert_true(display_rows[2].text == "struct   +16 0 ~6   module   +6  0 0    plans +2 0 0 ",
    "inventory should align plans in the third column")
  assert_true(display_rows[3].text == "enum     +2  0 0    files    +13 0 ~21",
    "inventory should align the final inventory row")
  assert_true(display_rows[1].cells[1].label == "function"
      and display_rows[1].cells[2].label == "type"
      and display_rows[1].cells[3].label == "docs",
    "inventory display rows should expose left, middle, and right cell targets")
  local detail_lines, _, detail_targets = walkthrough._inventory_detail_lines_for_test("function", {
    ["function"] = {
      added = {
        { name = "DeepModule.build", relpath = "src/deep/module.ts", line = 42 },
        { name = "Other.run", relpath = "src/other.ts", line = 7 },
      },
      modified = {
        { name = "DeepModule.update", relpath = "src/deep/module.ts", line = 58 },
      },
      removed = {
        { name = "OldModule.remove", relpath = "src/old.ts", line = 12 },
      },
    },
  })
  local detail_text = table.concat(detail_lines, "\n")
  assert_true(detail_text:find("Added:\nmodule.ts\n  DeepModule.build", 1, true) ~= nil,
    "inventory detail should group added symbols by basename")
  assert_true(detail_text:find("other.ts\n  Other.run", 1, true) ~= nil,
    "inventory detail should include later files in the added section")
  assert_true(find_list_row(detail_lines, "Added:") < find_list_row(detail_lines, "Modified:"),
    "inventory detail should list modified symbols after added symbols")
  assert_true(find_list_row(detail_lines, "Modified:") < find_list_row(detail_lines, "Deleted:"),
    "inventory detail should list deleted symbols after modified symbols")
  assert_true(detail_text:find("Modified:\nmodule.ts\n  DeepModule.update", 1, true) ~= nil,
    "inventory detail should render modified symbols in a Modified section")
  assert_true(detail_text:find("Deleted:\nold.ts\n  OldModule.remove", 1, true) ~= nil,
    "inventory detail should render removed symbols in a Deleted section")
  local deep_row = find_list_row(detail_lines, "  DeepModule.build")
  assert_true(detail_targets[deep_row].relpath == "src/deep/module.ts" and detail_targets[deep_row].line == 42,
    "inventory detail symbol rows should expose jump targets")
  assert_true(detail_text:find("src/deep/module.ts", 1, true) == nil,
    "inventory detail locations should not show full repo-relative paths")
  local added_only_detail = walkthrough._inventory_detail_lines_for_test("function", {
    ["function"] = {
      added = {
        { name = "DeepModule.build", relpath = "src/deep/module.ts", line = 42 },
      },
      modified = {},
      removed = {},
    },
  })
  local added_only_text = table.concat(added_only_detail, "\n")
  assert_true(added_only_text:find("Modified:", 1, true) == nil,
    "inventory detail should omit empty modified sections")
  assert_true(added_only_text:find("Deleted:", 1, true) == nil,
    "inventory detail should omit empty deleted sections")
  local inventory_row = find_row(summary_buf, inventory_file_row)
  local inventory_col = display_col_before(find_line(summary_buf, inventory_file_row), "files")
  vim.api.nvim_win_set_cursor(vim.fn.bufwinid(summary_buf), { inventory_row, inventory_col })
  trigger_buf_mapping(summary_buf, "<CR>")
  local detail_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return detail_buf ~= summary_buf and buffer_contains(detail_buf, "Files changed") end,
    "pressing enter on inventory row should open detail buffer")
  assert_true(buffer_contains(detail_buf, "Added:"), "inventory detail should include added section")
  assert_true(buffer_contains(detail_buf, "Modified:"), "inventory detail should include modified section")
  assert_true(not buffer_contains(detail_buf, "Deleted:"), "inventory detail should omit empty deleted section")
  assert_true(buffer_contains(detail_buf, "new.txt"), "inventory detail should list added files")
  assert_true(buffer_contains(detail_buf, "a.txt"), "inventory detail should list updated files")
  trigger_buf_mapping(detail_buf, "q")
  wait_for(function() return vim.api.nvim_get_current_buf() == summary_buf end,
    "closing inventory detail should return to status buffer")
  local inventory_source_view = set_cursor_with_view(summary_buf, inventory_row, inventory_col, inventory_row - 2)
  trigger_buf_mapping(summary_buf, ".")
  detail_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return detail_buf ~= summary_buf and buffer_contains(detail_buf, "Files changed") end,
    "pressing dot on inventory row should reopen detail buffer")
  vim.cmd("normal! \15")
  wait_for(function()
    local view = current_win_view(summary_buf)
    return vim.api.nvim_get_current_buf() == summary_buf
      and vim.api.nvim_win_get_cursor(vim.fn.bufwinid(summary_buf))[1] == inventory_row
      and view.topline == inventory_source_view.topline
      and view.leftcol == inventory_source_view.leftcol
  end, "inventory row detail jump should save a native jump-list return location and view")
  trigger_buf_mapping(summary_buf, ".")
  detail_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return detail_buf ~= summary_buf and buffer_contains(detail_buf, "Files changed") end,
    "pressing dot on inventory row should reopen detail buffer after jumpback")
  local new_file_row = find_row(detail_buf, "  new.txt")
  vim.api.nvim_win_set_cursor(vim.fn.bufwinid(detail_buf), { new_file_row, 0 })
  trigger_buf_mapping(detail_buf, ".")
  wait_for(function()
    return vim.api.nvim_get_current_buf() ~= detail_buf
      and vim.api.nvim_buf_get_name(0):gsub("\\", "/"):find("D:/diffreview-flow-root/new.txt", 1, true) ~= nil
  end, "pressing dot on an inventory detail row should open the target file")
  assert_true(vim.api.nvim_win_get_cursor(0)[1] == 1,
    "inventory detail jump should move to the target line")
  vim.cmd("normal! \15")
  wait_for(function()
    return vim.api.nvim_get_current_buf() == detail_buf
      and vim.api.nvim_win_get_cursor(0)[1] == new_file_row
  end, "inventory detail jump should save a jump-list location back to the detail row")
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), summary_buf)
  assert_true(buffer_has_highlight_for_text(summary_buf, "1. Update a.txt through the first task.",
    "1. Update a.txt through the first task.", "DiffReviewWalkthroughItemTitle"),
    "summary task title should be bold white")
  assert_true(buffer_contains(summary_buf, "Reviewers need the fixture story before individual file rewrites."),
    "summary task justification missing from folded task row")
  assert_true(not buffer_has_highlight_for_text(summary_buf, "Reviewers need the fixture story", "Reviewers",
    "DiffReviewWalkthroughJustification"), "summary task justification should use normal highlight")
  assert_true(not buffer_contains(summary_buf, "The first fixture row carries the opening example for"),
    "folded task should hide subtask justification")
  assert_true(not buffer_contains(summary_buf, "file Fixture edits"), "folded task should hide group rows")
  toggle_row(buf, "1. Update a.txt through the first task.")
  wait_for(function() return buffer_contains(summary_buf, "file Fixture edits") end,
    "expanding first walkthrough task did not show group rows")
  toggle_row(buf, "file Fixture edits")
  wait_for(function() return not buffer_contains(summary_buf, "Rewrite the first fixture file.") end,
    "folding a walkthrough group should hide its subtasks")
  toggle_row(buf, "file Fixture edits")
  wait_for(function() return buffer_contains(summary_buf, "Rewrite the first fixture file.") end,
    "unfolding a walkthrough group should show its subtasks")
  toggle_row(buf, "└─ Rewrite the first fixture file.")
  wait_for(function() return not buffer_contains(summary_buf, "Modify Cache a.txt rewrite") end,
    "folding a walkthrough subtask should hide its items")
  toggle_row(buf, "└─ Rewrite the first fixture file.")
  wait_for(function() return buffer_contains(summary_buf, "Modify Cache a.txt rewrite") end,
    "unfolding a walkthrough subtask should show its items")
  local rewrite_location = "◦ Rewrite the fixture line to NEW.            [a.txt:2 +1 -1]"
  local total_marker_location = "◦ Check the total marker on the next comment. [a.txt:2 +1 -1]"
  assert_true(not buffer_contains(summary_buf, rewrite_location),
    "folded walkthrough action should hide comment-location rows")
  toggle_row(buf, "Modify Cache a.txt rewrite")
  wait_for(function() return buffer_contains(summary_buf, rewrite_location) end,
    "expanding a walkthrough action should show the first comment-location row")
  assert_true(buffer_has_status_highlight_for_text(summary_buf, rewrite_location,
    "a.txt:2", "DiffReviewWalkthroughLocation"),
    "comment-location row should highlight the file location with the walkthrough location highlight")
  assert_true(buffer_has_status_highlight_for_text(summary_buf, rewrite_location,
    "+1", "DiffReviewAddRange"), "comment-location row should highlight added count")
  assert_true(buffer_has_status_highlight_for_text(summary_buf, rewrite_location,
    "-1",
    "DiffReviewDeleteRange"), "comment-location row should highlight removed count")
  assert_true(not buffer_contains(summary_buf, "─── a.txt:2"),
    "comment-location row should not use box-drawing detail prefixes")
  assert_true(buffer_contains(summary_buf, total_marker_location),
    "expanding a walkthrough action should show all associated comment-location rows")
  assert_true(find_line(summary_buf, rewrite_location):find("[", 1, true)
    == find_line(summary_buf, total_marker_location):find("[", 1, true),
    "comment-location rows should align bracketed file labels")
  local location_row = find_row(summary_buf, rewrite_location)
  local location_source_view = set_cursor_with_view(summary_buf, location_row, 0, location_row - 3)
  trigger_buf_mapping(summary_buf, ".")
  wait_for(function() return buffer_contains(summary_buf, "NEW a.txt") end,
    "pressing dot on a walkthrough location should expand the target diff")
  local previous_mark = vim.api.nvim_buf_call(summary_buf, function()
    return vim.fn.getpos("''")
  end)
  assert_true(previous_mark[2] == location_row,
    "pressing dot on a walkthrough location should set the previous context mark")
  assert_true(vim.api.nvim_win_get_cursor(vim.fn.bufwinid(summary_buf))[1] == find_row(summary_buf, "NEW a.txt"),
    "pressing dot on a walkthrough location should jump to the target diff row")
  trigger_buf_mapping(summary_buf, ",")
  wait_for(function()
    local view = current_win_view(summary_buf)
    return vim.api.nvim_win_get_cursor(vim.fn.bufwinid(summary_buf))[1] == location_row
      and view.topline == location_source_view.topline
      and view.leftcol == location_source_view.leftcol
  end, "walkthrough location jump should save a native jump-list return location and view")
  toggle_row(buf, "a.txt +")
  wait_for(function() return not buffer_contains(summary_buf, "NEW a.txt") end,
    "folding target file after location jump should hide its diff again")
  toggle_row(buf, "Modify Cache a.txt rewrite")
  wait_for(function() return not buffer_contains(summary_buf, rewrite_location) end,
    "folding a walkthrough action should hide comment-location rows again")
  local expanded_second_task_row = find_row(summary_buf, "2. Update b.txt through the second task.")
  local expanded_lines = vim.api.nvim_buf_get_lines(summary_buf, 0, -1, false)
  assert_true(expanded_lines[expanded_second_task_row - 1] == "",
    "expanded walkthrough task should leave a blank separator before the next task")
  toggle_row(buf, "2. Update b.txt through the second task.")
  wait_for(function() return buffer_contains(summary_buf, "   └─ Add fn b.txt rewrite to repeat") end,
    "expanding second walkthrough task did not show item rows")
  assert_true(buffer_contains(summary_buf, "The first fixture row carries the opening example for"),
    "summary subtask justification missing")
  assert_true(buffer_contains(summary_buf, "rendering."),
    "summary subtask justification continuation missing")
  assert_true(not buffer_contains(summary_buf, "why:"), "summary justification should not render a why label")
  assert_true(buffer_contains(summary_buf, "file Fixture edits"), "summary group type row missing")
  assert_true((find_line(summary_buf, "file Fixture edits") or ""):match("^  file ") ~= nil,
    "expanded walkthrough group rows should be indented under the task")
  assert_true(buffer_contains(summary_buf, "└─ Rewrite the first fixture file."), "summary subtask row missing")
  assert_true((find_line(summary_buf, "└─ Rewrite the first fixture file.") or ""):match("^  └─ ") ~= nil,
    "expanded walkthrough subtask rows should be indented under the task")
  assert_true(buffer_contains(summary_buf, "   └─ Modify Cache a.txt rewrite to rewrite"),
    "summary item action row missing display verb")
  assert_true((find_line(summary_buf, "Modify Cache a.txt rewrite") or ""):match("^     └─ ") ~= nil,
    "expanded walkthrough action rows should preserve tree indentation under the task")
  assert_true(buffer_contains(summary_buf, "   └─ Add fn b.txt rewrite to repeat"),
    "summary add action row should not be padded")
  assert_true(buffer_contains(summary_buf, "the second line"),
    "summary item inline note prefix missing")
  assert_true(buffer_contains(summary_buf, "first fixture file"),
    "summary item inline note missing")
  wait_for(function() return buffer_has_highlight(summary_buf, "DiffReviewWalkthroughActionModify") end,
    "summary action highlight missing")
  assert_true(buffer_has_highlight(summary_buf, "DiffReviewWalkthroughActionModify"), "summary action highlight missing")
  assert_true(buffer_has_highlight(summary_buf, "DiffReviewWalkthroughItemTitle"), "summary item title highlight missing")
  assert_true(buffer_has_highlight_for_text(summary_buf, "Fixture edits", "file", "DiffReviewWalkthroughType"),
    "summary group type highlight missing")
  assert_true(buffer_has_highlight_for_text(summary_buf, "Fixture edits", "Fixture edits",
    "DiffReviewWalkthroughItemTitle"), "summary group title highlight missing")
  assert_true(not buffer_contains(summary_buf, "󰈙 file Fixture edits"), "summary should not show group type icon")
  assert_true(not buffer_contains(summary_buf, "File Fixture edits"), "summary should not show group type text")
  assert_true(not buffer_contains(summary_buf, "󰙅 Cache a.txt rewrite"), "summary should not show change kind icon")
  assert_true(not buffer_contains(summary_buf, "󰊕 fn b.txt rewrite"), "summary should not show function type icon")
  assert_true(not buffer_contains(summary_buf, "Struct a.txt rewrite"), "summary should not show change kind text")
  assert_true(not buffer_contains(summary_buf, "struct a.txt rewrite"), "summary should not show fallback type keyword")
  assert_true(buffer_contains(summary_buf, "Cache a.txt rewrite"), "summary should show change role text")
  assert_true(not buffer_contains(summary_buf, "Function b.txt rewrite"), "summary should not show function type text")
  assert_true(buffer_has_highlight_for_text(summary_buf, "a.txt rewrite", "Cache",
    "DiffReviewWalkthroughActionModify"), "summary modified change kind should match action highlight")
  assert_true(buffer_has_highlight_for_text(summary_buf, "b.txt rewrite", "fn",
    "DiffReviewWalkthroughActionAdd"), "summary added function type should match action highlight")
  assert_true(buffer_has_highlight_for_text(summary_buf, "Modify Cache a.txt rewrite", "Modify",
    "DiffReviewWalkthroughActionModify"), "summary action highlight missing")
  assert_true(buffer_has_highlight_for_text(summary_buf, "a.txt rewrite", "a.txt rewrite",
    "DiffReviewWalkthroughItemTitle"), "summary item title highlight missing")
  assert_true(not buffer_has_highlight_for_text(summary_buf, "a.txt rewrite to rewrite", "second",
    "DiffReviewWalkthroughItemTitle"), "summary inline note should not be title-highlighted")
  local action_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughActionModify" })
  assert_true(action_hl.italic == true, "summary action highlight should be italic")
  local type_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughType" })
  assert_true(type_hl.fg == 0x5bff94, "summary group type highlight should use config green")
  local title_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughItemTitle" })
  assert_true(title_hl.bold == true and title_hl.fg == 0xffffff, "summary item title should be bold white")
  assert_true(not buffer_has_highlight_for_text(summary_buf, "Reviewers need the fixture story", "Reviewers",
    "DiffReviewWalkthroughJustification"), "summary task justification should use normal highlight")
  assert_true(buffer_has_highlight_for_text(summary_buf, "The first fixture row", "The first fixture row",
    "DiffReviewWalkthroughJustification"), "summary subtask justification highlight missing")
  local justification_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughJustification" })
  assert_true(justification_hl.fg == 0xe5c07b and justification_hl.italic == true,
    "summary justification should be yellow italic")
  assert_true(not buffer_contains(summary_buf, "Legend:"), "summary should not show an action legend")
  assert_true(not box_contains(buf, "concrete"), "collapsed files should not render walkthrough boxes")

  expand_row_if_needed(buf, "a.txt +", "NEW a.txt")
  wait_for(function() return box_contains(buf, "concrete") end, "visible a.txt comment box missing")
  wait_for(function() return box_contains(buf, "task total") end, "second visible a.txt comment box missing")
  local step_row = find_row(buf, "NEW a.txt")
  local cursor_row = vim.api.nvim_win_get_cursor(vim.fn.bufwinid(buf))[1]
  assert_true(cursor_row == find_row(buf, "a.txt +"), "expanding a file should not jump to the walkthrough step")
  assert_true(walkthrough_extmark_count(buf) > 0, "walkthrough extmarks missing")
  assert_true(not row_has_line_highlight(buf, step_row, "DiffReviewWalkthroughRegionAdd"),
    "automatic walkthrough comments should not highlight selected regions")
  assert_true(not row_has_line_highlight(buf, step_row, "DiffReviewWalkthroughRegion"),
    "automatic walkthrough comments should not use generic blue region background")
  assert_true(box_contains(buf, "Deviation:"), "inline comment box should render the callout kind")
  assert_true(box_contains(buf, "high-priority review context"), "inline comment box should render callout text")
  assert_true(box_has_highlight_for_text(buf, "Deviation:", "DiffReviewWalkthroughCalloutDeviation"),
    "inline callout kind should use deviation highlight")
  local deviation_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewWalkthroughCalloutDeviation" })
  assert_true(deviation_hl.fg == 0xff5555 and deviation_hl.bold == true,
    "deviation callout should be red and bold")
  local box_mark = comment_box_mark_containing(buf, "concrete")
  assert_true(box_mark ~= nil, "inline comment box mark missing")
  assert_true(box_mark[2] == step_row - 1, "inline comment box should anchor below the selected row")
  assert_true(box_mark[4].virt_lines_above ~= true, "inline comment box should render below the selected row")
  assert_true(box_contains(buf, "1.1-2 Rewrite the fixture line to NEW."), "inline box heading missing")
  assert_true(box_contains(buf, "1.2-2 Check the total marker on the next comment."),
    "automatic mode should render all visible comments")
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
  assert_true(not box_contains(buf, "Modify Cache a.txt rewrite"), "inline box should not show item context")
  assert_true(not box_contains(buf, "rewrite the second line for the first fixture file"),
    "inline box should not show change note")
  assert_true(not box_contains(buf, "[z] back"), "inline box should not show command footer")

  expand_row_if_needed(buf, "b.txt +", "NEW b.txt")
  wait_for(function() return box_contains(buf, "forward navigation") end, "step 3 box did not render")
  assert_true(box_contains(buf, "2.1-1 Mirror the rewrite in the second fixture."), "step 3 task label missing")
  assert_true(not box_contains(buf, "Task 2.1-1 Mirror the rewrite in the second fixture."),
    "step 2 task label should not include Task")
  local step3_box_mark = comment_box_mark_containing(buf, "forward navigation")
  assert_true(comment_box_header_text(step3_box_mark):find("Rewrite the second fixture file.", 1, true) == nil,
    "step 3 header should not show subtask context")
  local restored_q_desc = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("q", "n", false, true).desc
  end)
  assert_true(restored_q_desc == original_q_desc, "q mapping should not be overridden: " .. tostring(restored_q_desc))
  local z_map = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("z", "n", false, true)
  end)
  assert_true(z_map.buffer ~= 1, "z should not be buffer-mapped by integrated walkthrough")

  toggle_row(buf, "a.txt +")
  wait_for(function() return not box_contains(buf, "concrete") end,
    "folding a.txt should remove its visible walkthrough boxes")
  assert_true(box_contains(buf, "forward navigation"), "folding a.txt should keep b.txt's visible box")

  toggle_row(buf, "Walkthrough:")
  wait_for(function()
    return buffer_contains(buf, "Walkthrough:") and not buffer_contains(buf, "Reviewers need the fixture story")
  end, "walkthrough summary did not fold")
  toggle_row(buf, "Walkthrough:")
  wait_for(function() return buffer_contains(buf, "Reviewers need the fixture story") end,
    "walkthrough summary did not unfold")

  diff_review.render_status(buf, nil, nil, { reuse_sections = true })
  wait_for(function() return box_contains(buf, "forward navigation") end, "re-render dropped visible walkthrough boxes")

  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "walkthrough did not toggle off")
  wait_for(function() return walkthrough_extmark_count(buf) == 0 end, "toggle off did not clear walkthrough extmarks")
  assert_row_before(buf, "a.txt +", "b.txt +", "inactive status should use path order")

  local graph_doc = valid_doc()
  graph_doc.tasks[1].groups[1].subtasks[1].changes[#graph_doc.tasks[1].groups[1].subtasks[1].changes + 1] = {
    action = "Modify",
    kind = "Struct",
    role = "Cache",
    target = "a.txt follow-up",
    note = "keep the summary graph connected",
    annotations = {
      {
        title = "Second action comment.",
        file = "a.txt",
        start = { line = 2, col = 1 },
        ["end"] = { line = 2, col = 9 },
        comment = "The second action keeps the first action from being the last sibling.",
      },
    },
  }
  set_walkthrough_doc(graph_doc)
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  expand_row_if_needed(buf, "Modify Cache a.txt rewrite",
    "   │  ◦ Rewrite the fixture line to NEW.            [a.txt:2 +1 -1]")
  assert_true(buffer_contains(summary_buf, "   │  ◦ Check the total marker on the next comment. [a.txt:2 +1 -1]"),
    "non-last action detail rows should keep the vertical tree guide")
  assert_row_before(buf, "   │  ◦ Check the total marker on the next comment. [a.txt:2 +1 -1]",
    "   └─ Modify Cache a.txt follow-up", "detail rows should remain visually connected before the next action")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "graph walkthrough did not toggle off")

  local basename_doc = valid_doc()
  basename_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations[1].file =
    "blue/engine/plugins/physics/src/particle_render.rs"
  set_walkthrough_doc(basename_doc)
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  expand_row_if_needed(buf, "Modify Cache a.txt rewrite",
    "◦ Rewrite the fixture line to NEW.            [particle_render.rs:2]")
  wait_for(function()
    return buffer_contains(summary_buf, "◦ Rewrite the fixture line to NEW.            [particle_render.rs:2]")
  end,
    "comment-location row should render nested paths as basenames")
  assert_true(not buffer_contains(summary_buf, "blue/engine/plugins/physics/src/particle_render.rs:2"),
    "comment-location row should not render full walkthrough paths")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "basename walkthrough did not toggle off")

  local added_span_doc = valid_doc()
  added_span_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Store solver buffers on a resource.",
      file = "new.txt",
      start = { line = 2, col = 1 },
      ["end"] = { line = 4, col = 1 },
      comment = "The selected span covers only the added buffer lines that belong to this explanation.",
    },
    {
      title = "Expose a render-facing buffer snapshot.",
      file = "new.txt",
      start = { line = 2, col = 1 },
      ["end"] = { line = 4, col = 1 },
      comment = "The second row keeps the location label aligned while sharing the same selected span.",
    },
  }
  set_walkthrough_doc(added_span_doc)
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  local added_span_location = "◦ Store solver buffers on a resource.       [new.txt:2 +3 -0]"
  local added_span_second_location = "◦ Expose a render-facing buffer snapshot.   [new.txt:2 +3 -0]"
  wait_for(function() return buffer_contains(summary_buf, added_span_location) end,
    "comment-location row should count added lines from the selected walkthrough span")
  assert_true(buffer_contains(summary_buf, added_span_second_location),
    "comment-location row should keep aligned labels for repeated selected spans")
  assert_true(find_line(summary_buf, added_span_location):find("[", 1, true)
    == find_line(summary_buf, added_span_second_location):find("[", 1, true),
    "selected-span location rows should align bracketed file labels")
  local added_span_row = find_row(summary_buf, added_span_location)
  vim.api.nvim_win_set_cursor(vim.fn.bufwinid(summary_buf), { added_span_row, 0 })
  trigger_buf_mapping(summary_buf, "<CR>")
  wait_for(function() return buffer_contains(summary_buf, "line two new.txt") end,
    "pressing enter on a selected-span walkthrough location should expand the target diff")
  assert_true(vim.api.nvim_win_get_cursor(vim.fn.bufwinid(summary_buf))[1] == find_row(summary_buf, "line two new.txt"),
    "pressing enter on a selected-span walkthrough location should jump to the selected diff row")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "added-span walkthrough did not toggle off")

  untracked_files = { "untracked.txt" }
  local untracked_doc = valid_doc()
  untracked_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Store untracked buffer rows.",
      file = "untracked.txt",
      start = { line = 2, col = 1 },
      ["end"] = { line = 4, col = 1 },
      comment = "The selected span belongs to a new untracked file before its synthetic diff has loaded.",
    },
  }
  set_walkthrough_doc(untracked_doc)
  buf = open_status()
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  local untracked_location = "◦ Store untracked buffer rows.              [untracked.txt:2 +3 -0]"
  wait_for(function() return buffer_contains(summary_buf, untracked_location) end,
    "comment-location row should count selected lines for untracked files without loaded hunks")
  assert_true(not buffer_contains(summary_buf, "untracked.txt:2 +0 -0"),
    "untracked walkthrough locations should not collapse to zero stats before diff load")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end,
    "untracked walkthrough did not toggle off")
  untracked_files = {}

  untracked_files = { "particle_storage.rs" }
  local count_alignment_doc = valid_doc()
  count_alignment_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Store solver buffers outside the system.",
      file = "particle_storage.rs",
      start = { line = 12, col = 1 },
      ["end"] = { line = 20, col = 1 },
      comment = "The selected span proves short added counts still align with wider neighboring added counts.",
    },
    {
      title = "Allocate compute storage from one capacity.",
      file = "particle_storage.rs",
      start = { line = 22, col = 1 },
      ["end"] = { line = 73, col = 1 },
      comment = "The wider added count should stay compact inside the bracketed location label.",
    },
    {
      title = "Write spawn batches into shared buffers.",
      file = "particle_storage.rs",
      start = { line = 88, col = 1 },
      ["end"] = { line = 114, col = 1 },
      comment = "The selected span keeps a compact bracketed location for another two-digit added count.",
    },
    {
      title = "Expose renderable particle buffers.",
      file = "particle_storage.rs",
      start = { line = 116, col = 1 },
      ["end"] = { line = 124, col = 1 },
      comment = "The longer line number should not add padding between the plus and minus counts.",
    },
  }
  set_walkthrough_doc(count_alignment_doc)
  buf = open_status()
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  local storage_location = "◦ Store solver buffers outside the system.    [particle_storage.rs:12 +9 -0]"
  local allocate_location = "◦ Allocate compute storage from one capacity. [particle_storage.rs:22 +52 -0]"
  local write_location = "◦ Write spawn batches into shared buffers.    [particle_storage.rs:88 +27 -0]"
  local expose_location = "◦ Expose renderable particle buffers.         [particle_storage.rs:116 +9 -0]"
  wait_for(function() return buffer_contains(summary_buf, storage_location) end,
    "comment-location row should keep short added counts compact before the removed count")
  assert_true(buffer_contains(summary_buf, allocate_location),
    "comment-location row should keep wider added counts compact")
  assert_true(buffer_contains(summary_buf, write_location),
    "comment-location row should keep another two-digit added count compact")
  assert_true(buffer_contains(summary_buf, expose_location),
    "comment-location row should not pad longer file line labels")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end,
    "count-alignment walkthrough did not toggle off")
  untracked_files = {}

  local padded_location_doc = valid_doc()
  padded_location_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Define particle render modes.",
      file = "blue/engine/plugins/physics/src/particle_render.rs",
      start = { line = 3, col = 1 },
      ["end"] = { line = 3, col = 1 },
      comment = "Render mode definitions give spawn data an explicit choice before the render handoff.",
    },
    {
      title = "Set triangle billboard as the default mode.",
      file = "blue/engine/plugins/physics/src/particle_render.rs",
      start = { line = 19, col = 1 },
      ["end"] = { line = 19, col = 1 },
      comment = "Existing particle spawns keep their old visual behavior unless they opt into model rendering.",
    },
    {
      title = "Store render mode on ParticleSpawn.",
      file = "blue/engine/plugins/physics/src/particle_spawn.rs",
      start = { line = 18, col = 1 },
      ["end"] = { line = 18, col = 1 },
      comment = "Spawn data now carries the selected rendering path beside the simulation inputs.",
    },
    {
      title = "Remove direct billboard rendering from simulation.",
      file = "blue/engine/plugins/physics/src/particle_system.rs",
      start = { line = 120, col = 1 },
      ["end"] = { line = 120, col = 1 },
      comment = "Billboard rendering moves out of the simulation loop so compute ownership stays separate from visuals.",
    },
  }
  set_walkthrough_doc(padded_location_doc)
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  expand_row_if_needed(buf, "Modify Cache a.txt rewrite",
    "◦ Remove direct billboard rendering from simulation. [particle_system.rs:120]")
  local define_location = "◦ Define particle render modes.                      [particle_render.rs:3]"
  local default_location = "◦ Set triangle billboard as the default mode.        [particle_render.rs:19]"
  local store_location = "◦ Store render mode on ParticleSpawn.                [particle_spawn.rs:18]"
  local remove_location = "◦ Remove direct billboard rendering from simulation. [particle_system.rs:120]"
  assert_true(buffer_contains(summary_buf, define_location),
    "comment-location row should render the title before the bracketed location")
  assert_true(buffer_contains(summary_buf, default_location),
    "comment-location row should not pad one-digit and two-digit line numbers")
  assert_true(buffer_contains(summary_buf, store_location),
    "comment-location row should not pad shorter filenames")
  assert_true(buffer_contains(summary_buf, remove_location),
    "comment-location row should keep the longest label bracketed after the title")
  local bracket_col = find_line(summary_buf, define_location):find("[", 1, true)
  assert_true(bracket_col == find_line(summary_buf, default_location):find("[", 1, true)
    and bracket_col == find_line(summary_buf, store_location):find("[", 1, true)
    and bracket_col == find_line(summary_buf, remove_location):find("[", 1, true),
    "comment-location rows should align bracketed file labels across the item")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "padded-location walkthrough did not toggle off")

  local global_location_doc = valid_doc()
  global_location_doc.tasks[1].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Short local step.",
      file = "a.txt",
      start = { line = 2, col = 1 },
      ["end"] = { line = 2, col = 1 },
      comment = "The short title should still align with the longest walkthrough location in the summary.",
    },
  }
  global_location_doc.tasks[1].groups[1].subtasks[1].changes[2] = {
    action = "Add",
    kind = "Function",
    target = "b.txt rewrite",
    note = "repeat the rewrite with a longer location row",
    annotations = {
      {
        title = "Longer walkthrough location title across tasks.",
        file = "b.txt",
        start = { line = 2, col = 1 },
        ["end"] = { line = 2, col = 1 },
        comment = "The long title in another action should set the shared location column for every step row.",
      },
    },
  }
  global_location_doc.tasks[2].groups[1].subtasks[1].changes[1].annotations = {
    {
      title = "Mirror the rewrite in the second fixture.",
      file = "b.txt",
      start = { line = 2, col = 1 },
      ["end"] = { line = 2, col = 1 },
      comment = "The second task remains part of the document but is not needed for this fold regression.",
    },
  }
  set_walkthrough_doc(global_location_doc)
  buf = open_status()
  captured_notifications = {}
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  expand_row_if_needed(buf, "└─ Rewrite the first fixture file.", "Modify Cache a.txt rewrite")
  expand_row_if_needed(buf, "Modify Cache a.txt rewrite", "◦ Short local step.")
  expand_row_if_needed(buf, "Add fn b.txt rewrite", "◦ Longer walkthrough location title across tasks.")
  local short_global_location = find_line(summary_buf, "◦ Short local step.")
  local long_global_location = find_line(summary_buf, "◦ Longer walkthrough location title across tasks.")
  assert_true(display_col_before(short_global_location, "[") == display_col_before(long_global_location, "["),
    "comment-location rows should align bracketed file labels across the whole walkthrough")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end,
    "global-location walkthrough did not toggle off")

  local reversed_doc = valid_doc()
  reversed_doc.tasks = { reversed_doc.tasks[2], reversed_doc.tasks[1] }
  set_walkthrough_doc(reversed_doc)
  summary_buf = start_walkthrough(buf)
  assert_row_before(buf, "b.txt +", "a.txt +", "active walkthrough should sort files by comment order")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "reversed walkthrough did not toggle off")
  assert_row_before(buf, "a.txt +", "b.txt +", "turning off walkthrough should restore path order")

  -- ── document staleness warning ─────────────────────────────────────────────
  local stale_doc = valid_doc()
  stale_doc.commit = string.rep("b", 40)
  set_walkthrough_doc(stale_doc)
  summary_buf = start_walkthrough(buf)
  assert_true(buffer_contains(summary_buf, "WARNING"), "stale walkthrough should warn in the summary")
  trigger_buf_mapping(buf, "ow")

  -- ── step staleness: nearest visible line ──────────────────────────────────
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
              changes = {
                {
                  action = "Modify",
                  kind = "Test",
                  target = "Stale line reference",
                  note = "fall back to the nearest rendered line",
                  annotations = {
                    {
                      file = "a.txt",
                      start = { line = 999, col = 1 },
                      ["end"] = { line = 999, col = 1 },
                      comment = "Stale line reference.",
                    },
                  },
                },
                {
                  action = "Modify",
                  kind = "Test",
                  target = "Missing file reference",
                  note = "surface a missing-file note instead of failing",
                  annotations = {
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
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "a.txt +", "NEW a.txt")
  wait_for(function() return box_contains(buf, "position approximated") end, "nearest match note missing")
  assert_true(not box_contains(buf, "not in current diff"), "missing files should not render comment boxes in status mode")
  trigger_buf_mapping(buf, "ow")

  -- ── partially staged file + visible region split across hunks ─────────────
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
              changes = {
                {
                  action = "Modify",
                  kind = "Test",
                  target = "Staged region",
                  note = "anchor the staged-only section",
                  annotations = {
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
                  action = "Modify",
                  kind = "Test",
                  target = "Split region",
                  note = "anchor the first rendered split row",
                  annotations = {
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
                  action = "Modify",
                  kind = "Test",
                  target = "Long region",
                  note = "keep start visible for long regions",
                  annotations = {
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
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "c.txt +", "NEW c.txt")
  wait_for(function() return box_contains(buf, "staged section") end, "staged step box did not render")
  assert_true(not box_contains(buf, "stale"), "staged region must not be flagged stale")

  expand_row_if_needed(buf, "a.txt +", "NEW a.txt")
  wait_for(function() return box_contains(buf, "ends inside the second hunk") end, "split-region step box did not render")
  local second_hunk_row = find_row(buf, "NEW2 a.txt")
  assert_true(not box_contains(buf, "approximated"), "split region must not be flagged approximated")

  wait_for(function() return box_contains(buf, "Long selected region keeps its start visible") end,
    "long-region step box did not render")
  local long_start_row = find_row(buf, "NEW long 02 a.txt")
  local long_box_mark = comment_box_mark_containing(buf, "Long selected region")
  assert_true(long_box_mark ~= nil, "long-region comment box mark missing")
  local long_anchor_row = long_box_mark[2] + 1
  assert_true(long_anchor_row >= long_start_row,
    ("long-region box should anchor at or after the rendered start row (start %d, anchor %d)"):format(
      long_start_row, long_anchor_row))
  trigger_buf_mapping(buf, "ow")

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

  fixtures[root .. "/.walkthrough.json"] = vim.json.encode({ version = 1, summary = "x", commit = "zz", annotations = {} })
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("expected 10") end, "v1 rejection notification absent")

  local missing_flow = valid_doc()
  missing_flow.flow = nil
  set_walkthrough_doc(missing_flow)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("missing or empty \"flow\"") end,
    "missing flow notification absent")

  local invalid_flow_node = valid_doc()
  invalid_flow_node.flow[1].text = ""
  set_walkthrough_doc(invalid_flow_node)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("flow 1: missing \"text\"") end,
    "invalid flow node notification absent")

  local artifact_type_doc = valid_doc()
  local artifact_changes = artifact_type_doc.tasks[1].groups[1].subtasks[1].changes
  artifact_changes[1].kind = "App"
  artifact_changes[1].role = nil
  artifact_changes[1].target = "app artifact"
  artifact_changes[1].note = "configure the app scenario"
  set_walkthrough_doc(artifact_type_doc)
  summary_buf = start_walkthrough(buf)
  expand_row_if_needed(buf, "1. Update a.txt through the first task.", "file Fixture edits")
  expand_row_if_needed(buf, "file Fixture edits", "└─ Rewrite the first fixture file.")
  wait_for(function()
    return buffer_contains(summary_buf, "Modify app app artifact")
  end, "App change kind should be accepted and rendered")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end, "artifact-type walkthrough did not toggle off")

  local invalid_group_type = valid_doc()
  invalid_group_type.tasks[1].groups[1].type = "Method"
  set_walkthrough_doc(invalid_group_type)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("group 1: missing or invalid \"type\"") end,
    "invalid group type notification absent")

  local invalid_change_kind = valid_doc()
  invalid_change_kind.tasks[1].groups[1].subtasks[1].changes[1].kind = "Doc"
  set_walkthrough_doc(invalid_change_kind)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("change 1: missing or invalid \"kind\"") end,
    "invalid change kind notification absent")

  local invalid_change_role = valid_doc()
  invalid_change_role.tasks[1].groups[1].subtasks[1].changes[1].role = ""
  set_walkthrough_doc(invalid_change_role)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("change 1: invalid \"role\"") end,
    "invalid change role notification absent")

  local invalid_change_note = valid_doc()
  invalid_change_note.tasks[1].groups[1].subtasks[1].changes[1].note = string.rep("x", 51)
  set_walkthrough_doc(invalid_change_note)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"note\" must be 50 characters or less") end,
    "invalid change note length notification absent")

  local invalid_change_children = valid_doc()
  invalid_change_children.tasks[1].groups[1].subtasks[1].changes[1].children = {}
  set_walkthrough_doc(invalid_change_children)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"children\" is not supported") end,
    "invalid change children notification absent")

  local missing_change_annotations = valid_doc()
  missing_change_annotations.tasks[1].groups[1].subtasks[1].changes[1].annotations = nil
  set_walkthrough_doc(missing_change_annotations)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("missing or empty \"annotations\"") end,
    "missing change annotations notification absent")

  local invalid_callout_kind = valid_doc()
  invalid_callout_kind.tasks[1].groups[1].subtasks[1].changes[1].annotations[1].callout.kind = "note"
  set_walkthrough_doc(invalid_callout_kind)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("callout has invalid \"kind\"") end,
    "invalid callout kind notification absent")

  local plural_callouts = valid_doc()
  plural_callouts.tasks[1].groups[1].subtasks[1].changes[1].annotations[1].callouts = {}
  set_walkthrough_doc(plural_callouts)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"callouts\" is not supported") end,
    "plural callouts notification absent")

  local too_long_callout = valid_doc()
  too_long_callout.tasks[1].groups[1].subtasks[1].changes[1].annotations[1].callout.text = string.rep("x", 181)
  set_walkthrough_doc(too_long_callout)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("callout \"text\" must be 180 characters or less") end,
    "too-long callout notification absent")

  local long_allowed_justification = valid_doc()
  long_allowed_justification.tasks[1].justification =
    "Rendering now has multiple paths, so each spawn declares its mode before render handoff."
  long_allowed_justification.tasks[1].groups[1].subtasks[1].justification =
    "The fixture now has a longer local rationale, so walkthrough validation must accept more than eighty characters."
  set_walkthrough_doc(long_allowed_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return buffer_contains(buf, "Walkthrough:") end,
    "long allowed justification should enter walkthrough mode")
  assert_true(not saw_notification_containing("\"justification\" must be"),
    "allowed long justification should not emit a length notification")
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return not buffer_contains(buf, "Walkthrough:") end,
    "long allowed justification walkthrough did not toggle off")

  local invalid_task_justification = valid_doc()
  invalid_task_justification.tasks[1].justification = ""
  set_walkthrough_doc(invalid_task_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("task 1: invalid \"justification\"") end,
    "invalid task justification notification absent")

  local too_long_task_justification = valid_doc()
  too_long_task_justification.tasks[1].justification = string.rep("x", 171)
  set_walkthrough_doc(too_long_task_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"justification\" must be 170 characters or less") end,
    "too-long task justification notification absent")

  local invalid_subtask_justification = valid_doc()
  invalid_subtask_justification.tasks[1].groups[1].subtasks[1].justification = ""
  set_walkthrough_doc(invalid_subtask_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("subtask 1: invalid \"justification\"") end,
    "invalid subtask justification notification absent")

  local too_long_subtask_justification = valid_doc()
  too_long_subtask_justification.tasks[1].groups[1].subtasks[1].justification = string.rep("x", 171)
  set_walkthrough_doc(too_long_subtask_justification)
  captured_notifications = {}
  trigger_buf_mapping(buf, "ow")
  wait_for(function() return saw_notification_containing("\"justification\" must be 170 characters or less") end,
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
