vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.integrations.gh")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local original_notify = vim.notify
local captured_notifications = {}
local review_data_dir = vim.fn.tempname()
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = { message = tostring(message), level = level, opts = opts }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

--- Two changed files, each "old line" -> replacement at new-file line 2.
local function file_diff(relpath, replacement)
  replacement = replacement or "NEW"
  return table.concat({
    "diff --git a/" .. relpath .. " b/" .. relpath,
    "index 1111111..2222222 100644",
    "--- a/" .. relpath,
    "+++ b/" .. relpath,
    "@@ -1,3 +1,3 @@",
    " alpha " .. relpath,
    "-old " .. relpath,
    "+" .. replacement .. " " .. relpath,
    " omega " .. relpath,
  }, "\n")
end

local pr_diff_text = file_diff("src/a.txt") .. "\n" .. file_diff("src/b.txt")

---@type { command: string[], input: string? }[]
local review_calls = {}
local system_call_keys = {}
local pending_review_reads = {}
local pending_review_creates = {}
local comment_creates = {}
local comment_updates = {}
local comment_deletes = {}
local review_should_fail = false
local remote_pending_review = nil
local remote_pending_comments = {}
local opened_urls = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  system_call_keys[#system_call_keys + 1] = key
  vim.defer_fn(function()
    if key:find("gh pr diff", 1, true) then
      cb({ code = 0, stdout = pr_diff_text, stderr = "", output = pr_diff_text })
    elseif key == "gh api graphql --input -" and input:find("query", 1, true) and input:find("reviewThreads", 1, true) then
      pending_review_reads[#pending_review_reads + 1] = { command = command, input = input }
      local review_node = remote_pending_review and {
        id = remote_pending_review.node_id,
        databaseId = remote_pending_review.id,
        state = remote_pending_review.state,
        viewerDidAuthor = true,
        body = remote_pending_review.body or "",
        author = remote_pending_review.user,
        commit = { oid = remote_pending_review.commit_id },
      } or nil
      local thread_nodes = {}
      if review_node then
        for _, comment in ipairs(remote_pending_comments) do
          thread_nodes[#thread_nodes + 1] = {
            comments = {
              nodes = {
                {
                  id = comment.node_id,
                  databaseId = comment.id,
                  body = comment.body,
                  path = comment.path,
                  line = comment.line,
                  position = comment.position,
                  createdAt = comment.created_at,
                  updatedAt = comment.updated_at,
                  author = comment.user,
                  pullRequestReview = {
                    id = review_node.id,
                    databaseId = review_node.databaseId,
                    state = review_node.state,
                  },
                },
              },
            },
          }
        end
      end
      local stdout = vim.json.encode({
        data = {
          repository = {
            pullRequest = {
              reviews = { nodes = review_node and { review_node } or {} },
              reviewThreads = { nodes = thread_nodes },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    elseif key == "gh api --method POST /repos/owner/repo/pulls/12/reviews --input -" then
      pending_review_creates[#pending_review_creates + 1] = { command = command, input = input }
      local stdout = vim.json.encode({ id = 7, node_id = "PRR_7", state = "PENDING" })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    elseif key == "gh api graphql --input -" and input:find("addPullRequestReviewComment", 1, true) then
      local payload = vim.json.decode(input or "{}")
      local mutation_input = payload.variables and payload.variables.input or {}
      comment_creates[#comment_creates + 1] = { command = command, input = input, payload = mutation_input }
      local database_id = 100 + #comment_creates
      local stdout = vim.json.encode({
        data = {
          addPullRequestReviewComment = {
            comment = {
              id = "PRRC_" .. tostring(database_id),
              databaseId = database_id,
              body = mutation_input.body,
              path = mutation_input.path,
              line = 2,
              position = mutation_input.position,
              createdAt = "2026-06-13T10:11:12Z",
              updatedAt = "2026-06-13T10:11:12Z",
              author = { login = "me" },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    elseif key == "gh api graphql --input -" and input:find("updatePullRequestReviewComment", 1, true) then
      comment_updates[#comment_updates + 1] = { command = command, input = input }
      local stdout = vim.json.encode({
        data = {
          updatePullRequestReviewComment = {
            pullRequestReviewComment = {
              id = "PRRC_101",
              databaseId = 101,
              body = "Edited comment body",
              path = "src/a.txt",
              line = 2,
              position = 3,
              createdAt = "2026-06-13T10:12:12Z",
              updatedAt = "2026-06-13T10:12:12Z",
              author = { login = "me" },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
    elseif key == "gh api graphql --input -" and input:find("deletePullRequestReviewComment", 1, true) then
      comment_deletes[#comment_deletes + 1] = { command = command, input = input }
      cb({ code = 0, stdout = '{"data":{"deletePullRequestReviewComment":{"clientMutationId":null}}}', stderr = "", output = "" })
    elseif key:find("/reviews", 1, true) then
      review_calls[#review_calls + 1] = { command = command, input = input }
      if review_should_fail then
        cb({ code = 1, stdout = "", stderr = "mock review failure", output = "mock review failure" })
      else
        cb({ code = 0, stdout = '{"id": 7}', stderr = "", output = '{"id": 7}' })
      end
    else
      cb({ code = 1, stdout = "", stderr = "unexpected: " .. key, output = "unexpected: " .. key })
    end
  end, 3)
end

function gh_backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

---@type DiffReviewGitBackend
local git_backend = {}
function git_backend.systemlist() return {}, 1 end
function git_backend.systemlist_async(_, cb) vim.defer_fn(function() cb({}, 1) end, 3) end
function git_backend.system() return "", 1 end
function git_backend.system_async(_, _, cb) vim.defer_fn(function() cb({ code = 1, stdout = "", stderr = "", output = "" }) end, 3) end
function git_backend.delete() return 0 end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function set_datetime_now(value)
  local epoch = diff_review._datetime.parse(value)
  assert_true(type(epoch) == "number", "test datetime did not parse: " .. tostring(value))
  diff_review._datetime.now_override = function() return epoch end
end

local function lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function plain_winbar()
  return (vim.wo.winbar or ""):gsub("%%#[^#]+#", ""):gsub("%%%*", ""):gsub("%%=", " "):gsub("%%%%", "%%")
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function buffer_has_exact_line(buf, expected)
  for _, line in ipairs(lines(buf)) do
    if line == expected then return true end
  end
  return false
end

local function buffer_keymap(buf, mode, lhs)
  for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(buf, mode)) do
    if keymap.lhs == lhs then return keymap end
  end
  return nil
end

local function find_row(buf, needle)
  for index, line in ipairs(lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines(buf), "\n"), 2)
end

local function row_is_folded(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  return vim.fn.foldclosed(row) ~= -1
end

local function fold_text_at(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  local text = vim.fn.foldtextresult(row)
  if text ~= "" then return text end
  return vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
end

local function assert_cursor_clamped_to_line(buf, row, label)
  vim.api.nvim_win_set_buf(0, buf)
  local line = lines(buf)[row] or ""
  vim.fn.setpos(".", { 0, row, #line + 1, 40 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  local cursor = vim.api.nvim_win_get_cursor(0)
  local pos = vim.fn.getcurpos()
  assert_true(cursor[1] == row, label .. " cursor moved rows: " .. vim.inspect(cursor))
  assert_true(cursor[2] == #line, label .. " cursor was not clamped to line end: " .. vim.inspect({ cursor = cursor, line = line }))
  assert_true((pos[4] or 0) == 0, label .. " cursor kept virtual columns: " .. vim.inspect(pos))
end

local function row_has_line_highlight(buf, row, highlight_group)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
    local details = mark[4] or {}
    if details.line_hl_group == highlight_group then return true end
  end
  return false
end

local function row_after(buf, needle, after)
  local content = lines(buf)
  for index = after + 1, #content do
    if content[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. after .. ": " .. needle, 2)
end

local function start_command_capture()
  local original_cmd = vim.cmd
  local commands = {}
  vim.cmd = function(command)
    commands[#commands + 1] = command
    return original_cmd(command)
  end
  return commands, function()
    vim.cmd = original_cmd
  end
end

local function trigger(buf, key, row)
  vim.api.nvim_win_set_buf(0, buf)
  if row then
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  end
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

local function trigger_visual(buf, key, start_row, end_row)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { start_row, 0 })
  vim.cmd("normal! V")
  vim.api.nvim_win_set_cursor(0, { end_row, 0 })
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "x", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing visual buffer mapping for " .. key)
  mapping.callback()
end

local function set_inline_comment_body(buf, comment, text)
  vim.api.nvim_win_set_buf(0, buf)
  pcall(vim.cmd, "stopinsert")
  local start_row, end_row = diff_review._review.comment_body_rows(buf, comment)
  assert_true(start_row ~= nil and end_row ~= nil, "inline comment body rows missing")
  local body_lines = vim.split(text, "\n", { plain = true })
  vim.api.nvim_win_set_cursor(0, { start_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, start_row - 1, end_row, false, body_lines)
  vim.api.nvim_win_set_cursor(0, { start_row + #body_lines - 1, #(body_lines[#body_lines] or "") })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function() return comment.body == text end, "inline comment body edit did not save")
end

local function create_inline_comment_with_key(buf, row, label)
  vim.api.nvim_win_set_buf(0, buf)
  pcall(vim.cmd, "stopinsert")
  local state = diff_review._review.state(buf)
  local previous_count = state and #(state.review_comments or {}) or 0
  local popup_called = false
  diff_review._review.input_provider = function(_, _, _) popup_called = true end
  local commands, restore_commands = start_command_capture()
  trigger(buf, "C", row)
  local comment
  wait_for(function()
    state = diff_review._review.state(buf)
    local comments = state and state.review_comments or {}
    if #comments ~= previous_count + 1 then return false end
    comment = comments[#comments]
    if not comment or comment.body ~= "" then return false end
    local start_row = diff_review._review.comment_body_rows(buf, comment)
    return vim.api.nvim_get_current_buf() == buf and start_row and vim.api.nvim_win_get_cursor(0)[1] == start_row
  end, (label or "C") .. " did not create and focus an empty inline comment")
  assert_true(not popup_called, (label or "C") .. " opened the input popup")
  wait_for(function() return vim.tbl_contains(commands, "startinsert") end, (label or "C") .. " did not request insert mode")
  restore_commands()
  return comment
end

local pr = {
  number = 12,
  title = "Add the thing",
  body = "",
  url = "https://github.com/owner/repo/pull/12",
  repo = "owner/repo",
  headRefName = "feature",
  headRefOid = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  commits = {},
  files = {
    { path = "src/a.txt", additions = 1, deletions = 1 },
    { path = "src/b.txt", additions = 1, deletions = 1 },
  },
  changedFiles = 2,
  additions = 2,
  deletions = 2,
}

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  diff_review._review.set_data_dir_for_test(review_data_dir)
  set_datetime_now("2026-06-13T10:20:12Z")

  local buf = diff_review.open_review(pr, { cwd = "D:/diffreview-review-root" })
  assert_true(buf ~= nil, "open_review did not return a buffer")
  wait_for(function() return buffer_contains(buf, "NEW src/a.txt") end, "review diff did not render")
  assert_true(not vim.wo[0].wrap, "review buffer should reuse GitStatus no-wrap formatting")
  assert_true(not vim.wo[0].linebreak, "review buffer should not enable linebreak")
  assert_true(not vim.wo[0].breakindent, "review buffer should not enable breakindent")
  assert_true(vim.wo[0].conceallevel == 0, "review buffer should not conceal diff code rows")
  assert_true(vim.wo[0].fillchars:find("fold: ", 1, true) ~= nil, "review buffer should not draw fold filler dots")
  assert_true(vim.wo[0].winhighlight:find("Folded:Normal", 1, true) ~= nil, "review buffer should not recolor folded rows")

  -- ── layout + hint shows the review commands ────────────────────────────────
  assert_true(buffer_contains(buf, "Title: Add the thing"), "title missing")
  assert_true(buffer_contains(buf, "Review Comment:"), "review comment label missing")
  assert_cursor_clamped_to_line(buf, find_row(buf, "Title: Add the thing"), "review title")
  assert_cursor_clamped_to_line(buf, find_row(buf, "Review Comment:"), "review comment label")
  assert_true(buffer_contains(buf, "Unviewed Changes (2)"), "unviewed section missing both files")
  assert_true(buffer_contains(buf, "Viewed Changes (0)"), "viewed section missing")
  local hint = plain_winbar()
  assert_true(hint:find("Review #12", 1, true) ~= nil, "hint missing review title: " .. hint)
  for _, token in ipairs({ "<Tab> toggle", "N Collapse Parent", "S viewed", "U unviewed", "C comment", "J delete", "<C-s> sync", "cc submit", "q close", "? help" }) do
    assert_true(hint:find(token, 1, true) ~= nil, "hint missing " .. token .. ": " .. hint)
  end
  for _, token in ipairs({ "y next", "z prev", "b browse" }) do
    assert_true(hint:find(token, 1, true) == nil, "hint should not pin " .. token .. ": " .. hint)
  end
  assert_true(not buffer_contains(buf, "Hint:"), "hint should be a sticky winbar, not buffer text")

  -- ── ? is generated from the same command model as the Hint line ───────────
  trigger(buf, "?")
  local help_buf = vim.api.nvim_get_current_buf()
  local function help_has(key, desc)
    for _, line in ipairs(lines(help_buf)) do
      if line:find(key, 1, true) and line:find(desc, 1, true) then return true end
    end
    return false
  end
  for _, command in ipairs({
    { "<Tab>", "Toggle fold" },
    { "N", "Collapse Parent" },
    { "S", "Mark file as viewed" },
    { "U", "Move file back to unviewed" },
    { "C", "Add or edit an inline comment" },
    { "J", "Delete draft comment" },
    { "y", "Jump to next draft comment" },
    { "z", "Jump to previous draft comment" },
    { "<C-s>", "Sync inline comments to GitHub" },
    { "cc", "Submit review to GitHub" },
    { "b", "Browse pull request" },
    { "o, <CR>", "Open PR/about or jump to file" },
    { "R", "Refresh DiffReview" },
    { "q", "Close DiffReview" },
    { "?", "Show help" },
  }) do
    assert_true(help_has(command[1], command[2]), "help missing " .. command[1] .. " " .. command[2])
  end
  pcall(vim.api.nvim_win_close, 0, true)
  vim.api.nvim_set_current_buf(buf)

  -- ── Unviewed/Viewed sections use the shared GitStatus fold behavior ────────
  trigger(buf, "<Tab>", find_row(buf, "Unviewed Changes (2)"))
  wait_for(function() return row_is_folded(buf, find_row(buf, "src/a.txt +1 -1")) end, "Unviewed section did not fold")
  assert_true(row_is_folded(buf, find_row(buf, "src/b.txt +1 -1")), "folded Unviewed section still showed b.txt")
  trigger(buf, "<Tab>", find_row(buf, "Unviewed Changes (2)"))
  wait_for(function() return not row_is_folded(buf, find_row(buf, "src/a.txt +1 -1")) end, "Unviewed section did not unfold")

  -- ── S/U move section headers and files between sections ────────────────────
  trigger(buf, "S", find_row(buf, "Unviewed Changes (2)"))
  wait_for(function() return buffer_contains(buf, "Unviewed Changes (0)") end, "S on Unviewed header did not move all files")
  assert_true(buffer_contains(buf, "Viewed Changes (2)"), "S on Unviewed header did not fill Viewed")
  trigger(buf, "U", find_row(buf, "Viewed Changes (2)"))
  wait_for(function() return buffer_contains(buf, "Unviewed Changes (2)") end, "U on Viewed header did not move all files")
  assert_true(buffer_contains(buf, "Viewed Changes (0)"), "U on Viewed header did not clear Viewed")

  trigger(buf, "S", find_row(buf, "src/a.txt +1 -1"))
  wait_for(function() return buffer_contains(buf, "Unviewed Changes (1)") end, "S did not move a.txt to viewed")
  assert_true(buffer_contains(buf, "Viewed Changes (1)"), "viewed count did not increase")
  trigger(buf, "<Tab>", find_row(buf, "Viewed Changes (1)"))
  wait_for(function() return row_is_folded(buf, row_after(buf, "src/a.txt +1 -1", find_row(buf, "Viewed Changes"))) end, "Viewed section did not fold")
  assert_true(not row_is_folded(buf, find_row(buf, "src/b.txt +1 -1")), "folding Viewed hid the Unviewed file")
  trigger(buf, "<Tab>", find_row(buf, "Viewed Changes (1)"))
  wait_for(function()
    return not row_is_folded(buf, row_after(buf, "src/a.txt +1 -1", find_row(buf, "Viewed Changes")))
  end, "Viewed section did not unfold")
  local persisted_buf = diff_review.open_review(pr, { cwd = "D:/diffreview-review-root" })
  assert_true(persisted_buf ~= nil, "persisted review did not open")
  wait_for(function() return buffer_contains(persisted_buf, "Viewed Changes (1)") end, "viewed hunk did not persist")
  assert_true(
    row_after(persisted_buf, "src/a.txt +1 -1", find_row(persisted_buf, "Viewed Changes")) ~= nil,
    "same PR did not reopen with a.txt viewed"
  )
  local updated_pr = vim.deepcopy(pr)
  updated_pr.headRefOid = "feedfacefeedfacefeedfacefeedfacefeedface"
  pr_diff_text = file_diff("src/a.txt", "NEWER") .. "\n" .. file_diff("src/b.txt")
  local updated_buf = diff_review.open_review(updated_pr, { cwd = "D:/diffreview-review-root" })
  assert_true(updated_buf ~= nil, "updated review did not open")
  wait_for(function() return buffer_contains(updated_buf, "NEWER src/a.txt") end, "updated PR diff did not render")
  assert_true(buffer_contains(updated_buf, "Unviewed Changes (2)"), "changed viewed hunk did not become unviewed")
  assert_true(buffer_contains(updated_buf, "Viewed Changes (0)"), "changed viewed hunk remained viewed")
  pr_diff_text = file_diff("src/a.txt") .. "\n" .. file_diff("src/b.txt")
  vim.api.nvim_win_set_buf(0, buf)
  trigger(buf, "U", row_after(buf, "src/a.txt +1 -1", find_row(buf, "Viewed Changes")))
  wait_for(function() return buffer_contains(buf, "Unviewed Changes (2)") end, "U did not move a.txt back")

  -- ── editable review summary ────────────────────────────────────────────────
  local comment_label = find_row(buf, "Review Comment:")
  assert_cursor_clamped_to_line(buf, comment_label + 1, "empty review summary")
  vim.api.nvim_win_set_cursor(0, { comment_label + 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "review comment line must be editable")
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, comment_label, comment_label + 1, false, { "Looks good overall" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  assert_true(diff_review._review.state(buf).review_comment_text == "Looks good overall", "summary text not captured")
  assert_cursor_clamped_to_line(buf, comment_label + 1, "review summary text")
  assert_true(buffer_keymap(buf, "n", "q") == nil, "q command map must be removed in editable review text")
  local global_q_count = 0
  vim.keymap.set("n", "q", function()
    global_q_count = global_q_count + 1
  end, { silent = true })
  local global_q_ok, global_q_err = pcall(function()
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("q", true, false, true), "x", false)
    wait_for(function() return global_q_count == 1 end, "global q mapping did not run in editable review text")
    assert_true(vim.api.nvim_buf_is_valid(buf), "q closed the review buffer from editable review text")
  end)
  pcall(vim.keymap.del, "n", "q")
  if not global_q_ok then error(global_q_err) end

  assert_true(buffer_keymap(buf, "n", "cc") == nil, "cc command map must be removed in editable review text")
  local global_cc_count = 0
  vim.keymap.set("n", "cc", function()
    global_cc_count = global_cc_count + 1
  end, { silent = true })
  local global_cc_ok, global_cc_err = pcall(function()
    local review_call_count = #review_calls
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("cc", true, false, true), "x", false)
    wait_for(function() return global_cc_count == 1 end, "global cc mapping did not run in editable review text")
    assert_true(#review_calls == review_call_count, "cc submitted the review from editable review text")
  end)
  pcall(vim.keymap.del, "n", "cc")
  if not global_cc_ok then error(global_cc_err) end

  -- ── real comment input <C-s> saves and returns to normal mode ──────────────
  local popup_comment
  local create_commands, restore_create_commands = start_command_capture()
  diff_review._review.open_input("Comment", function(text) popup_comment = text end)
  local popup_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return vim.tbl_contains(create_commands, "startinsert") end, "new comment popup did not request insert mode")
  restore_create_commands()
  local popup_q_map = vim.api.nvim_buf_call(popup_buf, function()
    return vim.fn.maparg("q", "n", false, true)
  end)
  assert_true(popup_q_map.buffer ~= 1, "comment popup must not bind q to cancel")
  local popup_ctrl_q_map = vim.api.nvim_buf_call(popup_buf, function()
    return vim.fn.maparg("<C-q>", "i", false, true)
  end)
  assert_true(popup_ctrl_q_map.buffer == 1, "comment popup must bind Ctrl-q to cancel")
  vim.api.nvim_buf_set_lines(popup_buf, 0, -1, false, { "Popup comment" })
  vim.cmd("startinsert")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return popup_comment == "Popup comment" end, "popup <C-s> did not submit the comment")
  wait_for(function() return vim.fn.mode() == "n" end, "popup <C-s> did not return to normal mode")

  local edited_popup_comment
  local edit_commands, restore_edit_commands = start_command_capture()
  diff_review._review.open_input("Edit comment", function(text) edited_popup_comment = text end, "Existing popup comment")
  wait_for(function() return vim.tbl_contains(edit_commands, "startinsert") end, "edit comment popup did not request insert mode")
  restore_edit_commands()
  popup_buf = vim.api.nvim_get_current_buf()
  assert_true(lines(popup_buf)[1] == "Existing popup comment", "edit popup did not prefill the existing comment")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return edited_popup_comment == "Existing popup comment" end, "edit popup <C-s> did not submit")
  wait_for(function() return vim.fn.mode() == "n" end, "edit popup <C-s> did not keep normal mode")

  local multiline_popup_comment
  local multiline_commands, restore_multiline_commands = start_command_capture()
  diff_review._review.open_input("Edit comment", function(text) multiline_popup_comment = text end, "one\ntwo\nthree")
  wait_for(function() return vim.tbl_contains(multiline_commands, "startinsert") end, "multiline edit popup did not request insert mode")
  restore_multiline_commands()
  popup_buf = vim.api.nvim_get_current_buf()
  assert_true(vim.api.nvim_win_get_height(0) == 3, "multiline edit popup did not expand to content height")
  assert_true(lines(popup_buf)[3] == "three", "multiline edit popup did not prefill all content")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return multiline_popup_comment == "one\ntwo\nthree" end, "multiline edit popup <C-s> did not submit")
  vim.api.nvim_win_set_buf(0, buf)

  -- ── C off a changed line is a no-op ────────────────────────────────────────
  local input_called = false
  diff_review._review.input_provider = function(_, _, _) input_called = true end
  trigger(buf, "C", find_row(buf, "Title:"))
  vim.wait(40, function() return false end, 10)
  assert_true(not input_called, "C on a non-diff line must not open the input")

  -- ── C on a changed line drafts locally; <C-s> syncs to GitHub ──────────────
  local first_comment = create_inline_comment_with_key(buf, find_row(buf, "NEW src/a.txt"), "C on a changed line")
  assert_true(not buffer_contains(buf, "*you commented"), "empty inline comment should not show a dirty marker")
  set_inline_comment_body(buf, first_comment, "This rename needs a test")
  wait_for(function() return buffer_contains(buf, "This rename needs a test") end, "inline comment not rendered as real lines")
  assert_true(#pending_review_creates == 0, "drafting a comment created a pending review before manual sync")
  assert_true(#comment_creates == 0, "drafting a comment synced before manual sync")
  assert_true(#review_calls == 0, "drafting a comment must not submit the review")
  assert_true(buffer_contains(buf, " L2"), "inline comment header missing")
  assert_true(buffer_contains(buf, diff_review._review.comment_icon .. " *you commented"), "inline comment header missing comment icon")
  assert_true(buffer_contains(buf, "*you commented"), "dirty comment header missing * marker")
  assert_true(buffer_contains(buf, "commented"), "inline comment author/action header missing")
  -- the comment lines are real and below the anchor line
  assert_true(
    find_row(buf, "This rename needs a test") > find_row(buf, "NEW src/a.txt"),
    "comment must render below its anchor line"
  )
  trigger(buf, "<C-s>")
  wait_for(function() return not buffer_contains(buf, "*you commented") end, "manual sync did not clear dirty marker immediately")
  wait_for(function() return #pending_review_creates == 1 end, "manual sync did not create a pending review")
  wait_for(function() return #comment_creates == 1 end, "manual sync did not create the pending review comment")
  assert_true(comment_creates[1].command[3] == "graphql", "pending review comment create did not use GraphQL")
  assert_true(comment_creates[1].payload.pullRequestReviewId == "PRR_7", "pending review comment create used the wrong review node id")
  assert_true(comment_creates[1].payload.position == 3, "pending review comment create did not send diff position")
  assert_true(comment_creates[1].payload.commitOID == pr.headRefOid, "pending review comment create did not target the PR head commit")

  -- ── cursor can land on the comment; y jumps to it ──────────────────────────
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  trigger(buf, "y")
  local landed = vim.api.nvim_win_get_cursor(0)[1]
  assert_true((lines(buf)[landed] or ""):match("%sL2$") ~= nil, "y did not jump to the comment")
  local comment_obj = diff_review._review.comment_under_cursor(buf)
  assert_true(comment_obj ~= nil and comment_obj.body == "This rename needs a test", "cursor not recognized as on the comment")

  -- ── inline comments render as sized header/body/footer rows ───────────────
  local comment_header_row = find_row(buf, " L2")
  local comment_body_row = find_row(buf, "This rename needs a test")
  local comment_footer_row = comment_body_row + 1
  vim.api.nvim_win_set_cursor(0, { comment_header_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  local clamped_cursor = vim.api.nvim_win_get_cursor(0)
  local body_start_col, body_end_col = diff_review._review.comment_body_text_bounds(lines(buf)[comment_body_row])
  assert_true(clamped_cursor[1] == comment_header_row, "cursor should be allowed on inline comment header")
  assert_true(not vim.bo[buf].modifiable, "inline comment header must stay read-only")
  local header_line = lines(buf)[comment_header_row] or ""
  local footer_line = lines(buf)[comment_footer_row] or ""
  local rule_width = diff_review._review.comment_rule_width()
  assert_true(header_line:find(diff_review._review.comment_icon, 1, true) == 1, "comment header must start with the comment icon")
  assert_true(not header_line:match("^%-%-"), "comment header must not start with outer rule markers")
  assert_true(not header_line:match("%-%-$"), "comment header must not end with outer rule markers")
  assert_true(header_line:match("%sL%d+$") ~= nil, "comment header must end with the right-aligned line number")
  assert_true(footer_line:match("^%-%-+$"), "comment footer must render a plain rule")
  assert_true(vim.fn.strdisplaywidth(header_line) <= rule_width, "comment header rule exceeded text width")
  assert_true(vim.fn.strdisplaywidth(footer_line) <= rule_width, "comment footer rule exceeded text width")
  local review_win = vim.api.nvim_get_current_win()
  local narrow_win = vim.api.nvim_open_win(buf, false, {
    relative = "editor",
    row = 0,
    col = 0,
    width = 48,
    height = 4,
    style = "minimal",
  })
  diff_review._review.refresh_inline_comment_rules(buf, narrow_win)
  header_line = lines(buf)[comment_header_row] or ""
  footer_line = lines(buf)[comment_footer_row] or ""
  local narrow_rule_width = diff_review._review.comment_rule_width(narrow_win, buf)
  assert_true(vim.fn.strdisplaywidth(header_line) <= narrow_rule_width, "resized comment header rule exceeded text width")
  assert_true(vim.fn.strdisplaywidth(footer_line) <= narrow_rule_width, "resized comment footer rule exceeded text width")
  vim.api.nvim_win_close(narrow_win, true)
  vim.api.nvim_set_current_win(review_win)
  diff_review._review.refresh_inline_comment_rules(buf, review_win)
  local body_line = lines(buf)[comment_body_row]
  assert_true(body_line == "This rename needs a test", "inline comment body must be raw text")
  assert_true(body_start_col == 0, "inline comment body should start at column 0")
  assert_true(body_end_col == #body_line, "inline comment body bounds should cover the raw line")
  vim.api.nvim_win_set_cursor(0, { comment_body_row, #body_line })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(vim.bo[buf].modifiable, "inline comment body must become editable on hover")
  assert_true(clamped_cursor[1] == comment_body_row, "cursor left comment body")
  assert_true(clamped_cursor[2] == body_end_col, "cursor should stay one column after the final character")
  assert_cursor_clamped_to_line(buf, comment_body_row, "inline comment body")
  assert_true(buffer_keymap(buf, "n", ";") == nil, "review must not hard-code a column-zero mapping for inline comments")
  assert_true(buffer_keymap(buf, "n", "/") == nil, "review must not hard-code a max-column mapping for inline comments")
  assert_true(
    diff_review._review.comment_body_at_row(buf, comment_footer_row) ~= comment_obj,
    "inline comment must not include a footer row in the editable body"
  )
  vim.api.nvim_win_set_cursor(0, { comment_footer_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(not vim.bo[buf].modifiable, "inline comment footer must stay read-only")

  local empty_state = diff_review._review.state(buf)
  local empty_original_body = empty_state.review_comments[1].body
  empty_state.review_comments[1].body = ""
  diff_review._review.render(buf)
  comment_header_row = find_row(buf, " L2")
  local empty_start_row, empty_end_row = diff_review._review.comment_body_rows(buf, empty_state.review_comments[1])
  assert_true(empty_start_row == empty_end_row, "empty inline comment should render one editable body row")
  local empty_body_line = lines(buf)[empty_start_row]
  local empty_start_col, empty_end_col = diff_review._review.comment_body_text_bounds(empty_body_line)
  assert_true(empty_body_line == "", "empty inline comment body should be a blank editable row")
  assert_true(empty_start_col == 0, "empty inline comment text should start at column 0")
  assert_true(empty_start_col == empty_end_col, "empty inline comment bounds should collapse")
  vim.api.nvim_win_set_cursor(0, { empty_start_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == empty_start_row, "empty inline comment cursor left the body row")
  assert_true(clamped_cursor[2] == empty_start_col, "empty inline comment cursor should stay at column 0")
  empty_state.review_comments[1].body = empty_original_body
  diff_review._review.render(buf)
  comment_header_row = find_row(buf, " L2")
  comment_body_row = find_row(buf, "This rename needs a test")

  -- ── C on an existing comment focuses inline text instead of opening popup ──
  diff_review._review.input_provider = nil
  trigger(buf, "C", comment_header_row)
  wait_for(function() return vim.api.nvim_get_current_buf() == buf end, "C on comment opened a popup buffer")
  assert_true(buffer_contains(buf, "This rename needs a test"), "C on comment hid inline comment text")
  local focused_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(focused_cursor[1] == comment_body_row, "C on comment did not focus the inline body")

  vim.api.nvim_buf_set_lines(buf, comment_body_row - 1, comment_body_row, false, { "Edited comment body" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == "Edited comment body"
  end, "inline comment edit did not update local state")
  assert_true(#comment_updates == 0, "inline comment edit synced before manual sync")
  assert_true(buffer_contains(buf, "*me commented"), "inline comment edit did not mark the header dirty")
  assert_true(
    row_has_line_highlight(buf, find_row(buf, "*me commented"), "DiffReviewReviewCommentHeader"),
    "dirty inline comment header lost its review-header highlight"
  )
  trigger(buf, "<C-s>")
  wait_for(function() return not buffer_contains(buf, "*me commented") end, "manual sync did not clear dirty marker after inline edit")
  wait_for(function() return #comment_updates >= 1 end, "manual sync did not update the edited comment")
  assert_true(vim.api.nvim_get_current_buf() == buf, "inline comment edit switched buffers")
  assert_true(buffer_contains(buf, "Edited comment body"), "inline comment edit changed state but not visible text")
  assert_true(not buffer_contains(buf, "This rename needs a test"), "old comment text still present after inline edit")

  -- ── <Tab> folds/unfolds the comment under the cursor ──────────────────────
  local folded_comment = diff_review._review.state(buf).review_comments[1]
  trigger(buf, "<Tab>", find_row(buf, "Edited comment body"))
  wait_for(function() return folded_comment.review_folded == true end, "<Tab> on comment body did not fold the comment")
  local folded_row = diff_review._review.comment_header_row0(buf, folded_comment) + 1
  local folded_body_row = diff_review._review.comment_body_rows(buf, folded_comment)
  local folded_line = fold_text_at(buf, folded_row)
  assert_true(
    folded_line:find(diff_review._review.comment_icon .. " me commented", 1, true) == 1,
    "folded comment did not start with icon/user/date: " .. folded_line
  )
  assert_true(
    folded_line:find(" | Edited comment body", 1, true) ~= nil,
    "folded comment did not include preview text: " .. folded_line
  )
  assert_true(buffer_has_exact_line(buf, "Edited comment body"), "folded comment lost the editable body row")
  assert_true(folded_body_row ~= nil and row_is_folded(buf, folded_body_row), "folded comment body row stayed visible")
  assert_true(not vim.bo[buf].modifiable, "folded comment preview must stay read-only")

  trigger(buf, "<Tab>", folded_row)
  wait_for(function()
    local start_row = diff_review._review.comment_body_rows(buf, folded_comment)
    return folded_comment.review_folded ~= true
      and start_row ~= nil
      and not row_is_folded(buf, start_row)
      and buffer_has_exact_line(buf, "Edited comment body")
  end, "<Tab> on folded comment did not unfold it")

  trigger(buf, "<Tab>", find_row(buf, "Edited comment body"))
  wait_for(function() return folded_comment.review_folded == true end, "<Tab> on editable comment body did not stay mapped")
  trigger(buf, "<Tab>", vim.api.nvim_win_get_cursor(0)[1])
  wait_for(function()
    local start_row = diff_review._review.comment_body_rows(buf, folded_comment)
    return folded_comment.review_folded ~= true
      and start_row ~= nil
      and not row_is_folded(buf, start_row)
      and buffer_has_exact_line(buf, "Edited comment body")
  end, "second folded comment toggle did not restore the body")

  trigger(buf, "<Tab>", find_row(buf, " L2"))
  wait_for(function() return folded_comment.review_folded == true end, "<Tab> on comment header did not fold the comment")
  trigger(buf, "C", vim.api.nvim_win_get_cursor(0)[1])
  wait_for(function()
    local start_row = diff_review._review.comment_body_rows(buf, folded_comment)
    return folded_comment.review_folded ~= true
      and start_row ~= nil
      and not row_is_folded(buf, start_row)
      and vim.api.nvim_win_get_cursor(0)[1] == start_row
  end, "C on folded comment did not unfold and focus the body")

  -- ── visual deletion edits inline without opening a popup ───────────────────
  local deletion_state = diff_review._review.state(buf)
  local deletion_original_body = deletion_state.review_comments[1].body
  deletion_state.review_comments[1].body = "This is not the correct that, This is not the correct answer"
  diff_review._review.render(buf)
  local deletion_row = find_row(buf, "This is not the correct")
  local deletion_line = lines(buf)[deletion_row]
  local deletion_edited_line = deletion_line:gsub("not", "", 1)
  local deletion_expected_text = "This is  the correct that, This is not the correct answer"
  vim.api.nvim_win_set_cursor(0, { deletion_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, deletion_row - 1, deletion_row, false, { deletion_edited_line })
  vim.api.nvim_win_set_cursor(0, { deletion_row, deletion_edited_line:find("the correct", 1, true) - 1 })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function() return deletion_state.review_comments[1].body == deletion_expected_text end, "visual deletion did not update inline body")
  vim.wait(40, function() return false end, 10)
  assert_true(vim.api.nvim_get_current_buf() == buf, "visual deletion opened an edit popup")
  assert_true(
    diff_review._review.comment_body_text_from_line(lines(buf)[deletion_row]) == deletion_expected_text,
    "visual deletion inserted an extra character: " .. tostring(lines(buf)[deletion_row])
  )
  assert_true(
    vim.api.nvim_win_get_cursor(0)[2]
      == diff_review._review.comment_body_text_bounds(lines(buf)[deletion_row])
        + deletion_expected_text:find("the correct", 1, true) - 1,
    "visual deletion cursor did not stay before the following word"
  )
  deletion_state.review_comments[1].body = deletion_original_body
  diff_review._review.render(buf)

  -- ── native edits on comment bodies update local state; <C-s> syncs them ───
  local direct_edit_row = find_row(buf, "Edited comment body")
  vim.api.nvim_win_set_cursor(0, { direct_edit_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "rendered inline comment body must become editable on hover")
  vim.api.nvim_buf_set_lines(buf, direct_edit_row - 1, direct_edit_row, false, { "Direct edited comment body" })
  vim.api.nvim_win_set_cursor(0, { direct_edit_row, 7 })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == "Direct edited comment body"
  end, "inline edit did not save immediately")
  assert_true(vim.api.nvim_get_current_buf() == buf, "inline edit opened a popup")
  local direct_cursor_line = lines(buf)[direct_edit_row]
  local direct_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, direct_edit_row, direct_cursor_line)
  assert_true(vim.api.nvim_win_get_cursor(0)[2] == direct_start_col + 7, "inline edit did not preserve the edited position")
  assert_true(#comment_updates == 1, "shortcut comment edit synced before manual sync")
  assert_true(buffer_contains(buf, "*me commented"), "shortcut comment edit did not mark the header dirty")
  trigger(buf, "<C-s>")
  wait_for(function() return #comment_updates >= 2 end, "manual sync did not sync shortcut comment edit")
  wait_for(function() return not buffer_contains(buf, "*me commented") end, "shortcut comment sync did not clear dirty marker")

  local normal_change_mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("C", "n", false, true)
  end)
  assert_true(
    normal_change_mapping.buffer ~= 1,
    "C command map must be removed while the cursor is in an inline comment body"
  )
  local second_edit_row = find_row(buf, "Direct edited comment body")
  vim.api.nvim_win_set_cursor(0, { second_edit_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, second_edit_row - 1, second_edit_row, false, { "Visual changed comment body" })
  vim.api.nvim_win_set_cursor(0, { second_edit_row, 6 })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == "Visual changed comment body"
  end, "second inline edit did not save immediately")
  assert_true(vim.api.nvim_get_current_buf() == buf, "second inline edit opened a popup")
  local second_cursor_line = lines(buf)[second_edit_row]
  local second_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, second_edit_row, second_cursor_line)
  assert_true(vim.api.nvim_win_get_cursor(0)[2] == second_start_col + 6, "second inline edit did not preserve the edited position")
  assert_true(#comment_updates == 2, "visual change comment edit synced before manual sync")
  assert_true(buffer_contains(buf, "*me commented"), "visual change comment edit did not mark the header dirty")
  trigger(buf, "<C-s>")
  wait_for(function() return #comment_updates >= 3 end, "manual sync did not sync visual change comment edit")
  wait_for(function() return not buffer_contains(buf, "*me commented") end, "visual change comment sync did not clear dirty marker")

  assert_true(
    diff_review._review.comment_body_text_from_line("  Alpha ") == "  Alpha ",
    "inline comment body readback must preserve raw leading and trailing spaces"
  )
  local space_state = diff_review._review.state(buf)
  space_state.review_comments[1].body = "AlphaBeta"
  diff_review._review.render(buf)
  local space_row = find_row(buf, "AlphaBeta")
  local space_line = lines(buf)[space_row]
  local space_start_col = diff_review._review.comment_body_text_bounds(space_line)
  local prefix = space_line:sub(1, space_start_col)
  vim.api.nvim_win_set_cursor(0, { space_row, space_start_col + #"Alpha" })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, space_row - 1, space_row, false, { prefix .. "Alpha " })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  assert_true(
    vim.wait(3000, function()
      return diff_review._review.state(buf).review_comments[1].body == "Alpha "
    end, 10),
    "trailing space inserted in inline comment was not preserved; body="
      .. tostring(diff_review._review.state(buf).review_comments[1].body)
      .. " line="
      .. tostring(lines(buf)[space_row])
      .. " cursor="
      .. vim.inspect(vim.api.nvim_win_get_cursor(0))
  )
  local spaced_body_row = find_row(buf, "Alpha")
  local spaced_body_line = lines(buf)[spaced_body_row]
  assert_true(
    diff_review._review.comment_body_text_from_line(spaced_body_line) == "Alpha ",
    "trailing space inserted in inline comment was stripped or moved: " .. tostring(spaced_body_line)
  )
  vim.api.nvim_buf_set_lines(buf, spaced_body_row - 1, spaced_body_row, false, { prefix .. "Alpha B" })
  vim.api.nvim_win_set_cursor(0, { spaced_body_row, space_start_col + #"Alpha B" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == "Alpha B"
  end, "typing after a preserved inline comment space did not keep the separator")
  space_state.review_comments[1].body = "Visual changed comment body"
  diff_review._review.render(buf)

  local open_line_row = find_row(buf, "Visual changed comment body")
  local has_buffer_open_above_map = false
  for _, keymap in ipairs(vim.api.nvim_buf_get_keymap(buf, "n")) do
    if keymap.lhs == "O" then has_buffer_open_above_map = true end
  end
  assert_true(not has_buffer_open_above_map, "review must not hard-code an O mapping for inline comments")
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { open_line_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(buffer_keymap(buf, "n", "o") == nil, "o command map must be removed in inline comment text")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("o", true, false, true), "x", false)
  assert_true(
    vim.wait(3000, function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      return cursor[1] == open_line_row + 1
    end, 10),
    "o did not pass through to native insert-line-below behavior"
  )
  vim.api.nvim_exec_autocmds("CursorMovedI", { buffer = buf })
  local opened_cursor = vim.api.nvim_win_get_cursor(0)
  local opened_line = lines(buf)[opened_cursor[1]]
  local opened_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, opened_cursor[1], opened_line)
  assert_true(opened_cursor[1] == open_line_row + 1, "o did not create a line below the inline comment row")
  assert_true(opened_cursor[2] == opened_start_col, "o did not place the cursor in the editable comment text cell")
  assert_true(opened_start_col == 0, "new inline comment body row should start at column 0")
  assert_true(opened_line:find("│", 1, true) == nil, "new inline comment body row exposed a real border")
  assert_true(
    (lines(buf)[opened_cursor[1] - 1] or ""):find("Visual changed comment body", 1, true) ~= nil,
    "o did not leave the original comment text above"
  )
  vim.api.nvim_buf_set_lines(
    buf,
    opened_cursor[1] - 1,
    opened_cursor[1],
    false,
    { (" "):rep(opened_start_col) .. "Inserted below" }
  )
  vim.api.nvim_win_set_cursor(0, { opened_cursor[1], opened_start_col + #"Inserted below" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  local expected_open_line_body = "Visual changed comment body\nInserted below"
  assert_true(
    vim.wait(3000, function()
      return diff_review._review.state(buf).review_comments[1].body == expected_open_line_body
    end, 10),
    "o-created non-empty visual line did not save as typed: "
      .. tostring(diff_review._review.state(buf).review_comments[1].body)
  )
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  vim.wait(40, function() return false end, 10)
  assert_true(
    diff_review._review.state(buf).review_comments[1].body == expected_open_line_body,
    "o-created non-empty visual line changed after rerender"
  )
  pcall(vim.cmd, "stopinsert")

  space_state.review_comments[1].body = "First paragraph\n\nLast paragraph"
  diff_review._review.render(buf)
  local last_open_row = find_row(buf, "Last paragraph")
  local _, last_open_end_row = diff_review._review.comment_body_rows(buf, space_state.review_comments[1])
  assert_true(last_open_row == last_open_end_row, "test setup did not place the cursor on the final comment body row")
  vim.api.nvim_win_set_cursor(0, { last_open_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(buffer_keymap(buf, "n", "o") == nil, "o command map must be removed on the final comment body row")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("o", true, false, true), "x", false)
  wait_for(function()
    return vim.api.nvim_win_get_cursor(0)[1] == last_open_row + 1
  end, "o on the final inline comment row did not create a line below it")
  vim.api.nvim_exec_autocmds("CursorMovedI", { buffer = buf })
  local last_open_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(
    last_open_cursor[1] == last_open_row + 1,
    "cursor moved out of the comment after opening below the final body row: " .. vim.inspect(last_open_cursor)
  )
  assert_true(
    diff_review._review.comment_body_at_row(buf, last_open_cursor[1]) == space_state.review_comments[1],
    "newly opened final-row line was not treated as editable comment body"
  )
  local last_open_line = lines(buf)[last_open_cursor[1]]
  local last_open_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, last_open_cursor[1], last_open_line)
  assert_true(last_open_cursor[2] == last_open_start_col, "final-row inserted body cursor stayed in the gutter")
  assert_true(last_open_start_col == 0, "final-row inserted body line should start at column 0")
  assert_true(last_open_line:find("│", 1, true) == nil, "final-row inserted body line exposed a real border")
  vim.api.nvim_buf_set_lines(
    buf,
    last_open_cursor[1] - 1,
    last_open_cursor[1],
    false,
    { (" "):rep(last_open_start_col) .. "Inserted at end" }
  )
  vim.api.nvim_win_set_cursor(0, { last_open_cursor[1], last_open_start_col + #"Inserted at end" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  wait_for(function()
    return space_state.review_comments[1].body == "First paragraph\n\nLast paragraph\nInserted at end"
  end, "final-row open-line edit did not save back into the comment body")
  pcall(vim.cmd, "stopinsert")

  space_state.review_comments[1].body = "Visual changed comment body"
  diff_review._review.render(buf)
  local paragraph_row = find_row(buf, "Visual changed comment body")
  local paragraph_line = lines(buf)[paragraph_row]
  local paragraph_start_col = diff_review._review.comment_body_text_bounds(paragraph_line)
  vim.api.nvim_win_set_cursor(0, { paragraph_row, paragraph_start_col })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, paragraph_row, paragraph_row, false, {
    (" "):rep(paragraph_start_col),
    (" "):rep(paragraph_start_col) .. "Test me quickly.",
  })
  vim.api.nvim_win_set_cursor(0, { paragraph_row + 2, paragraph_start_col + #"Test me quickly." })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  local expected_paragraph_body = "Visual changed comment body\n\nTest me quickly."
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == expected_paragraph_body
  end, "empty inline row did not create a paragraph break")
  diff_review._review.render(buf)
  local paragraph_first_row = find_row(buf, "Visual changed comment body")
  local paragraph_second_row = find_row(buf, "Test me quickly.")
  assert_true(paragraph_second_row > paragraph_first_row + 1, "paragraph break did not render as an empty row")
  local paragraph_blank_line = lines(buf)[paragraph_first_row + 1]
  local paragraph_blank_start_col = diff_review._review.comment_body_text_bounds(paragraph_blank_line)
  assert_true(
    diff_review._review.comment_body_text_from_line(paragraph_blank_line, paragraph_blank_start_col) == "",
    "paragraph break row rendered non-empty text: " .. tostring(paragraph_blank_line)
  )

  diff_review._review.state(buf).review_comments[1].body = "Visual changed comment body"
  diff_review._review.render(buf)
  local code_row = find_row(buf, "Visual changed comment body")
  local code_lines = {
    "export interface AzureAdConfig {",
    "  tenantId: string;",
    "  clientId: string;",
    "}",
  }
  vim.api.nvim_win_set_cursor(0, { code_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, code_row - 1, code_row, false, code_lines)
  vim.api.nvim_win_set_cursor(0, { code_row + #code_lines - 1, #code_lines[#code_lines] })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  local expected_code_body = table.concat(code_lines, "\n")
  assert_true(
    vim.wait(3000, function()
      return diff_review._review.state(buf).review_comments[1].body == expected_code_body
    end, 10),
    "pasted code block was not preserved as raw body lines; body="
      .. vim.inspect(diff_review._review.state(buf).review_comments[1].body)
  )
  diff_review._review.render(buf)
  assert_true(not buffer_contains(buf, "```"), "raw code paste should not add markdown fences")
  assert_true(buffer_contains(buf, "  tenantId: string;"), "raw code paste did not preserve indentation")

  diff_review._review.state(buf).review_comments[1].body = "Visual changed comment body"
  diff_review._review.render(buf)

  local state = diff_review._review.state(buf)
  local original_body = state.review_comments[1].body
  local reflow_text =
    "This is next the time for this FOOBAR thing that, thas is not the time for which the only time know this thing whiche we know.test the"
  local reflow_row = find_row(buf, "Visual changed comment body")
  vim.api.nvim_win_set_cursor(0, { reflow_row, 12 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, reflow_row - 1, reflow_row, false, { reflow_text })
  vim.api.nvim_win_set_cursor(0, { reflow_row, reflow_text:find("FOOBAR", 1, true) - 1 })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function()
    return state.review_comments[1].body == reflow_text
  end, "long inline edit did not update comment body")
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  vim.wait(40, function() return false end, 10)
  assert_true(
    state.review_comments[1].body == reflow_text,
    "long raw line was rewritten while editing: " .. tostring(state.review_comments[1].body)
  )
  local reflow_start_row, reflow_end_row = diff_review._review.comment_body_rows(buf, state.review_comments[1])
  assert_true(vim.api.nvim_get_current_buf() == buf, "long inline edit opened a popup")
  assert_true(vim.api.nvim_win_get_cursor(0)[1] >= reflow_start_row, "long inline edit cursor left comment body")
  assert_true(vim.api.nvim_win_get_cursor(0)[1] <= reflow_end_row, "long inline edit cursor left comment body")
  local flow_target_row = reflow_start_row
  assert_true(reflow_end_row == reflow_start_row, "long raw line should stay one editable row")
  local flow_target_line = lines(buf)[flow_target_row]
  local flow_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, flow_target_row, flow_target_line)
  local flow_target_text = diff_review._review.comment_body_text_from_line(flow_target_line, flow_start_col)
  vim.api.nvim_buf_set_lines(
    buf,
    flow_target_row - 1,
    flow_target_row,
    false,
    { (" "):rep(flow_start_col) .. flow_target_text .. " test" }
  )
  vim.api.nvim_win_set_cursor(0, { flow_target_row, flow_start_col + #flow_target_text + #" test" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  local expected_reflow_body = reflow_text .. " test"
  wait_for(function()
    return state.review_comments[1].body == expected_reflow_body
  end, "editing at a raw row end did not update the local body: " .. tostring(state.review_comments[1].body))
  diff_review._review.render(buf)
  wait_for(function()
    local start_row, end_row = diff_review._review.comment_body_rows(buf, state.review_comments[1])
    return start_row and end_row and end_row == start_row and lines(buf)[start_row] == expected_reflow_body
  end, "raw-row end edit did not rerender comment body")
  local reflow_update_count = #comment_updates
  trigger(buf, "<C-s>")
  wait_for(function() return #comment_updates > reflow_update_count end, "manual sync did not update raw edited comment")
  local reflow_payload = vim.json.decode(comment_updates[#comment_updates].input)
  assert_true(
    reflow_payload.variables.input.body == expected_reflow_body,
    "manual sync did not send the raw body text: " .. vim.inspect(reflow_payload.variables.input.body)
  )

  local sync_body_start_row, sync_body_end_row = diff_review._review.comment_body_rows(buf, state.review_comments[1])
  assert_true(sync_body_start_row ~= nil and sync_body_end_row ~= nil, "sync normalization setup did not find comment body rows")
  local sync_row = sync_body_start_row
  local sync_line = lines(buf)[sync_row]
  local sync_start_col = diff_review._review.comment_body_text_bounds_at_row(buf, sync_row, sync_line)
  assert_true(sync_start_col == 0, "sync body rows should start at column 0")
  local sync_lines = {
    "foo",
    "bar",
    "",
    "baz",
    "qux",
  }
  vim.api.nvim_buf_set_lines(buf, sync_body_start_row - 1, sync_body_end_row, false, sync_lines)
  vim.api.nvim_win_set_cursor(0, { sync_row + #sync_lines - 1, sync_start_col + #"qux" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  local expected_sync_local_body = table.concat(sync_lines, "\n")
  wait_for(function()
    return state.review_comments[1].body == expected_sync_local_body
  end, "sync normalization test did not preserve local edited rows")
  local sync_update_count = #comment_updates
  trigger(buf, "<C-s>")
  wait_for(function() return #comment_updates > sync_update_count end, "manual sync did not update paragraph normalization test")
  local paragraph_payload = vim.json.decode(comment_updates[#comment_updates].input)
  assert_true(
    paragraph_payload.variables.input.body == expected_sync_local_body,
    "manual sync did not send raw rows while preserving blank paragraphs: "
      .. vim.inspect(paragraph_payload.variables.input.body)
  )
  wait_for(function() return not buffer_contains(buf, "*me commented") end, "normalized sync did not clear dirty marker")
  state.review_comments[1].body = original_body
  diff_review._review.render(buf)

  local selection_state = diff_review._review.state(buf)
  local selection_original_body = selection_state.review_comments[1].body
  selection_state.review_comments[1].body =
    "This is not the time for this only way that, thas is not the time for which the only time know this thing whiche we know. s is not the"
  diff_review._review.render(buf)
  local selection_row = find_row(buf, "This is not the time")
  local selection_line = lines(buf)[selection_row]
  local selection_start_col, selection_end_col = diff_review._review.comment_body_text_selection_bounds(selection_line)
  assert_true(selection_line:find("│", 1, true) == nil, "inline comment body line must not expose a selectable border")
  assert_true(selection_start_col == 0, "inline comment body selection should start at column 0")
  assert_true(selection_end_col == #selection_line - 1, "inline comment body selection should cover the raw line")
  vim.api.nvim_win_set_cursor(0, { selection_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(buffer_keymap(buf, "n", "V") == nil, "review must not hard-code V in inline comment bodies")
  vim.cmd("normal! V")
  local selection_mode = vim.api.nvim_get_mode().mode
  local visual_start = vim.fn.getpos("v")
  local visual_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(selection_mode == "V", "native V in inline comment body did not start linewise visual mode")
  assert_true(visual_start[2] == selection_row, "visual selection started outside the inline comment body row")
  assert_true(visual_cursor[1] == selection_row, "visual selection left the inline comment body row")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "x", false)
  selection_state.review_comments[1].body = selection_original_body
  diff_review._review.render(buf)

  -- ── C near the comment focuses the inline body without using a popup ───────
  local popup_called = false
  diff_review._review.input_provider = function(_, _, _) popup_called = true end
  trigger(buf, "C", find_row(buf, "NEW src/a.txt"))
  assert_true(not popup_called, "C on the line above opened a popup")
  assert_true(vim.api.nvim_get_current_buf() == buf, "C on the line above switched buffers")
  assert_true(vim.api.nvim_win_get_cursor(0)[1] == find_row(buf, "Visual changed comment body"), "C on the line above did not focus body")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "line above comment edit added a duplicate draft")

  popup_called = false
  trigger(buf, "C", find_row(buf, "Visual changed comment body") + 2)
  assert_true(not popup_called, "C on the line below opened a popup")
  local line_below_focus = vim.api.nvim_win_get_cursor(0)[1]
  vim.api.nvim_buf_set_lines(buf, line_below_focus - 1, line_below_focus, false, { "Edited comment body" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function()
    return diff_review._review.state(buf).review_comments[1].body == "Edited comment body"
  end, "line below inline edit did not save")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "line below comment edit added a duplicate draft")

  local draft_path = diff_review._review.storage_path(diff_review._review.state(buf))
  assert_true(
    vim.fs.normalize(draft_path):find(vim.fs.normalize(review_data_dir), 1, true) == 1,
    "review draft path was not under the data dir"
  )
  assert_true(
    vim.fs.normalize(draft_path) == vim.fs.normalize(vim.fs.joinpath(review_data_dir, "repos", "owner", "repo", "reviews", "12", "review.json")),
    "review draft path was not based on the GitHub PR identity: " .. draft_path
  )
  assert_true(vim.uv.fs_stat(draft_path) ~= nil, "review draft was not written")

  local same_pr_buf = diff_review.open_review(pr, { cwd = "D:/diffreview-review-root" })
  assert_true(same_pr_buf ~= nil, "same PR review did not open")
  wait_for(function() return buffer_contains(same_pr_buf, "Edited comment body") end, "same PR review did not load draft comment")
  assert_true(buffer_contains(same_pr_buf, "Looks good overall"), "same PR review did not load draft summary")
  wait_for(function()
    return diff_review._review.state(same_pr_buf).review_remote == nil
  end, "same PR review did not clear a stale cached pending review id")
  local stale_pending_create_count = #pending_review_creates
  local stale_pending_comment_count = #comment_creates
  local fresh_pending_comment =
    create_inline_comment_with_key(same_pr_buf, find_row(same_pr_buf, "NEW src/b.txt"), "C after stale cached pending review")
  set_inline_comment_body(same_pr_buf, fresh_pending_comment, "Fresh pending review comment")
  trigger(same_pr_buf, "<C-s>")
  wait_for(function() return #pending_review_creates == stale_pending_create_count + 1 end, "stale cached pending review was reused")
  wait_for(function() return #comment_creates == stale_pending_comment_count + 1 end, "fresh pending review comment was not created")
  assert_true(
    comment_creates[#comment_creates].payload.pullRequestReviewId == "PRR_7",
    "fresh pending review comment did not use the newly created pending review node id"
  )
  vim.api.nvim_win_set_buf(0, buf)

  local other_pr = vim.deepcopy(pr)
  other_pr.number = 13
  other_pr.url = "https://github.com/owner/repo/pull/13"
  local other_buf = diff_review.open_review(other_pr, { cwd = "D:/diffreview-review-root" })
  assert_true(other_buf ~= nil, "other PR review did not open")
  wait_for(function() return buffer_contains(other_buf, "NEW src/a.txt") end, "other PR review diff did not render")
  assert_true(
    vim.fs.normalize(diff_review._review.storage_path(diff_review._review.state(other_buf)))
      == vim.fs.normalize(vim.fs.joinpath(review_data_dir, "repos", "owner", "repo", "reviews", "13", "review.json")),
    "other PR review path was not based on its PR number"
  )
  assert_true(not buffer_contains(other_buf, "Edited comment body"), "other PR review loaded the first PR draft")
  assert_true(#diff_review._review.state(other_buf).review_comments == 0, "other PR review inherited draft comments")
  vim.api.nvim_win_set_buf(0, buf)

  -- ── remote pending comments import into a fresh review buffer ──────────────
  remote_pending_review = {
    id = 88,
    node_id = "PRR_88",
    state = "PENDING",
    body = "",
    user = { login = "me" },
    commit_id = pr.headRefOid,
  }
  remote_pending_comments = {
    {
      id = 202,
      node_id = "PRRC_202",
      pull_request_review_id = 88,
      body = "Remote web note",
      path = "src/a.txt",
      line = 2,
      position = 3,
      created_at = "2026-06-13T10:11:12Z",
      updated_at = "2026-06-13T10:11:12Z",
      user = { login = "me" },
    },
  }
  local remote_pr = vim.deepcopy(pr)
  remote_pr.number = 14
  remote_pr.url = "https://github.com/owner/repo/pull/14"
  remote_pr.repo = nil
  local remote_open_call_start = #system_call_keys
  local remote_buf = diff_review.open_review(remote_pr, { cwd = "D:/diffreview-review-root" })
  assert_true(remote_buf ~= nil, "remote PR review did not open")
  wait_for(function() return buffer_contains(remote_buf, "Remote web note") end, "remote pending comment was not imported")
  vim.wait(40, function() return false end, 10)
  assert_true(vim.api.nvim_get_current_buf() == remote_buf, "remote pending import opened an edit popup")
  assert_true(buffer_contains(remote_buf, "me commented 9 minutes ago"), "remote pending comment header missed author/date")
  assert_true(buffer_contains(remote_buf, " L2"), "remote pending comment header missed line number")
  assert_true(diff_review._review.state(remote_buf).pr.repo == "owner/repo", "remote PR repo was not resolved from its URL")
  local first_pending_read, first_diff
  for index = remote_open_call_start + 1, #system_call_keys do
    local key = system_call_keys[index]
    if not first_pending_read and key == "gh api graphql --input -" then first_pending_read = index end
    if not first_diff and key:find("gh pr diff", 1, true) then first_diff = index end
  end
  assert_true(first_pending_read ~= nil, "remote open did not check for a pending review")
  assert_true(first_diff ~= nil, "remote open did not load the PR diff")
  assert_true(first_pending_read < first_diff, "remote open loaded the diff before checking pending review")
  local remote_comment = diff_review._review.state(remote_buf).review_comments[1]
  assert_true(remote_comment.remote_id == 202, "remote comment id was not stored")
  assert_true(remote_comment.remote_node_id == "PRRC_202", "remote comment node id was not stored")
  assert_true(remote_comment.local_state == "clean", "imported remote comment was not clean")
  local creates_before_remote_add = #pending_review_creates
  local comment_creates_before_remote_add = #comment_creates
  local remote_new_comment =
    create_inline_comment_with_key(remote_buf, find_row(remote_buf, "NEW src/b.txt"), "C on imported review")
  set_inline_comment_body(remote_buf, remote_new_comment, "Comment on imported review")
  wait_for(function() return buffer_contains(remote_buf, "Comment on imported review") end, "comment on imported review not rendered")
  assert_true(#comment_creates == comment_creates_before_remote_add, "comment on imported review synced before manual sync")
  assert_true(buffer_contains(remote_buf, "*you commented"), "comment on imported review did not show dirty marker")
  trigger(remote_buf, "<C-s>")
  wait_for(function() return #comment_creates == comment_creates_before_remote_add + 1 end, "manual sync did not sync comment on imported review")
  wait_for(function() return not buffer_contains(remote_buf, "*you commented") end, "manual sync did not clear imported review dirty marker")
  assert_true(#pending_review_creates == creates_before_remote_add, "imported pending review was not reused")
  remote_pending_review = nil
  remote_pending_comments = {}
  vim.api.nvim_win_set_buf(0, buf)

  -- ── add a second comment, then J deletes the one under the cursor ──────────
  local second_comment = create_inline_comment_with_key(buf, find_row(buf, "NEW src/b.txt"), "C for second comment")
  set_inline_comment_body(buf, second_comment, "Second comment")
  wait_for(function() return buffer_contains(buf, "Second comment") end, "second comment not added")
  assert_true(#diff_review._review.state(buf).review_comments == 2, "expected two draft comments")
  trigger(buf, "J", find_row(buf, "NEW src/b.txt"))
  wait_for(function() return not buffer_contains(buf, "Second comment") end, "J did not delete the comment")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "delete did not remove the draft")

  -- ── b browses to PR changes, anchored when the cursor is on diff/comment ───
  trigger(buf, "b", find_row(buf, "Title: Add the thing"))
  assert_true(opened_urls[#opened_urls] == pr.url .. "/changes", "b did not browse to the PR changes URL")
  local a_hash = vim.fn.sha256("src/a.txt")
  trigger(buf, "b", find_row(buf, "NEW src/a.txt"))
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#diff-" .. a_hash .. "R2",
    "b did not browse to the added-line anchor: " .. tostring(opened_urls[#opened_urls])
  )
  trigger_visual(buf, "b", find_row(buf, "NEW src/a.txt"), find_row(buf, "omega src/a.txt"))
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#diff-" .. a_hash .. "R2-R3",
    "visual b did not browse to the selected range anchor: " .. tostring(opened_urls[#opened_urls])
  )
  trigger(buf, "b", find_row(buf, "old src/a.txt"))
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#diff-" .. a_hash .. "L2",
    "b did not browse to the deleted-line anchor: " .. tostring(opened_urls[#opened_urls])
  )
  local browse_comment_body_row = find_row(buf, "Edited comment body")
  local browse_comment_header_row = browse_comment_body_row - 1
  local browse_comment_footer_row = browse_comment_body_row + 1
  trigger(buf, "b", browse_comment_header_row)
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#r101",
    "b on comment header did not browse to the review comment anchor: " .. tostring(opened_urls[#opened_urls])
  )
  trigger(buf, "b", browse_comment_body_row)
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#r101",
    "b on comment body did not browse to the review comment anchor: " .. tostring(opened_urls[#opened_urls])
  )
  trigger(buf, "b", browse_comment_footer_row)
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#r101",
    "b on comment footer did not browse to the review comment anchor: " .. tostring(opened_urls[#opened_urls])
  )
  trigger(buf, "b", browse_comment_header_row - 1)
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#diff-" .. a_hash .. "R2",
    "b on code above a comment should browse to code, not comment: " .. tostring(opened_urls[#opened_urls])
  )
  trigger(buf, "b", browse_comment_footer_row + 1)
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#diff-" .. a_hash .. "R3",
    "b on code below a comment should browse to code, not comment: " .. tostring(opened_urls[#opened_urls])
  )

  -- ── submit flushes pending comments, then submits the pending review ───────
  diff_review._review.verdict_provider = function(on_choice) on_choice("APPROVE") end
  trigger(buf, "cc")
  wait_for(function() return #review_calls == 1 end, "submit did not post a review")
  local payload = vim.json.decode(review_calls[1].input)
  assert_true(payload.event == "APPROVE", "wrong verdict: " .. tostring(payload.event))
  assert_true(payload.body == "Looks good overall", "review body not the edited summary: " .. tostring(payload.body))
  assert_true(review_calls[1].command[5]:find("/reviews/7/events", 1, true) ~= nil, "submit did not use pending review events endpoint")
  wait_for(function() return saw_notification_containing("Review submitted (APPROVE, 1 comment)") end, "submit not notified")
  -- comments cleared after a successful submit
  wait_for(function() return not buffer_contains(buf, "Edited comment body") end, "comments not cleared after submit")
  assert_true(#diff_review._review.state(buf).review_comments == 0, "drafts not cleared after submit")
  assert_true(vim.uv.fs_stat(draft_path) == nil, "review draft was not deleted after submit")

  -- ── failed submit notifies and keeps the drafts ────────────────────────────
  local failing_comment = create_inline_comment_with_key(buf, find_row(buf, "NEW src/a.txt"), "C for failed submit")
  set_inline_comment_body(buf, failing_comment, "Another note")
  wait_for(function() return buffer_contains(buf, "Another note") end, "comment for failure case not added")
  review_should_fail = true
  captured_notifications = {}
  diff_review._review.verdict_provider = function(on_choice) on_choice("COMMENT") end
  trigger(buf, "cc", find_row(buf, "Title: Add the thing"))
  wait_for(function() return saw_notification_containing("PR review submit failed") end, "failed submit not notified")
  assert_true(buffer_contains(buf, "Another note"), "failed submit must keep the drafts")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review._review.input_provider = nil
diff_review._review.verdict_provider = nil
diff_review._review.set_data_dir_for_test(nil)
diff_review._datetime.now_override = nil
diff_review.reset_git_backend()
gh.reset_backend()
vim.fn.delete(review_data_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
