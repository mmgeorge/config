vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

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
      comment_creates[#comment_creates + 1] = { command = command, input = input }
      local stdout = vim.json.encode({
        data = {
          addPullRequestReviewComment = {
            comment = {
              id = "PRRC_101",
              databaseId = 101,
              body = "Edited comment body",
              path = "src/a.txt",
              line = 2,
              position = 3,
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

local function find_row(buf, needle)
  for index, line in ipairs(lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines(buf), "\n"), 2)
end

local function row_after(buf, needle, after)
  local content = lines(buf)
  for index = after + 1, #content do
    if content[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. after .. ": " .. needle, 2)
end

local function with_captured_commands(callback)
  local original_cmd = vim.cmd
  local commands = {}
  vim.cmd = function(command)
    commands[#commands + 1] = command
    return original_cmd(command)
  end
  local ok, result = pcall(callback)
  vim.cmd = original_cmd
  if not ok then error(result, 2) end
  return commands
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
  if row then vim.api.nvim_win_set_cursor(0, { row, 0 }) end
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

  local buf = diff_review.open_review(pr, { cwd = "D:/diffreview-review-root" })
  assert_true(buf ~= nil, "open_review did not return a buffer")
  wait_for(function() return buffer_contains(buf, "NEW src/a.txt") end, "review diff did not render")

  -- ── layout + hint shows the review commands ────────────────────────────────
  assert_true(buffer_contains(buf, "Title: Add the thing"), "title missing")
  assert_true(buffer_contains(buf, "Review Comment:"), "review comment label missing")
  assert_true(buffer_contains(buf, "Unviewed Changes (2)"), "unviewed section missing both files")
  assert_true(buffer_contains(buf, "Viewed Changes (0)"), "viewed section missing")
  local hint = plain_winbar()
  assert_true(hint:find("Review #12", 1, true) ~= nil, "hint missing review title: " .. hint)
  for _, token in ipairs({ "<Tab> toggle", "N Collapse Parent", "S viewed", "U unviewed", "C comment", "J delete", "<C-s> submit", "q close", "? help" }) do
    assert_true(hint:find(token, 1, true) ~= nil, "hint missing " .. token .. ": " .. hint)
  end
  for _, token in ipairs({ "y next", "n prev", "b browse" }) do
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
    { "C", "Add or edit a draft review comment" },
    { "J", "Delete draft comment" },
    { "y", "Jump to next draft comment" },
    { "n", "Jump to previous draft comment" },
    { "<C-s>", "Submit review to GitHub" },
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
  wait_for(function() return not buffer_contains(buf, "src/a.txt +1 -1") end, "Unviewed section did not fold")
  assert_true(not buffer_contains(buf, "src/b.txt +1 -1"), "folded Unviewed section still showed b.txt")
  trigger(buf, "<Tab>", find_row(buf, "Unviewed Changes (2)"))
  wait_for(function() return buffer_contains(buf, "src/a.txt +1 -1") end, "Unviewed section did not unfold")

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
  wait_for(function() return not buffer_contains(buf, "src/a.txt +1 -1") end, "Viewed section did not fold")
  assert_true(buffer_contains(buf, "src/b.txt +1 -1"), "folding Viewed hid the Unviewed file")
  trigger(buf, "<Tab>", find_row(buf, "Viewed Changes (1)"))
  wait_for(function() return buffer_contains(buf, "src/a.txt +1 -1") end, "Viewed section did not unfold")
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
  vim.api.nvim_win_set_cursor(0, { comment_label + 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "review comment line must be editable")
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, comment_label, comment_label + 1, false, { "Looks good overall" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  assert_true(diff_review._review.state(buf).review_comment_text == "Looks good overall", "summary text not captured")
  local passthrough_q_count = 0
  vim.keymap.set("n", "q", function()
    passthrough_q_count = passthrough_q_count + 1
  end, { silent = true })
  local passthrough_ok, passthrough_err = pcall(function()
    trigger(buf, "q", comment_label + 1)
    wait_for(function() return passthrough_q_count == 1 end, "q did not pass through in editable review text")
    assert_true(vim.api.nvim_buf_is_valid(buf), "q closed the review buffer from editable review text")
  end)
  pcall(vim.keymap.del, "n", "q")
  if not passthrough_ok then error(passthrough_err) end

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

  -- ── C on a changed line drafts locally, then syncs to a pending review ─────
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("This rename needs a test") end
  trigger(buf, "C", find_row(buf, "NEW src/a.txt"))
  wait_for(function() return buffer_contains(buf, "This rename needs a test") end, "comment box not rendered as real lines")
  wait_for(function() return #pending_review_creates == 1 end, "drafting a comment did not create a pending review")
  wait_for(function() return #comment_creates == 1 end, "drafting a comment did not sync the pending review comment")
  assert_true(#review_calls == 0, "drafting a comment must not submit the review")
  assert_true(buffer_contains(buf, " L2 "), "comment box line header missing")
  assert_true(buffer_contains(buf, " | "), "comment box author/date header missing")
  -- the comment lines are real and below the anchor line
  assert_true(
    find_row(buf, "This rename needs a test") > find_row(buf, "NEW src/a.txt"),
    "comment must render below its anchor line"
  )

  -- ── cursor can land on the comment; y jumps to it ──────────────────────────
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  trigger(buf, "y")
  local landed = vim.api.nvim_win_get_cursor(0)[1]
  assert_true(lines(buf)[landed]:find(" L2 ", 1, true) ~= nil, "y did not jump to the comment")
  local comment_obj = diff_review._review.comment_under_cursor(buf)
  assert_true(comment_obj ~= nil and comment_obj.body == "This rename needs a test", "cursor not recognized as on the comment")

  -- ── cursor is restricted to the inline comment text, not its box chrome ────
  local comment_header_row = find_row(buf, " L2 ")
  local comment_body_row = find_row(buf, "This rename needs a test")
  local comment_footer_row = comment_body_row + 1
  local comment_above_row = comment_header_row - 1
  local comment_below_row = comment_footer_row + 1
  vim.api.nvim_win_set_cursor(0, { comment_header_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  local clamped_cursor = vim.api.nvim_win_get_cursor(0)
  local body_start_col, body_end_col = diff_review._review.comment_body_text_bounds(lines(buf)[comment_body_row])
  assert_true(clamped_cursor[1] == comment_body_row, "cursor stayed on inline comment header")
  assert_true(clamped_cursor[2] == body_start_col, "cursor on header did not clamp to comment text start")

  assert_true((lines(buf)[comment_footer_row] or ""):find("╰", 1, true) ~= nil, "expected comment footer below body")
  vim.api.nvim_win_set_cursor(0, { comment_above_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_win_set_cursor(0, { comment_header_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == comment_body_row, "moving down onto inline comment header did not enter body text")

  vim.api.nvim_win_set_cursor(0, { comment_header_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == comment_above_row, "moving up from inline comment body did not skip the header")

  vim.api.nvim_win_set_cursor(0, { comment_below_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_win_set_cursor(0, { comment_footer_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == comment_body_row, "moving up onto inline comment footer did not enter body text")

  vim.api.nvim_win_set_cursor(0, { comment_footer_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == comment_below_row, "moving down from inline comment body did not skip the footer")

  local body_line = lines(buf)[comment_body_row]
  assert_true(body_line:find("│", 1, true) == nil, "inline comment body border must be virtual, not selectable text")
  vim.api.nvim_win_set_cursor(0, { comment_body_row, #body_line })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  clamped_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(clamped_cursor[1] == comment_body_row, "cursor left comment body while clamping padding")
  assert_true(clamped_cursor[2] == body_end_col, "cursor stayed beyond inline comment text")

  -- ── C on the comment hides it visually while the edit popup is active ──────
  diff_review._review.input_provider = nil
  trigger(buf, "C", find_row(buf, "This rename needs a test"))
  wait_for(function() return not buffer_contains(buf, "This rename needs a test") end, "comment stayed visible while edit popup was open")
  local edit_popup_buf = vim.api.nvim_get_current_buf()
  assert_true(lines(edit_popup_buf)[1] == "This rename needs a test", "real edit popup was not prefilled")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "editing visually removed the draft from state")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-q>", true, false, true), "x", false)
  wait_for(function() return buffer_contains(buf, "This rename needs a test") end, "comment did not return after canceling edit")

  trigger(buf, "C", find_row(buf, "This rename needs a test"))
  wait_for(function() return not buffer_contains(buf, "This rename needs a test") end, "comment stayed visible while saving edit")
  edit_popup_buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(edit_popup_buf, 0, -1, false, { "Edited comment body" })
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return buffer_contains(buf, "Edited comment body") end, "comment edit did not apply")
  assert_true(diff_review._review.state(buf).review_editing_comment == nil, "editing marker was not cleared after save")
  assert_true(not buffer_contains(buf, "This rename needs a test"), "old comment text still present after edit")

  -- ── visual deletion must not type the popup's insert-mode fallback key ─────
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
  wait_for(function() return vim.api.nvim_get_current_buf() ~= buf end, "delete edit popup did not receive focus")
  edit_popup_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return lines(edit_popup_buf)[1] == deletion_expected_text end, "delete edit popup text was not the edited comment")
  vim.wait(40, function() return false end, 10)
  assert_true(lines(edit_popup_buf)[1] == deletion_expected_text, "delete edit popup inserted an extra character")
  assert_true(
    vim.api.nvim_win_get_cursor(0)[2] == deletion_expected_text:find("the correct", 1, true) - 1,
    "delete edit popup cursor did not stay before the following word"
  )
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-q>", true, false, true), "x", false)
  wait_for(function() return vim.api.nvim_get_current_buf() == buf end, "delete edit popup did not cancel")
  deletion_state.review_comments[1].body = deletion_original_body
  diff_review._review.render(buf)

  -- ── native edits on comment bodies open the same edit popup ────────────────
  local direct_edit_row = find_row(buf, "Edited comment body")
  vim.api.nvim_win_set_cursor(0, { direct_edit_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "rendered inline comment body must become editable on hover")
  vim.api.nvim_buf_set_lines(buf, direct_edit_row - 1, direct_edit_row, false, { "Direct edited comment body" })
  vim.api.nvim_win_set_cursor(0, { direct_edit_row, 7 })
  local inline_commands, restore_inline_commands = start_command_capture()
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function() return vim.api.nvim_get_current_buf() ~= buf end, "inline edit popup did not receive focus")
  wait_for(function() return vim.tbl_contains(inline_commands, "startinsert") end, "inline edit popup did not request insert mode")
  restore_inline_commands()
  wait_for(function() return not buffer_contains(buf, "Edited comment body") end, "comment stayed visible while shortcut popup was open")
  edit_popup_buf = vim.api.nvim_get_current_buf()
  assert_true(lines(edit_popup_buf)[1] == "Direct edited comment body", "inline edit popup was not prefilled with edited text")
  assert_true(vim.api.nvim_win_get_cursor(0)[2] == 7, "inline edit popup cursor did not preserve the edited position")
  assert_true(
    diff_review._review.state(buf).review_comments[1].body == "Edited comment body",
    "inline edit must not save until popup submit"
  )
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return buffer_contains(buf, "Direct edited comment body") end, "shortcut comment edit did not apply")
  wait_for(function() return #comment_updates >= 2 end, "shortcut comment edit did not sync")

  local visual_change_mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("c", "x", false, true)
  end)
  assert_true(visual_change_mapping.desc ~= "Edit review comment", "visual c must not be a plugin edit-intent mapping")
  local second_edit_row = find_row(buf, "Direct edited comment body")
  vim.api.nvim_win_set_cursor(0, { second_edit_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  vim.api.nvim_buf_set_lines(buf, second_edit_row - 1, second_edit_row, false, { "Visual changed comment body" })
  vim.api.nvim_win_set_cursor(0, { second_edit_row, 6 })
  local second_inline_commands, restore_second_inline_commands = start_command_capture()
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function() return vim.api.nvim_get_current_buf() ~= buf end, "second inline edit popup did not receive focus")
  wait_for(function() return vim.tbl_contains(second_inline_commands, "startinsert") end, "second inline edit popup did not request insert mode")
  restore_second_inline_commands()
  wait_for(function() return not buffer_contains(buf, "Direct edited comment body") end, "comment stayed visible during visual edit popup")
  edit_popup_buf = vim.api.nvim_get_current_buf()
  assert_true(lines(edit_popup_buf)[1] == "Visual changed comment body", "second inline edit popup was not prefilled")
  assert_true(vim.api.nvim_win_get_cursor(0)[2] == 6, "second inline edit popup cursor did not preserve the edited position")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-s>", true, false, true), "x", false)
  wait_for(function() return buffer_contains(buf, "Visual changed comment body") end, "visual change comment edit did not apply")
  wait_for(function() return #comment_updates >= 3 end, "visual change comment edit did not sync")

  local state = diff_review._review.state(buf)
  local original_body = state.review_comments[1].body
  state.review_comments[1].body =
    "This is a deliberately long inline review comment body that should wrap before the rendered comment box can exceed the maximum allowed width of seventy two columns."
  diff_review._review.render(buf)
  for row, entry in pairs(state.entries or {}) do
    if entry.kind == "review_comment" then
      assert_true(vim.fn.strdisplaywidth(lines(buf)[row] or "") <= 72, "inline comment box exceeded 72 columns")
    end
  end
  state.review_comments[1].body = original_body
  diff_review._review.render(buf)

  local selection_state = diff_review._review.state(buf)
  local selection_original_body = selection_state.review_comments[1].body
  selection_state.review_comments[1].body =
    "This is not the time for this only way that, thas is not the time for which the only time know this thing whiche we know. s is not the"
  diff_review._review.render(buf)
  local selection_row = find_row(buf, "This is not the time")
  local selection_line = lines(buf)[selection_row]
  local selection_start_col, selection_end_col = diff_review._review.comment_body_text_bounds(selection_line)
  assert_true(selection_line:find("│", 1, true) == nil, "inline comment body line must not expose a selectable border")
  assert_true((lines(buf)[selection_row + 1] or ""):find("│", 1, true) == nil, "wrapped comment body line must not expose a selectable border")
  vim.api.nvim_win_set_cursor(0, { selection_row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  trigger(buf, "V", selection_row)
  local selection_mode = vim.api.nvim_get_mode().mode
  local visual_start = vim.fn.getpos("v")
  local visual_cursor = vim.api.nvim_win_get_cursor(0)
  assert_true(selection_mode == "v", "V in inline comment body did not start characterwise visual mode")
  assert_true(visual_start[2] == selection_row and visual_start[3] == selection_start_col + 1, "visual selection started outside comment text")
  assert_true(visual_cursor[1] == selection_row and visual_cursor[2] == selection_end_col, "visual selection ended outside comment text")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "x", false)
  selection_state.review_comments[1].body = selection_original_body
  diff_review._review.render(buf)

  -- ── C near the comment edits it (input prefilled with existing body) ───────
  local prefill_seen
  prefill_seen = nil
  diff_review._review.input_provider = function(_, on_submit, prefill)
    prefill_seen = prefill
    on_submit("Edited from above")
  end
  trigger(buf, "C", find_row(buf, "NEW src/a.txt"))
  wait_for(function() return buffer_contains(buf, "Edited from above") end, "C on the line above did not edit the comment")
  assert_true(prefill_seen == "Visual changed comment body", "line above edit was not prefilled with existing body")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "line above comment edit added a duplicate draft")

  prefill_seen = nil
  diff_review._review.input_provider = function(_, on_submit, prefill)
    prefill_seen = prefill
    on_submit("Edited comment body")
  end
  trigger(buf, "C", find_row(buf, "Edited from above") + 2)
  wait_for(function() return buffer_contains(buf, "Edited comment body") end, "C on the line below did not edit the comment")
  assert_true(prefill_seen == "Edited from above", "line below edit was not prefilled with existing body")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "line below comment edit added a duplicate draft")

  local draft_path = diff_review._review.storage_path(diff_review._review.state(buf))
  assert_true(
    vim.fs.normalize(draft_path):find(vim.fs.normalize(review_data_dir), 1, true) == 1,
    "review draft path was not under the data dir"
  )
  assert_true(
    vim.fs.normalize(draft_path) == vim.fs.normalize(vim.fs.joinpath(review_data_dir, "owner", "repo", "12", "review.json")),
    "review draft path was not based on the GitHub PR identity: " .. draft_path
  )
  assert_true(vim.uv.fs_stat(draft_path) ~= nil, "review draft was not written")

  local same_pr_buf = diff_review.open_review(pr, { cwd = "D:/diffreview-review-root" })
  assert_true(same_pr_buf ~= nil, "same PR review did not open")
  wait_for(function() return buffer_contains(same_pr_buf, "Edited comment body") end, "same PR review did not load draft comment")
  assert_true(buffer_contains(same_pr_buf, "Looks good overall"), "same PR review did not load draft summary")

  local other_pr = vim.deepcopy(pr)
  other_pr.number = 13
  other_pr.url = "https://github.com/owner/repo/pull/13"
  local other_buf = diff_review.open_review(other_pr, { cwd = "D:/diffreview-review-root" })
  assert_true(other_buf ~= nil, "other PR review did not open")
  wait_for(function() return buffer_contains(other_buf, "NEW src/a.txt") end, "other PR review diff did not render")
  assert_true(
    vim.fs.normalize(diff_review._review.storage_path(diff_review._review.state(other_buf)))
      == vim.fs.normalize(vim.fs.joinpath(review_data_dir, "owner", "repo", "13", "review.json")),
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
  assert_true(buffer_contains(remote_buf, "me | 2026-06-13 10:11"), "remote pending comment header missed author/date")
  assert_true(buffer_contains(remote_buf, " L2 "), "remote pending comment header missed line number")
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
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("Comment on imported review") end
  trigger(remote_buf, "C", find_row(remote_buf, "NEW src/b.txt"))
  wait_for(function() return buffer_contains(remote_buf, "Comment on imported review") end, "comment on imported review not rendered")
  wait_for(function() return #comment_creates >= 2 end, "comment on imported review did not sync")
  assert_true(#pending_review_creates == creates_before_remote_add, "imported pending review was not reused")
  remote_pending_review = nil
  remote_pending_comments = {}
  vim.api.nvim_win_set_buf(0, buf)

  -- ── add a second comment, then J deletes the one under the cursor ──────────
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("Second comment") end
  trigger(buf, "C", find_row(buf, "NEW src/b.txt"))
  wait_for(function() return buffer_contains(buf, "Second comment") end, "second comment not added")
  assert_true(#diff_review._review.state(buf).review_comments == 2, "expected two draft comments")
  trigger(buf, "J", find_row(buf, "Second comment"))
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
  trigger(buf, "b", find_row(buf, " L2 "))
  assert_true(
    opened_urls[#opened_urls] == pr.url .. "/changes#r101",
    "b did not browse to the review comment anchor: " .. tostring(opened_urls[#opened_urls])
  )

  -- ── submit flushes pending comments, then submits the pending review ───────
  diff_review._review.verdict_provider = function(on_choice) on_choice("APPROVE") end
  trigger(buf, "<C-s>")
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
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("Another note") end
  trigger(buf, "C", find_row(buf, "NEW src/a.txt"))
  wait_for(function() return buffer_contains(buf, "Another note") end, "comment for failure case not added")
  review_should_fail = true
  captured_notifications = {}
  diff_review._review.verdict_provider = function(on_choice) on_choice("COMMENT") end
  trigger(buf, "<C-s>")
  wait_for(function() return saw_notification_containing("PR review submit failed") end, "failed submit not notified")
  assert_true(buffer_contains(buf, "Another note"), "failed submit must keep the drafts")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review._review.input_provider = nil
diff_review._review.verdict_provider = nil
diff_review._review.set_data_dir_for_test(nil)
diff_review.reset_git_backend()
gh.reset_backend()
vim.fn.delete(review_data_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
