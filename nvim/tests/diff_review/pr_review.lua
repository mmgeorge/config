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

--- Two changed files, each "old line" -> "NEW line" at new-file line 2.
local function file_diff(relpath)
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

local pr_diff_text = file_diff("src/a.txt") .. "\n" .. file_diff("src/b.txt")

---@type { command: string[], input: string? }[]
local review_calls = {}
local review_should_fail = false
local opened_urls = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  vim.defer_fn(function()
    if key:find("gh pr diff", 1, true) then
      cb({ code = 0, stdout = pr_diff_text, stderr = "", output = pr_diff_text })
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

local function trigger(buf, key, row)
  vim.api.nvim_win_set_buf(0, buf)
  if row then vim.api.nvim_win_set_cursor(0, { row, 0 }) end
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
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
  local hint = lines(buf)[1]
  for _, token in ipairs({ "S viewed", "U unviewed", "C comment", "J delete", "y next", "<C-s> submit", "b browse" }) do
    assert_true(hint:find(token, 1, true) ~= nil, "hint missing " .. token .. ": " .. hint)
  end

  -- ── S/U move a file between sections ───────────────────────────────────────
  trigger(buf, "S", find_row(buf, "src/a.txt +1 -1"))
  wait_for(function() return buffer_contains(buf, "Unviewed Changes (1)") end, "S did not move a.txt to viewed")
  assert_true(buffer_contains(buf, "Viewed Changes (1)"), "viewed count did not increase")
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

  -- ── C off a changed line is a no-op ────────────────────────────────────────
  local input_called = false
  diff_review._review.input_provider = function(_, _, _) input_called = true end
  trigger(buf, "C", find_row(buf, "Title:"))
  vim.wait(40, function() return false end, 10)
  assert_true(not input_called, "C on a non-diff line must not open the input")

  -- ── C on a changed line drafts a LOCAL comment (no GitHub call yet) ─────────
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("This rename needs a test") end
  trigger(buf, "C", find_row(buf, "NEW src/a.txt"))
  wait_for(function() return buffer_contains(buf, "This rename needs a test") end, "comment box not rendered as real lines")
  assert_true(#review_calls == 0, "drafting a comment must NOT post to GitHub yet")
  assert_true(buffer_contains(buf, "comment 1"), "comment box header missing")
  -- the comment lines are real and below the anchor line
  assert_true(
    find_row(buf, "This rename needs a test") > find_row(buf, "NEW src/a.txt"),
    "comment must render below its anchor line"
  )

  -- ── cursor can land on the comment; y jumps to it ──────────────────────────
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  trigger(buf, "y")
  local landed = vim.api.nvim_win_get_cursor(0)[1]
  assert_true(lines(buf)[landed]:find("comment 1", 1, true) ~= nil, "y did not jump to the comment")
  local comment_obj = diff_review._review.comment_under_cursor(buf)
  assert_true(comment_obj ~= nil and comment_obj.body == "This rename needs a test", "cursor not recognized as on the comment")

  -- ── C on the comment edits it (input prefilled with existing body) ─────────
  local prefill_seen
  diff_review._review.input_provider = function(_, on_submit, prefill)
    prefill_seen = prefill
    on_submit("Edited comment body")
  end
  trigger(buf, "C", find_row(buf, "This rename needs a test"))
  wait_for(function() return buffer_contains(buf, "Edited comment body") end, "comment edit did not apply")
  assert_true(prefill_seen == "This rename needs a test", "edit input was not prefilled with the existing body")
  assert_true(not buffer_contains(buf, "This rename needs a test"), "old comment text still present after edit")
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

  -- ── add a second comment, then J deletes the one under the cursor ──────────
  diff_review._review.input_provider = function(_, on_submit, _) on_submit("Second comment") end
  trigger(buf, "C", find_row(buf, "NEW src/b.txt"))
  wait_for(function() return buffer_contains(buf, "Second comment") end, "second comment not added")
  assert_true(#diff_review._review.state(buf).review_comments == 2, "expected two draft comments")
  trigger(buf, "J", find_row(buf, "Second comment"))
  wait_for(function() return not buffer_contains(buf, "Second comment") end, "J did not delete the comment")
  assert_true(#diff_review._review.state(buf).review_comments == 1, "delete did not remove the draft")

  -- ── b browses to the PR ────────────────────────────────────────────────────
  trigger(buf, "b")
  assert_true(opened_urls[#opened_urls] == pr.url, "b did not browse to the PR URL")

  -- ── submit posts the whole review in ONE request ───────────────────────────
  diff_review._review.verdict_provider = function(on_choice) on_choice("APPROVE") end
  trigger(buf, "<C-s>")
  wait_for(function() return #review_calls == 1 end, "submit did not post a review")
  local payload = vim.json.decode(review_calls[1].input)
  assert_true(payload.event == "APPROVE", "wrong verdict: " .. tostring(payload.event))
  assert_true(payload.body == "Looks good overall", "review body not the edited summary: " .. tostring(payload.body))
  assert_true(payload.commit_id == pr.headRefOid, "review not anchored to the head SHA")
  assert_true(type(payload.comments) == "table" and #payload.comments == 1, "review must carry the one draft comment")
  local c = payload.comments[1]
  assert_true(c.path == "src/a.txt", "wrong comment path: " .. tostring(c.path))
  assert_true(c.side == "RIGHT", "added line should be RIGHT side")
  assert_true(c.line == 2, "wrong comment line: " .. tostring(c.line))
  assert_true(c.body == "Edited comment body", "wrong comment body: " .. tostring(c.body))
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
