vim.opt.runtimepath:append("nvim")
local adapter = require("forge.review_document")
local cache = require("github.repo_cache")
local index = require("github.issue_index")
local warmed_users, warmed_issues, saves, actions = 0, 0, 0, 0
cache.ensure_metadata = function(_, repo) assert(repo == "owner/repo") warmed_users = warmed_users + 1 end
index.ensure_repo = function(_, repo) assert(repo == "owner/repo") warmed_issues = warmed_issues + 1 end
cache.contributors = function(repo) assert(repo == "owner/repo") return { { login = "alice", name = "Alice" } } end
index.search = function(repo, query)
  assert(repo == "owner/repo" and query == "issue")
  return { { number = 42, title = "Issue search", repo = repo, state = "OPEN" } }
end
local field = {
  { region = "title", text = "PR title", baseline = "PR title", revision = 0, sequence = 0 },
  { region = "reviewers", text = "@alice", baseline = "@alice", revision = 0, sequence = 0 },
  { region = "body", text = "Description text", baseline = "Description text", revision = 0, sequence = 0 },
}
local revision = 0
local function block(identity, text, metadata)
  return { id = identity, text = vim.split(text, "\n", { plain = true }), metadata = vim.tbl_extend("force", {
    target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {}, editable_region = {},
  }, metadata or {}) }
end
local function field_block(value)
  local rows = vim.split(value.text, "\n", { plain = true })
  return block("region:" .. value.region, value.text, { editable_region = { {
    id = value.region, revision = value.revision, sequence = value.sequence,
    range = { start = { row = 0, column = 0 }, ["end"] = { row = #rows - 1, column = #rows[#rows] } },
  } } })
end
local function snapshot()
  return { document = "editing", revision = revision, block = {
    field_block(field[1]), block("repo", "Repo:   owner/repo"), field_block(field[2]),
    block("label:body", "Description:"), field_block(field[3]), block("checks", "Checks:"),
  } }
end
local save_callback
adapter._set_runner_for_test(function(method, params, callback)
  if method == "review.open" then callback({ document = "editing", field = vim.deepcopy(field) })
  elseif method == "review.header" then callback({ ready = true })
  elseif method == "review.materialize" then callback({ snapshot = snapshot(), field = vim.deepcopy(field) })
  elseif method == "review.view" or method == "review.close" then callback({})
  elseif method == "review.load" then callback({ diagnostic = {} })
  elseif method == "review.act" then actions = actions + 1 error("editable text dispatched an action")
  elseif method == "review.region_edit" then
    local before = snapshot()
    local selected
    for _, value in ipairs(field) do if value.region == params.region then selected = value end end
    assert(selected and params.base == selected.revision)
    selected.text, selected.sequence, selected.revision = params.text, params.sequence, params.base + 1
    local start, previous_count = 0, 0
    for _, value in ipairs(before.block) do
      if value.id == "region:" .. params.region then previous_count = #value.text break end
      start = start + #value.text
    end
    local changed = field_block(selected)
    local count = 0
    for _, value in ipairs(before.block) do count = count + #value.text end
    revision = revision + 1
    callback({ document = "editing", region = params.region, sequence = params.sequence, revision = selected.revision,
      patch = { document = "editing", base = revision - 1, next = revision, base_rows = count,
        next_rows = count - previous_count + #changed.text, base_blocks = 6, next_blocks = 6,
        removed_block = {}, block_edit = {},
        text_edit = { { start_row = start, removed_rows = previous_count, text = changed.text } },
        metadata_edit = { { block = changed.id, row_count = #changed.text, metadata = changed.metadata } },
      } })
  elseif method == "review.save" then
    saves = saves + 1
    local captured = vim.deepcopy(field)
    save_callback = function(failure)
      if failure then callback(nil, failure) return end
      for position, value in ipairs(field) do value.baseline = captured[position].text end
      callback({ snapshot = { field = vim.deepcopy(field), uncertain = false }, remote = { outcome = "confirmed" } })
    end
  else error(method) end
end)
local errors = {}
local state = adapter.open({ directory = vim.fn.getcwd(), number = 7,
  repository = { hostname = cache.hostname(), owner = "owner", name = "repo" },
  on_error = function(message) errors[#errors + 1] = message end })
assert(vim.wait(1000, function() return state.shown and state.view_ready end))
assert(warmed_users == 1 and warmed_issues == 1 and cache.user_completion_enabled(state.replica.buffer))
local function cursor(row, column)
  vim.api.nvim_win_set_cursor(0, { row, column or 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = state.replica.buffer })
end
local function keys(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = state.replica.buffer })
end
local function marker_rows()
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.replica.buffer,
    vim.api.nvim_get_namespaces().ForgeReviewDirty, 0, -1, { details = true })) do
    local line = vim.api.nvim_buf_get_lines(state.replica.buffer, mark[2], mark[2] + 1, false)[1]
    if line == "Description:" then
      assert(mark[3] == #"Description" and mark[4].virt_text_pos == "inline")
      assert(mark[4].virt_text[1][1] == "*", "description marker must render before its colon")
    else
      assert(mark[4].virt_text[1][1] == " *")
    end
    result[#result + 1] = mark[2] + 1
  end
  return result
end
cursor(1)
assert(vim.bo.modifiable)
keys("A changed<CR><Esc>")
assert(vim.api.nvim_get_current_line() == "PR title changed", "title accepted a newline or remained locked")
cursor(3)
keys("A @bob<CR><Esc>")
assert(vim.api.nvim_get_current_line() == "@alice @bob", "reviewer editing failed")
cursor(5)
for _, key in ipairs({ "C", "J", "R", "b", "<CR>", "or" }) do
  assert(vim.fn.maparg(key, "n", false, true).buffer ~= 1, "native editing was overridden: " .. key)
end
keys("oSecond line<Esc>")
assert(not state.replica.editable.fault, state.replica.editable.fault)
assert(vim.api.nvim_get_current_line() == "Second line", "native o did not extend the description")
assert(vim.deep_equal(marker_rows(), { 1, 3, 4 }), vim.inspect(marker_rows()))
assert(vim.bo.modified)
assert(vim.wait(1000, function() return not require("forge.editable").suspend_generated_text(state.replica.editable) end))
cursor(2)
assert(not vim.bo.modifiable, "read-only metadata was unlocked")
cursor(6)
keys("<C-S>")
assert(vim.wait(1000, function() return saves == 1 end))
save_callback("injected save rejection")
assert(vim.wait(1000, function() return not state.saving and not state.rendering end))
assert(vim.bo.modified and #marker_rows() == 3, "rejection cleared unsaved state")
keys("<C-S>")
assert(vim.wait(1000, function() return saves == 2 end))
cursor(1)
keys("A newer<Esc>")
save_callback()
assert(vim.wait(1000, function() return not state.saving and not state.rendering end))
assert(vim.bo.modified and vim.deep_equal(marker_rows(), { 1 }), "save cleared text typed after submission")
keys("A<C-S><Esc>")
assert(vim.wait(1000, function() return saves == 3 end))
save_callback()
assert(vim.wait(1000, function() return not vim.bo.modified and #marker_rows() == 0 end))
keys("A write<Esc>")
vim.cmd("write")
assert(vim.wait(1000, function() return saves == 4 end))
save_callback()
assert(vim.wait(1000, function() return not vim.bo.modified and #marker_rows() == 0 end))
local function completion(row, text, source)
  cursor(row)
  keys("A " .. text .. " <Esc>")
  local line = vim.api.nvim_get_current_line()
  vim.api.nvim_win_set_cursor(0, { row, #line - 1 })
  assert(source:enabled(), "completion source disabled in editable field")
  local result
  source:get_completions({}, function(value) result = value end)
  assert(vim.wait(1000, function() return result ~= nil end))
  return result.items
end
local users = require("forge.views.pr.reviewer_source").new()
assert(completion(1, "@al", users)[1].textEdit.newText == "@alice")
assert(completion(3, "@al", users)[1].textEdit.newText == "@alice")
assert(completion(6, "@al", users)[1].textEdit.newText == "@alice")
local issues = require("github.issue_source").new()
assert(completion(6, "#issue", issues)[1].textEdit.newText == "#42")
assert(warmed_users == 1 and warmed_issues == 1, "completion performed another remote refresh")
assert(actions == 0 and #errors == 1 and errors[1] == "injected save rejection", vim.inspect(errors))
require("forge.editable").flush(state.replica.editable)
assert(vim.wait(1000, function() return not require("forge.editable").suspend_generated_text(state.replica.editable) end))
vim.api.nvim_buf_delete(state.replica.buffer, { force = true })
adapter._set_runner_for_test(nil)
print("review_editing: native fields, save markers, rejection, @ mentions, and # issue search passed")
