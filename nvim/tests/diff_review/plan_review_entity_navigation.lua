local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
  end
end

local entity_navigation = require("diff_review.views.plan_review.entity_navigation")

local review_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, {
  "Call count() on DataFrame",
  "Move here to cancel a pending jump",
})
vim.bo[review_buf].modified = false
local review_win = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(review_win, review_buf)
vim.api.nvim_win_set_cursor(review_win, { 1, 6 })

local model = {
  entity_at_position = function() return nil end,
  workspace_target_at_position = function() return nil end,
  rustdoc_target_at_position = function(_, source_line, _, _)
    assert_equals(source_line, 42, "Rust source navigation should retain the canonical source line")
    return {
      json_path = "/flows/0/steps/0/edges/0",
      selection = "callable",
    }
  end,
}
local captured_params = nil
local source_callback = nil
assert_equals(
  entity_navigation.jump(model, review_buf, review_win, {
    source_line = 42,
    plan_id = "plan",
    expected_version = 7,
    workspace_root = vim.fn.getcwd(),
    request = function(params, callback)
      captured_params = params
      source_callback = callback
    end,
  }),
  true,
  "External Rust navigation should claim a semantic flow symbol"
)
assert_equals(captured_params.plan_id, "plan", "Rust source navigation should send the plan identity")
assert_equals(captured_params.expected_version, 7, "Rust source navigation should guard the plan version")
assert_equals(captured_params.selection, "callable", "Rust source navigation should preserve token selection")

local source_path = vim.fn.tempname() .. ".rs"
vim.fn.writefile({
  "pub struct DataFrame;",
  "    pub async fn count(&self) -> usize { 0 }",
}, source_path)
source_callback({
  package = "datafusion",
  version = "54.1.0",
  path = source_path,
  line = 2,
  column = 5,
})
assert_equals(
  vim.wait(1000, function()
    return vim.api.nvim_buf_get_name(vim.api.nvim_win_get_buf(review_win)) == source_path
  end, 10),
  true,
  "Rust source navigation should open the resolved source file"
)
local source_buf = vim.api.nvim_win_get_buf(review_win)
assert_equals(vim.bo[source_buf].readonly, true, "Cargo-owned Rust source should open read-only")
assert_equals(vim.bo[source_buf].modifiable, false, "Cargo-owned Rust source should reject edits")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win),
  { 2, 4 },
  "Rust source navigation should convert the Rustdoc position to a Neovim cursor"
)
vim.cmd("normal! \15")
assert_equals(
  vim.api.nvim_win_get_buf(review_win),
  review_buf,
  "The jumplist should return to PlanReview"
)
assert_equals(
  vim.api.nvim_win_get_cursor(review_win),
  { 1, 6 },
  "The jumplist should restore the originating plan token"
)

local workspace_root = vim.fn.tempname()
vim.fn.mkdir(vim.fs.joinpath(workspace_root, "src"), "p")
local workspace_path = vim.fs.joinpath(workspace_root, "src", "cache.rs")
vim.fn.writefile({
  "pub struct WorkspaceCache;",
  "impl WorkspaceCache {}",
}, workspace_path)
vim.api.nvim_buf_set_lines(review_buf, 0, 1, false, { "Call load() on WorkspaceCache" })
vim.api.nvim_win_set_cursor(review_win, { 1, 18 })
local workspace_request_count = 0
local workspace_model = {
  workspace_target_at_position = function(_, source_line, _, _)
    assert_equals(source_line, 42, "Workspace navigation should retain the canonical source line")
    return {
      name = "WorkspaceCache",
      path = "src/cache.rs",
      line = 1,
    }
  end,
  entity_at_position = function()
    error("An anchored workspace entity should resolve before lexical planned entities")
  end,
}
assert_equals(
  entity_navigation.jump(workspace_model, review_buf, review_win, {
    source_line = 42,
    plan_id = "plan",
    expected_version = 7,
    workspace_root = workspace_root,
    request = function() workspace_request_count = workspace_request_count + 1 end,
  }),
  true,
  "Workspace navigation should claim an unchanged repository entity"
)
assert_equals(workspace_request_count, 0, "Workspace navigation should not issue a Rustdoc request")
local workspace_buf = vim.api.nvim_win_get_buf(review_win)
assert_equals(
  vim.api.nvim_buf_get_name(workspace_buf),
  workspace_path,
  "Workspace navigation should open the repository-relative source path"
)
assert_equals(vim.bo[workspace_buf].modifiable, true, "Workspace source should remain editable")
assert_equals(
  vim.api.nvim_win_get_cursor(review_win),
  { 1, 11 },
  "Workspace navigation should select the named declaration"
)
vim.cmd("normal! \15")
assert_equals(vim.api.nvim_win_get_buf(review_win), review_buf, "Workspace navigation should push the jumplist")

vim.api.nvim_win_set_cursor(review_win, { 1, 6 })
local stale_callback = nil
entity_navigation.jump(model, review_buf, review_win, {
  source_line = 42,
  plan_id = "plan",
  expected_version = 7,
  workspace_root = vim.fn.getcwd(),
  request = function(_, callback) stale_callback = callback end,
})
vim.api.nvim_win_set_cursor(review_win, { 2, 0 })
stale_callback({
  path = source_path,
  line = 2,
  column = 5,
})
vim.wait(50)
assert_equals(
  vim.api.nvim_win_get_buf(review_win),
  review_buf,
  "A Rust source response should not navigate after the review cursor moves"
)

vim.fn.delete(source_path)
vim.fn.delete(workspace_root, "rf")
vim.cmd("qa!")
