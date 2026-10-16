local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)))
  end
end

local entity_navigation = require("forge.views.plan_review.entity_navigation")

local review_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, {
  "Call count() on DataFrame",
  "Move here to cancel a pending jump",
})
vim.bo[review_buf].modified = false
local review_win = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(review_win, review_buf)
vim.api.nvim_win_set_cursor(review_win, { 1, 6 })

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

vim.fn.delete(workspace_root, "rf")
print("plan_review_entity_navigation: passed")
vim.cmd("qa!")
