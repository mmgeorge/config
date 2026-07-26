vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function find_row(buf, text)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(text, 1, true) then return row end
  end
  error("missing row containing " .. vim.inspect(text), 2)
end

local function row_has_highlight(buf, row, text, highlight)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local start_index, end_index = line:find(text, 1, true)
  if not start_index then return false end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, -1, { row - 1, 0 }, { row - 1, -1 }, {
    details = true,
  })) do
    local details = mark[4] or {}
    if details.hl_group == highlight
        and mark[3] <= start_index - 1
        and (details.end_col or mark[3]) >= end_index then
      return true
    end
  end
  return false
end

local function write_json(path, value)
  vim.fn.writefile({ vim.json.encode(value) }, path)
end

local fixture_dir = vim.fn.tempname()
local ok, failure = pcall(function()
  require("diff_review").setup({ harness = { backend = "mock" } })
  local comment_view = require("diff_review.views.plan_review.comment")
  local plan_fold = require("diff_review.views.plan_review.fold")
  local task_model = require("diff_review.views.plan_review.task_model")
  local task_tree = require("diff_review.render.task_tree")

  vim.fn.mkdir(fixture_dir, "p")
  local working_path = vim.fs.joinpath(fixture_dir, "working.md")
  local source_lines = {
    "# Fold plan tasks",
    "",
    "# Tasks",
    "",
    "1. **Own task rendering.** Keep plan structure semantic.",
    "",
    "   file src/plan.rs",
    "      └─ Create the shared renderer.",
    "         ├─ Add Resource `FirstOwner` — Retains a following sibling.",
    "         └─ Add Resource `FinalOwner` — Owns enough detail to wrap in a narrow review window.",
    "",
    "   file tests/plan_review.rs",
    "      └─ Add Integration test `renders_file_groups` — Keeps sibling file groups visually separate.",
    "",
    "# Tests",
    "",
    "## Unit tests",
  }
  vim.fn.writefile(source_lines, working_path)
  write_json(vim.fs.joinpath(fixture_dir, "working.json"), {
    entity_changes = {
      {
        entity_id = "first_owner",
        action = "add",
        kind = "resource",
        name = "FirstOwner",
        description = "Retains a following sibling.",
        path = "src/plan.rs",
      },
      {
        entity_id = "final_owner",
        action = "add",
        kind = "resource",
        name = "FinalOwner",
        description = "Owns enough detail to wrap in a narrow review window.",
        path = "src/plan.rs",
      },
    },
    tasks = {
      {
        task_id = "task",
        title = "Own task rendering.",
        description = "Keep FirstOwner references semantic.",
        files = {
          {
            path = "src/plan.rs",
            subtasks = {
              {
                subtask_id = "renderer",
                operation = "create",
                description = "the shared renderer.",
                entities = { "first_owner", "final_owner" },
              },
            },
          },
          {
            path = "tests/plan_review.rs",
            subtasks = {
              {
                subtask_id = "file_groups",
                operation = "test",
                action = "add",
                category = "integration",
                name = "renders_file_groups",
                behavior = "Keeps sibling file groups visually separate in a narrow review window.",
              },
            },
          },
        },
      },
    },
  })
  write_json(vim.fs.joinpath(fixture_dir, "working.index.json"), {
    anchor = {
      { line = 5, json_path = "/tasks/0", target = { target_type = "task", task_id = "task" } },
      {
        line = 7,
        json_path = "/tasks/0/files/0",
        path = "src/plan.rs",
        target = { target_type = "file", task_id = "task", path = "src/plan.rs" },
      },
      {
        line = 8,
        json_path = "/tasks/0/files/0/subtasks/0",
        path = "src/plan.rs",
        target = { target_type = "subtask", task_id = "task", path = "src/plan.rs", subtask_id = "renderer" },
      },
      {
        line = 9,
        json_path = "/entity_changes/0",
        path = "src/plan.rs",
        target = { target_type = "entity", entity_id = "first_owner" },
      },
      {
        line = 10,
        json_path = "/entity_changes/1",
        path = "src/plan.rs",
        target = { target_type = "entity", entity_id = "final_owner" },
      },
      {
        line = 12,
        json_path = "/tasks/0/files/1",
        path = "tests/plan_review.rs",
        target = { target_type = "file", task_id = "task", path = "tests/plan_review.rs" },
      },
      {
        line = 13,
        json_path = "/tasks/0/files/1/subtasks/0",
        path = "tests/plan_review.rs",
        target = {
          target_type = "subtask",
          task_id = "task",
          path = "tests/plan_review.rs",
          subtask_id = "file_groups",
        },
      },
    },
  })

  local model, model_error = task_model.load(working_path)
  assert_true(model ~= nil, model_error)
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  vim.api.nvim_win_set_width(win, 58)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, source_lines)
  local fold_controller = plan_fold.new()
  local annotation_list = {}
  comment_view.attach(buf, win, source_lines, annotation_list, {
    source_provider = function(width)
      return model:compose(task_tree.render(model:task_nodes(), width), width)
    end,
    before_render = function(render_buf, render_win)
      fold_controller:capture(render_buf, render_win)
    end,
    after_render = function(render_buf, render_win, projection)
      fold_controller:apply(render_buf, render_win, projection)
    end,
  })

  local task_row = find_row(buf, "1. Own task rendering.")
  assert_true(row_has_highlight(buf, task_row, "FirstOwner", "@type"),
    "task prose should highlight exact canonical entity references as types")
  assert_true(vim.fn.foldclosed(task_row) == task_row, "tasks should start folded")
  assert_equals(vim.wo[win].foldtext, "v:lua.diff_review_foldtext()",
    "plan folds should use the shared status fold text")
  assert_true(vim.wo[win].winhighlight:find("Folded:Normal", 1, true) ~= nil,
    "plan folds should use status-style folded-row highlighting")
  vim.api.nvim_win_set_cursor(win, { task_row, 0 })
  fold_controller:toggle(buf, win)
  assert_equals(vim.api.nvim_win_get_cursor(win)[1], task_row,
    "toggling a wrapped heading should preserve its visible title row")
  local first_file_row = find_row(buf, "   file src/plan.rs")
  assert_true(first_file_row > task_row, "file groups should remain visible beneath their task")
  assert_true(row_has_highlight(buf, first_file_row, "file", "DiffReviewFileKeyword"),
    "only the file keyword should use the shared yellow highlight")
  assert_true(row_has_highlight(buf, first_file_row, "src/plan.rs", "DiffReviewWalkthroughItemTitle"),
    "file paths should retain their existing title highlight")
  local second_file_row = find_row(buf, "   file tests/plan_review.rs")
  local separator_line = vim.api.nvim_buf_get_lines(buf, second_file_row - 2, second_file_row - 1, false)[1]
  assert_equals(separator_line, "", "sibling file groups should retain one blank separator row")
  local subtask_row = find_row(buf, "└─ Create the shared renderer.")
  local subtask_line = vim.api.nvim_buf_get_lines(buf, subtask_row - 1, subtask_row, false)[1]
  assert_true(subtask_line:find("   └─ ", 1, true) == 1,
    "subtasks should share their file group's task indentation")
  local test_row = find_row(buf, "└─ Add IntegrationTest `renders_file_groups`")
  local wrapped_test_line = vim.api.nvim_buf_get_lines(buf, test_row, test_row + 1, false)[1]
  assert_true(wrapped_test_line:find("      ", 1, true) == 1,
    "wrapped file children should align beneath their tree marker")
  assert_true(vim.fn.foldclosed(subtask_row) == -1,
    "opening a task should recursively reveal its complete subtask tree")
  vim.api.nvim_win_set_cursor(win, { subtask_row, 0 })
  fold_controller:toggle(buf, win)
  assert_true(vim.fn.foldclosed(subtask_row) == subtask_row,
    "subtasks should remain independently collapsible")
  fold_controller:toggle(buf, win)
  assert_true(vim.fn.foldclosed(subtask_row) == -1,
    "subtasks should reopen independently after a recursive task expansion")

  local first_owner_row = find_row(buf, "├─ Add Resource `FirstOwner`")
  local first_owner_line = vim.api.nvim_buf_get_lines(buf, first_owner_row - 1, first_owner_row, false)[1]
  assert_true(first_owner_line:find("      ├─ ", 1, true) == 1,
    "entity changes should indent beneath their subtask")
  assert_true(row_has_highlight(buf, first_owner_row, "Add", "DiffReviewWalkthroughActionAdd"),
    "plan entity actions should use the shared status highlight")
  assert_true(row_has_highlight(buf, first_owner_row, "Resource", "@keyword"),
    "plan entity kinds should use Tree-sitter keyword highlighting")
  assert_true(row_has_highlight(buf, first_owner_row, "FirstOwner", "@type"),
    "plan entity names should use Tree-sitter type highlighting")
  assert_true(row_has_highlight(buf, test_row, "IntegrationTest", "@type"),
    "integration test categories should render as one Tree-sitter type token")

  local final_owner_row = find_row(buf, "└─ Add Resource `FinalOwner`")
  local wrapped_owner_row = final_owner_row + 1
  local wrapped_owner_line = vim.api.nvim_buf_get_lines(buf, wrapped_owner_row - 1, wrapped_owner_row, false)[1]
  assert_true(wrapped_owner_line:find("         ", 1, true) == 1,
    "wrapped final entities should align without a dangling branch")
  assert_true(wrapped_owner_line:find("│", 1, true) == nil,
    "wrapped final entities should close their Unicode tree")

  local test_plan_row = find_row(buf, "# Tests")
  local test_plan_gap = vim.api.nvim_buf_get_lines(buf, test_plan_row - 2, test_plan_row - 1, false)[1]
  assert_equals(test_plan_gap, "", "the Tests heading should retain one blank separator row")

  vim.api.nvim_win_set_cursor(win, { wrapped_owner_row, 0 })
  comment_view.add_at_cursor(buf)
  vim.wait(20)
  vim.cmd("stopinsert")
  local header_row = find_row(buf, " Plan comment ")
  vim.api.nvim_buf_set_lines(buf, header_row, header_row + 1, false, { "Keep the semantic anchor" })
  assert_equals(comment_view.serialize(buf), {
    { line = 10, body = "Keep the semantic anchor" },
  }, "wrapped task comments should retain the canonical Markdown source line")

  comment_view.detach(buf)
  vim.api.nvim_buf_delete(buf, { force = true })
end)

vim.fn.delete(fixture_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
