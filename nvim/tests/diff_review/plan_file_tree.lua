vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function write_json(path, value)
  vim.fn.writefile({ vim.json.encode(value) }, path)
end

local function find_projection_row(projection, text)
  for row, value in ipairs(projection) do
    if value.text:find(text, 1, true) then return row, value end
  end
  error("missing projection row containing " .. vim.inspect(text), 2)
end

local fixture_directory = vim.fn.tempname()
local ok, failure = pcall(function()
  local plan_fold = require("diff_review.views.plan_review.fold")
  local task_model = require("diff_review.views.plan_review.task_model")

  vim.fn.mkdir(fixture_directory, "p")
  local working_path = vim.fs.joinpath(fixture_directory, "working.md")
  local source_line_list = {
    "# File inventory",
    "",
    "# Files",
    "",
    "hello",
    "├─ Cargo.toml",
    "└─ src",
    "   └─ (new) inspection.rs",
    "      ├─ (new) struct GeoParquetInspector",
    "      └─ LegacyReport → InspectionReport",
    "",
    "# Tasks",
    "",
    "# Tests",
  }
  vim.fn.writefile(source_line_list, working_path)
  write_json(vim.fs.joinpath(fixture_directory, "working.json"), {
    plan_id = "plan",
    version = 1,
    entity_changes = {
      {
        action = "add",
        kind = "struct",
        name = "GeoParquetInspector",
        description = "Owns inspection.",
        path = "hello/src/inspection.rs",
      },
      {
        action = "rename",
        renamed_from = "LegacyReport",
        kind = "struct",
        name = "InspectionReport",
        description = "Carries output.",
        path = "hello/src/inspection.rs",
      },
    },
    tasks = {},
  })
  write_json(vim.fs.joinpath(fixture_directory, "working.index.json"), {
    plan_id = "plan",
    plan_version = 1,
    anchor = {
      {
        line = 5,
        json_path = "/tasks",
        target = { target_type = "file_directory", path = "hello" },
      },
      {
        line = 6,
        json_path = "/tasks",
        target = { target_type = "file_tree_file", path = "hello/Cargo.toml" },
      },
      {
        line = 7,
        json_path = "/tasks",
        target = { target_type = "file_directory", path = "hello/src" },
      },
      {
        line = 8,
        json_path = "/tasks",
        target = { target_type = "file_tree_file", path = "hello/src/inspection.rs" },
      },
      {
        line = 9,
        json_path = "/entity_changes/0",
        target = {
          target_type = "file_tree_entity",
          name = "GeoParquetInspector",
          path = "hello/src/inspection.rs",
        },
      },
      {
        line = 10,
        json_path = "/entity_changes/1",
        target = {
          target_type = "file_tree_entity",
          name = "InspectionReport",
          path = "hello/src/inspection.rs",
        },
      },
    },
  })

  local model, model_error = task_model.load(working_path)
  assert_true(model ~= nil, model_error)
  local projection = model:compose({}, 80)
  local root_row, root = find_projection_row(projection, "hello")
  local directory_row, directory = find_projection_row(projection, "└─ src")
  local file_row, file = find_projection_row(projection, "(new) inspection.rs")
  local symbol_row, symbol = find_projection_row(projection, "(new) struct GeoParquetInspector")
  local rename_row, rename = find_projection_row(projection, "LegacyReport → InspectionReport")

  assert_equals(root.fold_id, "file-directory:hello", "root directories should own stable folds")
  assert_equals(directory.fold_id, "file-directory:hello/src", "nested directories should own stable folds")
  assert_equals(file.fold_id, "file-entry:hello/src/inspection.rs", "files should own symbol folds")
  assert_equals(file.default_folded, true, "file symbol lists should start folded")
  assert_equals(symbol.ancestor_ids[#symbol.ancestor_ids], file.fold_id, "symbols should descend from their file")
  assert_equals(rename.ancestor_ids[#rename.ancestor_ids], file.fold_id, "renamed symbols should descend from their file")

  local buffer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, buffer)
  local line_list = vim.tbl_map(function(row) return row.text end, projection)
  local fold_projection = { line_list = line_list, line_meta_list = projection }
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, line_list)
  local controller = plan_fold.new()
  controller:apply(buffer, window, fold_projection)
  assert_equals(vim.fn.foldclosed(root_row), -1, "directory folds should start open")
  assert_equals(vim.fn.foldclosed(file_row), file_row, "file symbol folds should start closed")

  vim.api.nvim_win_set_cursor(window, { file_row, 0 })
  controller:toggle(buffer, window)
  assert_equals(vim.fn.foldclosed(file_row), -1, "toggling a file should reveal every symbol")

  controller:capture(buffer, window)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, line_list)
  controller:apply(buffer, window, fold_projection)
  assert_equals(vim.fn.foldclosed(file_row), -1, "file expansion should survive projection rerenders")
  assert_true(directory_row > root_row and symbol_row > file_row and rename_row > symbol_row,
    "file inventory rows should preserve tree order")
end)

vim.fn.delete(fixture_directory, "rf")
if not ok then error(failure, 0) end
print("plan file tree tests passed")
vim.cmd("qa!")
