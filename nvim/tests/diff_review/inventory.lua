local inventory = require("diff_review.infra.inventory")

local root = "D:/diffreview-inventory-root"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function fake_file(relpath, status, diff)
  return {
    filename = root .. "/" .. relpath,
    relpath = relpath,
    git_status = status,
    untracked = status == "??",
    hunks = diff and { { diff = diff } } or {},
  }
end

local function compute(opts)
  local result = nil
  inventory.compute_async(vim.tbl_extend("force", {
    cwd = root,
    git_list_async = function(command, cb)
      cb({}, 1, "unexpected git command: " .. table.concat(command, " "))
    end,
  }, opts), function(computed)
    result = computed
  end)
  wait_for(function() return result ~= nil end, "inventory did not compute")
  return result
end

local function rows_by_label(result)
  local rows = {}
  for _, row in ipairs(result.rows or {}) do
    rows[row.label] = row
  end
  return rows
end

local function detail_names(result, label, action)
  local names = {}
  for _, change in ipairs((((result.details or {})[label] or {})[action] or {})) do
    names[#names + 1] = change.name
  end
  table.sort(names)
  return table.concat(names, ",")
end

local function assert_counts(row, added, removed, modified, label)
  assert_true(row ~= nil, "missing row: " .. label)
  assert_true(row.added == added,
    ("%s added mismatch: expected %d, got %s"):format(label, added, tostring(row.added)))
  assert_true(row.removed == removed,
    ("%s removed mismatch: expected %d, got %s"):format(label, removed, tostring(row.removed)))
  assert_true(row.modified == modified,
    ("%s modified mismatch: expected %d, got %s"):format(label, modified, tostring(row.modified)))
end

local function rust_diff()
  return table.concat({
    "diff --git a/src/lib.rs b/src/lib.rs",
    "--- a/src/lib.rs",
    "+++ b/src/lib.rs",
    "@@ -1,8 +1,8 @@",
    " pub struct Cache {",
    "-  value: u32,",
    "+  value: u64,",
    " }",
    " ",
    " pub fn keep() {",
    "-  old_call();",
    "+  new_call();",
    " }",
    "-pub fn remove_me() {}",
    "+pub enum Mode { Fast }",
  }, "\n")
end

local function typescript_diff()
  return table.concat({
    "diff --git a/src/panel.ts b/src/panel.ts",
    "--- a/src/panel.ts",
    "+++ b/src/panel.ts",
    "@@ -1,8 +1,10 @@",
    " interface Props {",
    "   name: string;",
    "+  enabled: boolean;",
    " }",
    " class Panel {",
    "   render() {",
    "-    return oldValue;",
    "+    return newValue;",
    "   }",
    " }",
    "+type Mode = \"card\";",
  }, "\n")
end

local function run()
  local result = compute({
    sections = {
      {
        name = "unstaged",
        files = {
          fake_file("src/lib.rs", "M", rust_diff()),
          fake_file("src/panel.ts", "M", typescript_diff()),
          fake_file("docs/usage.md", "M", table.concat({
            "diff --git a/docs/usage.md b/docs/usage.md",
            "--- a/docs/usage.md",
            "+++ b/docs/usage.md",
            "@@ -1 +1 @@",
            "-old docs",
            "+new docs",
          }, "\n")),
          fake_file("plans/followup.md", "M", table.concat({
            "diff --git a/plans/followup.md b/plans/followup.md",
            "--- a/plans/followup.md",
            "+++ b/plans/followup.md",
            "@@ -1 +1 @@",
            "-old plan",
            "+new plan",
          }, "\n")),
          fake_file("blue/docs/rendering.md", "M", table.concat({
            "diff --git a/blue/docs/rendering.md b/blue/docs/rendering.md",
            "--- a/blue/docs/rendering.md",
            "+++ b/blue/docs/rendering.md",
            "@@ -1 +1 @@",
            "-old nested docs",
            "+new nested docs",
          }, "\n")),
          fake_file("blue/plans/implicit.md", "M", table.concat({
            "diff --git a/blue/plans/implicit.md b/blue/plans/implicit.md",
            "--- a/blue/plans/implicit.md",
            "+++ b/blue/plans/implicit.md",
            "@@ -1 +1 @@",
            "-old nested plan",
            "+new nested plan",
          }, "\n")),
        },
      },
    },
    old_lines_by_relpath = {
      ["src/lib.rs"] = {
        "pub struct Cache {",
        "  value: u32,",
        "}",
        "",
        "pub fn keep() {",
        "  old_call();",
        "}",
        "pub fn remove_me() {}",
      },
      ["src/panel.ts"] = {
        "interface Props {",
        "  name: string;",
        "}",
        "class Panel {",
        "  render() {",
        "    return oldValue;",
        "  }",
        "}",
      },
    },
    new_lines_by_relpath = {
      ["src/lib.rs"] = {
        "pub struct Cache {",
        "  value: u64,",
        "}",
        "",
        "pub fn keep() {",
        "  new_call();",
        "}",
        "pub enum Mode { Fast }",
      },
      ["src/panel.ts"] = {
        "interface Props {",
        "  name: string;",
        "  enabled: boolean;",
        "}",
        "class Panel {",
        "  render() {",
        "    return newValue;",
        "  }",
        "}",
        "type Mode = \"card\";",
      },
      ["docs/usage.md"] = { "new docs" },
      ["plans/followup.md"] = { "new plan" },
      ["blue/docs/rendering.md"] = { "new nested docs" },
      ["blue/plans/implicit.md"] = { "new nested plan" },
    },
  })
  local rows = rows_by_label(result)
  assert_counts(rows["function"], 0, 1, 2, "function")
  assert_counts(rows.struct, 0, 0, 1, "struct")
  assert_counts(rows.enum, 1, 0, 0, "enum")
  assert_counts(rows.interface, 0, 0, 1, "interface")
  assert_counts(rows.type, 1, 0, 0, "type")
  assert_true(rows.class == nil, "class should not be modified by a method-body-only change")
  assert_counts(rows.docs, 0, 0, 2, "docs")
  assert_counts(rows.plans, 0, 0, 2, "plans")
  assert_counts(rows.files, 0, 0, 6, "files")
  assert_true(detail_names(result, "function", "modified") == "Panel.render,keep",
    "function detail list should include modified functions")
  assert_true(detail_names(result, "function", "removed") == "remove_me",
    "function detail list should include removed functions")
  assert_true(detail_names(result, "enum", "added") == "Mode",
    "enum detail list should include added enum")
  assert_true(detail_names(result, "docs", "modified") == "blue/docs/rendering.md,docs/usage.md",
    "docs detail list should include root and nested docs files")
  assert_true(detail_names(result, "plans", "modified") == "blue/plans/implicit.md,plans/followup.md",
    "plans detail list should include root and nested plan files")
  assert_true(detail_names(result, "files", "modified") == "blue/docs/rendering.md,blue/plans/implicit.md,docs/usage.md,plans/followup.md,src/lib.rs,src/panel.ts",
    "files detail list should include changed files")

  local nested_file = fake_file("docs/nested.md", "M", nil)
  nested_file.filename = root .. "/workspace/docs/nested.md"
  result = compute({
    sections = { { name = "unstaged", files = { nested_file } } },
    repo_relative = function(filename)
      return filename:sub(#root + 2)
    end,
    old_lines_by_relpath = { ["workspace/docs/nested.md"] = { "old nested docs" } },
    new_lines_by_relpath = { ["workspace/docs/nested.md"] = { "new nested docs" } },
  })
  rows = rows_by_label(result)
  assert_counts(rows.docs, 0, 0, 1, "nested working-directory docs")
  assert_true(detail_names(result, "files", "modified") == "workspace/docs/nested.md",
    "inventory should derive repository-relative paths from absolute filenames")

  result = compute({
    sections = {
      {
        name = "unstaged",
        files = {
          fake_file("src/new_panel.tsx", "A", nil),
          fake_file("src/old.rs", "D", nil),
        },
      },
    },
    old_lines_by_relpath = {
      ["src/old.rs"] = {
        "pub struct OldState {",
        "  value: u32,",
        "}",
        "pub fn old_helper() {}",
      },
    },
    new_lines_by_relpath = {
      ["src/new_panel.tsx"] = {
        "class NewPanel {",
        "  render() {",
        "    return <div />;",
        "  }",
        "}",
      },
      ["src/old.rs"] = {},
    },
  })
  rows = rows_by_label(result)
  assert_counts(rows.class, 1, 0, 0, "added class")
  assert_counts(rows["function"], 1, 1, 0, "added and removed functions")
  assert_counts(rows.struct, 0, 1, 0, "removed struct")
  assert_counts(rows.files, 1, 1, 0, "added and removed files")
  assert_true(detail_names(result, "function", "added") == "NewPanel.render",
    "added file detail list should include added methods")
  assert_true(detail_names(result, "function", "removed") == "old_helper",
    "removed file detail list should include removed functions")
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
