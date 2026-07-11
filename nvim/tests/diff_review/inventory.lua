local inventory = require("diff_review.infra.inventory")

local root = "D:/diffreview-inventory-root"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

---@param relpath string
---@param status string
---@return table
local function fake_file(relpath, status)
  return {
    filename = root .. "/" .. relpath,
    relpath = relpath,
    git_status = status,
    untracked = status == "??",
    hunks = {},
  }
end

---@param result DiffReviewInventoryResult
---@return table<string, DiffReviewInventoryRow>
local function rows_by_label(result)
  local rows = {}
  for _, row in ipairs(result.rows or {}) do
    rows[row.label] = row
  end
  return rows
end

---@param result DiffReviewInventoryResult
---@param label string
---@param action "added"|"removed"|"modified"
---@return string
local function detail_names(result, label, action)
  local names = {}
  for _, change in ipairs((((result.details or {})[label] or {})[action] or {})) do
    names[#names + 1] = change.name
  end
  table.sort(names)
  return table.concat(names, ",")
end

---@param row DiffReviewInventoryRow?
---@param added integer
---@param removed integer
---@param modified integer
---@param label string
local function assert_counts(row, added, removed, modified, label)
  assert_true(row ~= nil, "missing row: " .. label)
  assert_true(row.added == added,
    ("%s added mismatch: expected %d, got %s"):format(label, added, tostring(row.added)))
  assert_true(row.removed == removed,
    ("%s removed mismatch: expected %d, got %s"):format(label, removed, tostring(row.removed)))
  assert_true(row.modified == modified,
    ("%s modified mismatch: expected %d, got %s"):format(label, modified, tostring(row.modified)))
end

local function run()
  local commands = {}
  local result
  local tracked_json = vim.json.encode({
    summary = { fileCount = 3 },
    changes = {
      {
        entityId = "src/lib.rs::fn::run",
        changeType = "modified",
        entityType = "function",
        entityName = "run",
        filePath = "src/lib.rs",
        startLine = 8,
      },
      {
        entityId = "src/lib.rs::struct::Mode",
        changeType = "added",
        entityType = "struct",
        entityName = "Mode",
        filePath = "src/lib.rs",
        startLine = 2,
      },
      {
        entityId = "src/old.rs::enum::Legacy",
        changeType = "deleted",
        entityType = "enum",
        entityName = "Legacy",
        filePath = "src/old.rs",
        oldStartLine = 4,
      },
      {
        entityId = "docs/usage.md::orphan::module-level",
        changeType = "modified",
        entityType = "orphan",
        entityName = "module-level",
        filePath = "docs/usage.md",
      },
    },
  })
  local untracked_json = vim.json.encode({
    {
      file = root .. "/src/new.rs",
      type = "struct",
      name = "NewState",
      start_line = 1,
      end_line = 3,
    },
    {
      file = root .. "/src/new.rs",
      type = "function",
      name = "create",
      start_line = 5,
      end_line = 7,
    },
  })

  inventory.compute_async({
    cwd = root,
    sections = {
      {
        name = "unstaged",
        files = {
          fake_file("src/lib.rs", "M"),
          fake_file("src/old.rs", "D"),
          fake_file("docs/usage.md", "M"),
          fake_file("src/new.rs", "??"),
          fake_file("assets/unsupported.bin", "??"),
        },
      },
    },
    sem_available = function() return true end,
    repo_relative = function(filename)
      return filename:sub(#root + 2)
    end,
    run_text_async = function(command, _, cb)
      commands[#commands + 1] = command
      vim.schedule(function()
        local stdout = command[2] == "diff" and tracked_json or untracked_json
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      end)
    end,
  }, function(computed)
    result = computed
  end)

  wait_for(function() return result ~= nil end, "Sem inventory did not compute")
  assert_true(#commands == 2, "Sem inventory should run one diff and one batched entities command")
  assert_true(commands[1][1] == "sem" and commands[1][2] == "diff",
    "tracked inventory should come from sem diff")
  assert_true(commands[2][1] == "sem" and commands[2][2] == "entities",
    "untracked inventory should come from sem entities")

  local rows = rows_by_label(result)
  assert_counts(rows["function"], 1, 0, 1, "function")
  assert_counts(rows.struct, 2, 0, 0, "struct")
  assert_counts(rows.enum, 0, 1, 0, "enum")
  assert_counts(rows.docs, 0, 0, 1, "docs")
  assert_counts(rows.files, 1, 1, 2, "files")
  assert_true(detail_names(result, "function", "added") == "create",
    "untracked Sem entities should be added symbols")
  assert_true(detail_names(result, "function", "modified") == "run",
    "tracked Sem changes should preserve modified symbols")
  assert_true(detail_names(result, "files", "added") == "src/new.rs",
    "unsupported untracked files should be absent when Sem returns no entities")
  assert_true(not vim.inspect(commands):find("git", 1, true),
    "Sem inventory must not invoke the removed native Git inventory")

  local unavailable_result
  local unavailable_calls = 0
  inventory.compute_async({
    cwd = root,
    sections = {},
    sem_available = function() return false end,
    run_text_async = function()
      unavailable_calls = unavailable_calls + 1
    end,
  }, function(computed)
    unavailable_result = computed
  end)
  assert_true(unavailable_result ~= nil and unavailable_result.error:find("requires Sem", 1, true) ~= nil,
    "missing Sem should return a visible inventory error")
  assert_true(unavailable_calls == 0, "missing Sem should not invoke another inventory backend")

  local failed_result
  inventory.compute_async({
    cwd = root,
    sections = {},
    sem_available = function() return true end,
    run_text_async = function(_, _, cb)
      cb({ code = 1, stdout = "", stderr = "parse failed", output = "parse failed" })
    end,
  }, function(computed)
    failed_result = computed
  end)
  assert_true(failed_result ~= nil and failed_result.error:find("parse failed", 1, true) ~= nil,
    "Sem failures should surface the underlying error without native fallback")
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
