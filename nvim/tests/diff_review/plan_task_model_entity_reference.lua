vim.loader.enable(false)

local function write_json(path, value)
  vim.fn.writefile({ vim.json.encode(value) }, path)
end

local fixture_dir = vim.fn.tempname()
local ok, failure = pcall(function()
  local task_model = require("diff_review.views.plan_review.task_model")

  vim.fn.mkdir(fixture_dir, "p")
  local working_path = vim.fs.joinpath(fixture_dir, "working.md")
  vim.fn.writefile({
    "# Inspect GeoParquet metadata with DataFusion",
    "",
    "# Tasks",
    "",
    "1. Own GeoParquet inspection through a typed report.",
    "",
    "   file hello/src/inspection.rs",
    "      └─ Create the inspector, report, and typed error boundary.",
    "",
    "# Tests",
  }, working_path)
  write_json(vim.fs.joinpath(fixture_dir, "working.json"), {
    entity_changes = {
      {
        entity_id = "entity_geo_parquet_inspector",
        action = "add",
        kind = "struct",
        name = "GeoParquetInspector",
        description = "Coordinates inspection.",
        path = "hello/src/inspection.rs",
      },
      {
        entity_id = "entity_inspection_report",
        action = "add",
        kind = "struct",
        name = "InspectionReport",
        description = "Owns inspection output.",
        path = "hello/src/inspection.rs",
      },
      {
        entity_id = "entity_inspection_error",
        action = "add",
        kind = "enum",
        name = "InspectionError",
        description = "Classifies inspection failures.",
        path = "hello/src/inspection.rs",
      },
    },
    tasks = {
      {
        task_id = "task_geo_parquet_inspection",
        title = "Own GeoParquet inspection through a typed report.",
        description = "",
        files = {
          {
            path = "hello/src/inspection.rs",
            subtasks = {
              {
                subtask_id = "subtask_inspection_boundary",
                operation = "create",
                description = "the inspector, report, and typed error boundary.",
                entities = {
                  "GeoParquetInspector",
                  "InspectionReport",
                  "InspectionError",
                },
              },
            },
          },
        },
      },
    },
  })
  write_json(vim.fs.joinpath(fixture_dir, "working.index.json"), { anchor = {} })

  local model, model_error = task_model.load(working_path)
  assert(model ~= nil, model_error)
  local subtask = model:task_nodes()[1].children[1].children[1]
  assert(
    #subtask.children == 3,
    ("expected three entity children from canonical entity references, got %d"):format(#subtask.children)
  )
end)

vim.fn.delete(fixture_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
