vim.loader.enable(false)

local function write_json(path, value)
  vim.fn.writefile({ vim.json.encode(value) }, path)
end

local fixture_dir = vim.fn.tempname()
local ok, failure = pcall(function()
  local task_model = require("diff_review.views.plan_review.task_model")
  local entity_info = require("diff_review.views.plan_review.entity_info")

  vim.fn.mkdir(fixture_dir, "p")
  local working_path = vim.fs.joinpath(fixture_dir, "working.md")
  vim.fn.writefile({
    "# Inspect GeoParquet metadata with DataFusion",
    "",
    "Call validate() on PlanValidator",
    "Read geoparquet_metadata() from ParquetRecordBatchReaderBuilder",
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
    plan_id = "plan",
    version = 3,
    entity_changes = {
      {
        action = "add",
        kind = "struct",
        name = "GeoParquetInspector",
        description = "Coordinates inspection.",
        path = "hello/src/inspection.rs",
      },
      {
        action = "add",
        kind = "struct",
        name = "InspectionReport",
        description = "Owns inspection output.",
        path = "hello/src/inspection.rs",
      },
      {
        action = "add",
        kind = "enum",
        name = "InspectionError",
        description = "Classifies inspection failures.",
        path = "hello/src/inspection.rs",
      },
    },
    flows = {
      {
        steps = {
          {
            target = {
              kind = "planned_entity",
              entity = "GeoParquetInspector",
            },
            edges = {
              {
                relation = {
                  kind = "call",
                  callable = {
                    kind = "method",
                    name = "validate",
                  },
                },
                target = {
                  kind = "workspace_entity",
                  entity_kind = "type",
                  name = "PlanValidator",
                  path = "src/plan/validation.rs",
                  line = 76,
                },
              },
            },
          },
        },
      },
      {
        steps = {
          {
            target = {
              kind = "planned_entity",
              entity = "GeoParquetInspector",
            },
            edges = {
              {
                relation = {
                  kind = "read",
                  callable = {
                    kind = "method",
                    name = "geoparquet_metadata",
                  },
                },
                target = {
                  kind = "external_entity",
                  entity_kind = "type",
                  name = "ParquetRecordBatchReaderBuilder",
                  dependency = "parquet",
                },
              },
            },
          },
        },
      },
    },
    tasks = {
      {
        title = "Own GeoParquet inspection through a typed report.",
        description = "",
        files = {
          {
            path = "hello/src/inspection.rs",
            subtasks = {
              {
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
  write_json(vim.fs.joinpath(fixture_dir, "working.index.json"), {
    plan_id = "plan",
    plan_version = 3,
    anchor = {
      {
        line = 3,
        json_path = "/flows/0/steps/0/edges/0",
        target = {
          target_type = "flow_edge",
          callable_kind = "method",
          callable_name = "validate",
          reference_kind = "workspace_entity",
          target_name = "PlanValidator",
          target_is_type = true,
          workspace_path = "src/plan/validation.rs",
          workspace_line = 76,
        },
      },
      {
        line = 4,
        json_path = "/flows/1/steps/0/edges/0",
        target = {
          target_type = "flow_edge",
          callable_kind = "method",
          callable_name = "geoparquet_metadata",
          reference_kind = "external_entity",
          target_name = "ParquetRecordBatchReaderBuilder",
          target_is_type = true,
        },
      },
    },
  })

  local model, model_error = task_model.load(working_path)
  assert(model ~= nil, model_error)
  local subtask = model:task_nodes()[1].children[1].children[1]
  assert(
    #subtask.children == 3,
    ("expected three entity children from canonical entity references, got %d"):format(#subtask.children)
  )
  local flow_line = "Call validate() on PlanValidator"
  local target = model:workspace_target_at_position(3, flow_line, assert(flow_line:find("PlanValidator")) - 1)
  assert(target ~= nil, "workspace flow target should resolve from the canonical navigation anchor")
  assert(target.name == "PlanValidator", "workspace flow target should retain its semantic name")
  assert(target.path == "src/plan/validation.rs", "workspace flow target should retain its repository path")
  assert(target.line == 76, "workspace flow target should retain its declaration line")
  assert(
    model:rustdoc_target_at_position(3, flow_line, assert(flow_line:find("validate")) - 1) == nil,
    "workspace callables must not route through external Rustdoc resolution"
  )
  local rustdoc_line = "Read geoparquet_metadata() from ParquetRecordBatchReaderBuilder"
  assert(vim.deep_equal(
    model:rustdoc_target_at_position(
      4,
      rustdoc_line,
      assert(rustdoc_line:find("geoparquet_metadata", 1, true)) - 1
    ),
    {
      json_path = "/flows/1/steps/0/edges/0",
      selection = "callable",
    }
  ), "external callable should resolve from the rendered external-entity anchor")
  assert(vim.deep_equal(
    model:rustdoc_target_at_position(
      4,
      rustdoc_line,
      assert(rustdoc_line:find("ParquetRecordBatchReaderBuilder", 1, true)) - 1
    ),
    {
      json_path = "/flows/1/steps/0/edges/0",
      selection = "receiver",
    }
  ), "external receiver should resolve from the rendered external-entity anchor")

  local review_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(review_buf, 0, -1, false, { rustdoc_line })
  local review_win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(review_win, review_buf)
  vim.api.nvim_win_set_cursor(
    review_win,
    { 1, assert(rustdoc_line:find("geoparquet_metadata", 1, true)) - 1 }
  )
  local requested_target = nil
  assert(entity_info.show_context(model, review_buf, review_win, {
    source_line = 4,
    plan_id = "plan",
    expected_version = 3,
    request = function(target)
      requested_target = target
    end,
  }), "PlanReview ol should route an external callable to Rustdoc")
  assert(vim.deep_equal(requested_target, {
    json_path = "/flows/1/steps/0/edges/0",
    selection = "callable",
    plan_id = "plan",
    expected_version = 3,
  }), "PlanReview ol should request Rustdoc for the canonical external flow edge")
end)

vim.fn.delete(fixture_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
