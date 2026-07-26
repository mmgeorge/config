local M = {}

local task_tree_style = require("diff_review.render.task_tree_style")
local uml_style = require("diff_review.views.plan_review.uml_style")

---@class DiffReviewPlanTaskModel
---@field working_path string
---@field source_lines string[]
---@field document table
---@field navigation table
---@field task_heading_line integer
---@field test_heading_line integer
---@field anchor_by_json_path table<string, table>
---@field anchor_by_source_line table<integer, table>
---@field entity_name_set table<string, boolean>
local PlanTaskModel = {}
PlanTaskModel.__index = PlanTaskModel

local entity_kind_label = {
  abstract_class = "abstract class",
  app = "app",
  class = "class",
  config = "config",
  enum = "enum",
  field = "field",
  ["function"] = "fn",
  interface = "interface",
  method = "method",
  resource = "Resource",
  cache = "Cache",
  adapter = "Adapter",
  constant = "constant",
  struct = "struct",
  trait = "trait",
}

---@param value string?
---@return string
local function title_case(value)
  local text = tostring(value or ""):gsub("_", " ")
  return text:gsub("^%l", string.upper)
end

---@param path string
---@return string[]?, string?
local function read_lines(path)
  local ok, line_list = pcall(vim.fn.readfile, path)
  if not ok then return nil, ("Failed to read %s"):format(path) end
  return line_list, nil
end

---@param path string
---@return table?, string?
local function read_json(path)
  local line_list, read_error = read_lines(path)
  if not line_list then return nil, read_error end
  local ok, value = pcall(vim.json.decode, table.concat(line_list, "\n"))
  if not ok or type(value) ~= "table" then return nil, ("Failed to decode %s"):format(path) end
  return value, nil
end

---@param line_list string[]
---@param heading string
---@return integer?
local function heading_line(line_list, heading)
  for line_index, line in ipairs(line_list) do
    if line == heading then return line_index end
  end
  return nil
end

---@param navigation table
---@return table<string, table>
local function index_anchor_by_json_path(navigation)
  local result = {}
  for _, anchor in ipairs(navigation.anchor or {}) do
    if type(anchor.json_path) == "string" and result[anchor.json_path] == nil then
      result[anchor.json_path] = anchor
    end
  end
  return result
end

---@param navigation table
---@return table<integer, table>
local function index_anchor_by_source_line(navigation)
  local result = {}
  for _, anchor in ipairs(navigation.anchor or {}) do
    local source_line = tonumber(anchor.line)
    if source_line and result[source_line] == nil then result[source_line] = anchor end
  end
  return result
end

---@param model DiffReviewPlanTaskModel
---@param source_line integer
---@param width integer?
---@return table
local function source_row(model, source_line, width)
  local anchor = model.anchor_by_source_line[source_line]
  local target = anchor and anchor.target or nil
  local text = uml_style.align_owner(model.source_lines[source_line] or "", target, width)
  local segments = uml_style.segments(text, target)
  if target and target.target_type == "dependency_manifest" then
    segments = task_tree_style.file(tostring(target.manifest or ""))(text, 1)
  elseif source_line >= model.test_heading_line then
    local test_path = text:match("^file%s+(.+)$")
    if test_path then segments = task_tree_style.file(test_path)(text, 1) end
  end
  return {
    id = ("plan:source:%d"):format(source_line),
    text = text,
    source_line = source_line,
    ancestor_ids = {},
    segments = segments,
  }
end

---@param model DiffReviewPlanTaskModel
---@param json_path string
---@return table
local function node_source(model, json_path)
  local anchor = model.anchor_by_json_path[json_path] or {}
  return {
    source_line = tonumber(anchor.line),
    target = anchor.target,
    json_path = json_path,
    path = anchor.path,
  }
end

---@param source table
---@return table
local function source_fields(source)
  return {
    source_line = source.source_line,
    target = source.target,
    json_path = source.json_path,
    path = source.path,
  }
end

---@param entity table
---@param entity_name_set table<string, boolean>
---@return table
local function entity_node_fields(entity, entity_name_set)
  local action = title_case(entity.action)
  local kind = entity_kind_label[entity.kind] or tostring(entity.kind or "entity")
  local name = tostring(entity.name or "")
  return {
    text = ("%s %s `%s` — %s"):format(action, kind, name, tostring(entity.description or "")),
    segments_for_line = task_tree_style.entity_references(
      task_tree_style.change(action, kind, name),
      entity_name_set
    ),
  }
end

---@param test table
---@param entity_name_set table<string, boolean>
---@return table
local function test_node_fields(test, entity_name_set)
  local action = title_case(test.action)
  local kind = title_case(test.category) .. "Test"
  local name = tostring(test.name or "")
  return {
    text = ("%s %s `%s` — %s"):format(action, kind, name, tostring(test.behavior or "")),
    segments_for_line = task_tree_style.entity_references(
      task_tree_style.change(
        action,
        kind,
        name,
        "@type",
        "DiffReviewWalkthroughItemTitle"
      ),
      entity_name_set
    ),
  }
end

---@return DiffReviewTaskTreeNode[]
function PlanTaskModel:task_nodes()
  local entity_by_reference = {}
  local entity_index_by_reference = {}
  for entity_index, entity in ipairs(self.document.entity_changes or {}) do
    for _, entity_reference in ipairs({ entity.entity_id, entity.name }) do
      if type(entity_reference) == "string" and entity_reference ~= "" then
        entity_by_reference[entity_reference] = entity
        entity_index_by_reference[entity_reference] = entity_index - 1
      end
    end
  end

  local task_node_list = {}
  for task_index, task in ipairs(self.document.tasks or {}) do
    local task_title = tostring(task.title or "")
    local task_json_path = ("/tasks/%d"):format(task_index - 1)
    local task_source = node_source(self, task_json_path)
    local file_node_list = {}
    for file_index, file in ipairs(task.files or {}) do
      local file_json_path = ("%s/files/%d"):format(task_json_path, file_index - 1)
      local file_source = node_source(self, file_json_path)
      local subtask_node_list = {}
      for subtask_index, subtask in ipairs(file.subtasks or {}) do
        local subtask_json_path = ("%s/subtasks/%d"):format(file_json_path, subtask_index - 1)
        local subtask_source = node_source(self, subtask_json_path)
        if subtask.operation == "test" then
          local test_node = vim.tbl_extend("force", {
            id = ("plan:task:%d:file:%d:test:%d"):format(task_index, file_index, subtask_index),
            branch = true,
            foldable = false,
          }, test_node_fields(subtask, self.entity_name_set), source_fields(subtask_source))
          subtask_node_list[#subtask_node_list + 1] = test_node
        else
          local entity_node_list = {}
          for entity_position, entity_id in ipairs(subtask.entities or {}) do
            local entity = entity_by_reference[entity_id]
            if entity then
              local entity_json_path = ("/entity_changes/%d"):format(entity_index_by_reference[entity_id])
              local entity_source = node_source(self, entity_json_path)
              entity_node_list[#entity_node_list + 1] = vim.tbl_extend("force", {
                id = ("plan:entity:%s:%d"):format(tostring(entity_id), entity_position),
                branch = true,
                foldable = false,
              }, entity_node_fields(entity, self.entity_name_set), source_fields(entity_source))
            end
          end
          subtask_node_list[#subtask_node_list + 1] = vim.tbl_extend("force", {
            id = ("plan:task:%d:file:%d:subtask:%d"):format(task_index, file_index, subtask_index),
            text = ("%s %s"):format(title_case(subtask.operation), tostring(subtask.description or "")),
            segments_for_line = task_tree_style.entity_references(nil, self.entity_name_set),
            branch = true,
            foldable = #entity_node_list > 0,
            default_folded = true,
            children = entity_node_list,
          }, source_fields(subtask_source))
        end
      end
      file_node_list[#file_node_list + 1] = vim.tbl_extend("force", {
        id = ("plan:task:%d:file:%d"):format(task_index, file_index),
        text = "file " .. tostring(file.path or ""),
        segments_for_line = task_tree_style.file(tostring(file.path or "")),
        branch = false,
        foldable = false,
        child_prefix = "",
        gap_after = file_index < #(task.files or {}),
        children = subtask_node_list,
      }, source_fields(file_source))
    end
    task_node_list[#task_node_list + 1] = vim.tbl_extend("force", {
      id = ("plan:task:%d"):format(task_index),
      text = ("%s %s"):format(task_title, tostring(task.description or "")),
      segments_for_line = task_tree_style.entity_references(
        task_tree_style.task(("%d. %s"):format(task_index, task_title)),
        self.entity_name_set
      ),
      branch = false,
      first_prefix = ("%d. "):format(task_index),
      continuation_prefix = "   ",
      foldable = #file_node_list > 0,
      default_folded = true,
      gap_before_children = true,
      gap_after = task_index < #(self.document.tasks or {}),
      children = file_node_list,
    }, source_fields(task_source))
  end
  return task_node_list
end

---@param task_row_list DiffReviewTaskTreeRow[]
---@param width integer?
---@return table[]
function PlanTaskModel:compose(task_row_list, width)
  local source_row_list = {}
  for source_line = 1, self.task_heading_line do
    source_row_list[#source_row_list + 1] = source_row(self, source_line, width)
  end
  source_row_list[#source_row_list + 1] = {
    id = "plan:tasks:gap",
    text = "",
    source_line = self.task_heading_line + 1,
    ancestor_ids = {},
  }
  vim.list_extend(source_row_list, task_row_list)
  source_row_list[#source_row_list + 1] = {
    id = "plan:test-plan:gap",
    text = "",
    source_line = self.test_heading_line - 1,
    ancestor_ids = {},
  }
  for source_line = self.test_heading_line, #self.source_lines do
    source_row_list[#source_row_list + 1] = source_row(self, source_line, width)
  end
  return source_row_list
end

---@param working_path string
---@return DiffReviewPlanTaskModel?, string?
function M.load(working_path)
  local source_lines, markdown_error = read_lines(working_path)
  if not source_lines then return nil, markdown_error end
  local directory = vim.fs.dirname(working_path)
  local document, document_error = read_json(vim.fs.joinpath(directory, "working.json"))
  if not document then return nil, document_error end
  local navigation, navigation_error = read_json(vim.fs.joinpath(directory, "working.index.json"))
  if not navigation then return nil, navigation_error end
  local task_heading_line = heading_line(source_lines, "# Tasks")
  local test_heading_line = heading_line(source_lines, "# Tests")
  if not task_heading_line or not test_heading_line or test_heading_line <= task_heading_line then
    return nil, "Plan projection does not contain ordered Tasks and Tests sections"
  end
  local entity_name_set = {}
  for _, entity in ipairs(document.entity_changes or {}) do
    if type(entity.name) == "string" and entity.name ~= "" then entity_name_set[entity.name] = true end
  end
  return setmetatable({
    working_path = working_path,
    source_lines = source_lines,
    document = document,
    navigation = navigation,
    task_heading_line = task_heading_line,
    test_heading_line = test_heading_line,
    anchor_by_json_path = index_anchor_by_json_path(navigation),
    anchor_by_source_line = index_anchor_by_source_line(navigation),
    entity_name_set = entity_name_set,
  }, PlanTaskModel), nil
end

return M
