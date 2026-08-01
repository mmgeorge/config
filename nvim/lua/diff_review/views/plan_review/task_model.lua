local M = {}

local task_tree_style = require("diff_review.render.task_tree_style")
local uml_style = require("diff_review.views.plan_review.uml_style")

---@class DiffReviewPlanTaskModel
---@field working_path string
---@field source_lines string[]
---@field document table
---@field navigation table
---@field plan_id string
---@field plan_version integer
---@field task_heading_line integer
---@field test_heading_line integer
---@field anchor_by_json_path table<string, table>
---@field anchor_by_source_line table<integer, table>
---@field entity_name_set table<string, boolean>
---@field entity_source_line_by_name table<string, integer>
local PlanTaskModel = {}
PlanTaskModel.__index = PlanTaskModel

---@class DiffReviewPlanDescribedItem
---@field name string
---@field description string

---@class DiffReviewPlanEntity: DiffReviewPlanDescribedItem
---@field action "add"|"modify"|"remove"
---@field members DiffReviewPlanDescribedItem[]
---@field variants DiffReviewPlanDescribedItem[]

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

---@param character string
---@return boolean
local function is_identifier_character(character)
  return character ~= "" and character:match("[%w_]") ~= nil
end

---@param line string
---@param first_byte integer
---@param last_byte integer
---@param name string
---@return boolean
local function has_entity_boundaries(line, first_byte, last_byte, name)
  local first_character = name:sub(1, 1)
  local last_character = name:sub(-1)
  if is_identifier_character(first_character)
      and is_identifier_character(line:sub(first_byte - 1, first_byte - 1)) then
    return false
  end
  if is_identifier_character(last_character)
      and is_identifier_character(line:sub(last_byte + 1, last_byte + 1)) then
    return false
  end
  return true
end

---@param line string
---@param byte_col integer Zero-based byte column.
---@param name string?
---@return boolean
local function occupies_identifier(line, byte_col, name)
  if type(name) ~= "string" or name == "" then return false end
  local cursor_byte = byte_col + 1
  local search_byte = 1
  while true do
    local first_byte, last_byte = line:find(name, search_byte, true)
    if not first_byte then return false end
    if cursor_byte >= first_byte and cursor_byte <= last_byte
        and has_entity_boundaries(line, first_byte, last_byte, name) then
      return true
    end
    search_byte = first_byte + 1
  end
end

---@param document table
---@param line string
---@param byte_col integer Zero-based byte column.
---@return DiffReviewPlanEntity?
function M.entity_at_position(document, line, byte_col)
  local cursor_byte = byte_col + 1
  local selected_entity = nil
  local selected_name_length = -1
  for _, entity in ipairs(document.entity_changes or {}) do
    local name = type(entity.name) == "string" and entity.name or ""
    local search_byte = 1
    while name ~= "" do
      local first_byte, last_byte = line:find(name, search_byte, true)
      if not first_byte then break end
      if cursor_byte >= first_byte and cursor_byte <= last_byte
          and has_entity_boundaries(line, first_byte, last_byte, name)
          and #name > selected_name_length then
        selected_entity = entity
        selected_name_length = #name
      end
      search_byte = first_byte + 1
    end
  end
  return selected_entity
end

---@param path string
---@param include_leaf boolean
---@return string[]
local function directory_fold_id_list(path, include_leaf)
  local part_list = vim.split(path:gsub("\\", "/"), "/", { plain = true, trimempty = true })
  local limit = include_leaf and #part_list or math.max(0, #part_list - 1)
  local result = {}
  local current = ""
  for index = 1, limit do
    current = current == "" and part_list[index] or (current .. "/" .. part_list[index])
    result[#result + 1] = "file-directory:" .. current
  end
  return result
end

---@param model DiffReviewPlanTaskModel
---@param entity_name string?
---@return table?
local function entity_by_name(model, entity_name)
  return vim.iter(model.document.entity_changes or {}):find(function(entity)
    return entity.name == entity_name
  end)
end

---@param model DiffReviewPlanTaskModel
---@param text string
---@param target table?
---@return table[]
local function file_tree_segments(model, text, target)
  if not target then return { { text } } end
  if target.target_type == "file_tree_file" then
    local name = vim.fs.basename(tostring(target.path or ""))
    if text:find("(new)", 1, true) then
      return task_tree_style.change("(new)", "", name, nil, "DiffReviewWalkthroughItemTitle")(text, 1)
    elseif text:find("(remove)", 1, true) then
      return task_tree_style.change("(remove)", "", name, nil, "DiffReviewWalkthroughItemTitle")(text, 1)
    end
    local renamed_from = text:match("([^%s]+)%s+→%s+" .. vim.pesc(name))
    if renamed_from then
      return task_tree_style.rename(renamed_from, name, "DiffReviewWalkthroughItemTitle")(text, 1)
    end
    return task_tree_style.change("", "", name, nil, "DiffReviewWalkthroughItemTitle")(text, 1)
  elseif target.target_type == "file_tree_entity" then
    local entity = entity_by_name(model, target.name)
    if not entity then return { { text } } end
    if entity.action == "rename" then
      return task_tree_style.rename(tostring(entity.renamed_from or ""), tostring(entity.name or ""))(text, 1)
    end
    local action = entity.action == "add" and "(new)"
        or entity.action == "remove" and "(remove)"
        or ""
    local kind = entity_kind_label[entity.kind] or tostring(entity.kind or "entity")
    return task_tree_style.change(action, kind, tostring(entity.name or ""))(text, 1)
  elseif target.target_type == "file_tree_test" then
    local action = target.action == "add" and "(new)"
        or target.action == "remove" and "(remove)"
        or ""
    local kind = title_case(target.category) .. "Test"
    return task_tree_style.change(action, kind, tostring(target.name or ""), "@type", "@function")(text, 1)
  end
  return { { text } }
end

---@param target table?
---@return string?, string[], boolean, string?
local function file_tree_fold_metadata(target)
  if not target or type(target.path) ~= "string" then return nil, {}, false, nil end
  if target.target_type == "file_directory" then
    local id_list = directory_fold_id_list(target.path, true)
    local fold_id = table.remove(id_list)
    return fold_id, id_list, false, nil
  elseif target.target_type == "file_tree_file" then
    local fold_id = "file-entry:" .. target.path
    return fold_id, directory_fold_id_list(target.path, false), true, nil
  elseif target.target_type == "file_tree_entity" or target.target_type == "file_tree_test" then
    local fold_target_id = "file-entry:" .. target.path
    local ancestor_ids = directory_fold_id_list(target.path, false)
    ancestor_ids[#ancestor_ids + 1] = fold_target_id
    return nil, ancestor_ids, false, fold_target_id
  end
  return nil, {}, false, nil
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
  if target and type(target.target_type) == "string" and target.target_type:match("^file_") then
    segments = file_tree_segments(model, text, target)
  elseif target and target.target_type == "dependency_manifest" then
    segments = task_tree_style.file(tostring(target.manifest or ""))(text, 1)
  elseif source_line >= model.test_heading_line then
    local test_path = text:match("^file%s+(.+)$")
    if test_path then segments = task_tree_style.file(test_path)(text, 1) end
  end
  local fold_id, ancestor_ids, default_folded, fold_target_id = file_tree_fold_metadata(target)
  return {
    id = ("plan:source:%d"):format(source_line),
    text = text,
    source_line = source_line,
    fold_id = fold_id,
    fold_target_id = fold_target_id,
    default_folded = default_folded,
    ancestor_ids = ancestor_ids,
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
  local target = name
  if entity.action == "rename" then
    target = ("%s → %s"):format(tostring(entity.renamed_from or ""), name)
  end
  return {
    text = ("%s %s %s — %s"):format(action, kind, target, tostring(entity.description or "")),
    segments_for_line = task_tree_style.entity_references(
      task_tree_style.change(action, kind, target),
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
    if type(entity.name) == "string" and entity.name ~= "" then
      entity_by_reference[entity.name] = entity
      entity_index_by_reference[entity.name] = entity_index - 1
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
      local file_path = tostring(file.to or file.path or "")
      local file_label = file.action == "rename"
          and ("%s → %s"):format(tostring(file.from or ""), file_path)
        or file_path
      local subtask_node_list = {}
      for subtask_index, subtask in ipairs(file.subtasks or {}) do
        local subtask_json_path = ("%s/subtasks/%d"):format(file_json_path, subtask_index - 1)
        local subtask_source = node_source(self, subtask_json_path)
        if subtask.operation == "test" then
          local test_node = vim.tbl_extend("force", {
            id = subtask_json_path,
            branch = true,
            foldable = false,
          }, test_node_fields(subtask, self.entity_name_set), source_fields(subtask_source))
          subtask_node_list[#subtask_node_list + 1] = test_node
        else
          local entity_node_list = {}
          for _, entity_name in ipairs(subtask.entities or {}) do
            local entity = entity_by_reference[entity_name]
            if entity then
              local entity_json_path = ("/entity_changes/%d"):format(entity_index_by_reference[entity_name])
              local entity_source = node_source(self, entity_json_path)
              entity_node_list[#entity_node_list + 1] = vim.tbl_extend("force", {
                id = entity_json_path,
                branch = true,
                foldable = false,
              }, entity_node_fields(entity, self.entity_name_set), source_fields(entity_source))
            end
          end
          subtask_node_list[#subtask_node_list + 1] = vim.tbl_extend("force", {
            id = subtask_json_path,
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
        id = file_json_path,
        text = "file " .. file_label,
        segments_for_line = task_tree_style.file(file_label),
        branch = false,
        foldable = false,
        child_prefix = "",
        gap_after = file_index < #(task.files or {}),
        children = subtask_node_list,
      }, source_fields(file_source))
    end
    task_node_list[#task_node_list + 1] = vim.tbl_extend("force", {
      id = task_json_path,
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

---@param line string
---@param byte_col integer Zero-based byte column.
---@return DiffReviewPlanEntity?
function PlanTaskModel:entity_at_position(line, byte_col)
  return M.entity_at_position(self.document, line, byte_col)
end

---@param item_list table[]?
---@param identity_key string
---@param identity string?
---@return table?
local function item_by_identity(item_list, identity_key, identity)
  if type(identity) ~= "string" or identity == "" then return nil end
  for _, item in ipairs(item_list or {}) do
    if item[identity_key] == identity then return item end
  end
  return nil
end

---@param document table
---@param anchor table?
---@param line string
---@param byte_col integer Zero-based byte column.
---@return DiffReviewPlanDescribedItem?
function M.described_item_at_position(document, anchor, line, byte_col)
  local target = anchor and anchor.target or nil
  if type(target) == "table" then
    local entity_name = target.target_type == "entity" and target.name or target.entity
    local entity = item_by_identity(document.entity_changes, "name", entity_name)
    local item = entity
    if entity and target.target_type == "entity_member" then
      item = item_by_identity(entity.members, "name", target.member)
    elseif entity and target.target_type == "enum_variant" then
      item = item_by_identity(entity.variants, "name", target.variant)
    elseif entity and target.target_type == "enum_variant_field" then
      local variant = item_by_identity(entity.variants, "name", target.variant)
      item = variant and item_by_identity(variant.fields, "name", target.field) or nil
    end
    if item and occupies_identifier(line, byte_col, item.name) then return item end
  end
  return M.entity_at_position(document, line, byte_col)
end

---@param source_line integer
---@param line string
---@param byte_col integer Zero-based byte column.
---@return DiffReviewPlanDescribedItem?
function PlanTaskModel:described_item_at_position(source_line, line, byte_col)
  return M.described_item_at_position(
    self.document,
    self:anchor_at_source_line(source_line),
    line,
    byte_col
  )
end

---@param source_line integer
---@return table?
function PlanTaskModel:anchor_at_source_line(source_line)
  return self.anchor_by_source_line[source_line]
end

---@param source_line integer
---@param line string
---@param byte_col integer Zero-based byte column.
---@return table?
function PlanTaskModel:rustdoc_target_at_position(source_line, line, byte_col)
  local anchor = self:anchor_at_source_line(source_line)
  local target = anchor and anchor.target or nil
  if not target
      or target.target_type ~= "flow_edge"
      or target.reference_kind ~= "external_entity" then
    return nil
  end
  local selection = nil
  if occupies_identifier(line, byte_col, target.callable_name) then
    selection = "callable"
  elseif target.target_is_type and occupies_identifier(line, byte_col, target.target_name) then
    selection = "receiver"
  end
  if not selection then return nil end
  return {
    json_path = anchor.json_path,
    selection = selection,
  }
end

---@class DiffReviewPlanWorkspaceTarget
---@field name string
---@field path string
---@field line integer

---@param source_line integer
---@param line string
---@param byte_col integer Zero-based byte column.
---@return DiffReviewPlanWorkspaceTarget?
function PlanTaskModel:workspace_target_at_position(source_line, line, byte_col)
  local anchor = self:anchor_at_source_line(source_line)
  local target = anchor and anchor.target or nil
  if not target
      or (target.target_type ~= "flow_step" and target.target_type ~= "flow_edge")
      or target.reference_kind ~= "workspace_entity"
      or type(target.workspace_path) ~= "string"
      or type(target.workspace_line) ~= "number"
      or not occupies_identifier(line, byte_col, target.target_name) then
    return nil
  end
  return {
    name = target.target_name,
    path = target.workspace_path,
    line = target.workspace_line,
  }
end

---@param source_line integer
---@param line string
---@param byte_col integer Zero-based byte column.
---@return table?
function PlanTaskModel:dependency_at_position(source_line, line, byte_col)
  local anchor = self:anchor_at_source_line(source_line)
  local target = anchor and anchor.target or nil
  if not target or target.target_type ~= "dependency" then return nil end
  local dependency = vim.iter(self.document.dependencies or {}):find(function(candidate)
    return candidate.name == target.name
  end)
  if not dependency or type(dependency.name) ~= "string" then return nil end
  local first_byte, last_byte = line:find(dependency.name, 1, true)
  local cursor_byte = byte_col + 1
  if first_byte and cursor_byte >= first_byte and cursor_byte <= last_byte
      and has_entity_boundaries(line, first_byte, last_byte, dependency.name) then
    return dependency
  end
  return nil
end

---@param entity DiffReviewPlanEntity
---@return integer?
function PlanTaskModel:entity_declaration_source_line(entity)
  return self.entity_source_line_by_name[entity.name]
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
  if type(document.plan_id) ~= "string" or document.plan_id == "" then
    return nil, "Canonical plan document does not contain plan_id"
  end
  if type(document.version) ~= "number" or document.version < 1 then
    return nil, "Canonical plan document does not contain a valid version"
  end
  if navigation.plan_id ~= document.plan_id then
    return nil, "Plan navigation index does not target the canonical plan"
  end
  if navigation.plan_version ~= document.version then
    return nil, "Plan navigation index does not target the canonical plan version"
  end
  local task_heading_line = heading_line(source_lines, "# Tasks")
  local test_heading_line = heading_line(source_lines, "# Tests")
  if not task_heading_line or not test_heading_line or test_heading_line <= task_heading_line then
    return nil, "Plan projection does not contain ordered Tasks and Tests sections"
  end
  local anchor_by_json_path = index_anchor_by_json_path(navigation)
  local entity_name_set = {}
  local entity_source_line_by_name = {}
  for entity_index, entity in ipairs(document.entity_changes or {}) do
    if type(entity.name) == "string" and entity.name ~= "" then
      entity_name_set[entity.name] = true
      local entity_anchor = anchor_by_json_path[("/entity_changes/%d"):format(entity_index - 1)]
      local source_line = entity_anchor and tonumber(entity_anchor.line) or nil
      if source_line then entity_source_line_by_name[entity.name] = source_line end
    end
  end
  return setmetatable({
    working_path = working_path,
    source_lines = source_lines,
    document = document,
    navigation = navigation,
    plan_id = document.plan_id,
    plan_version = document.version,
    task_heading_line = task_heading_line,
    test_heading_line = test_heading_line,
    anchor_by_json_path = anchor_by_json_path,
    anchor_by_source_line = index_anchor_by_source_line(navigation),
    entity_name_set = entity_name_set,
    entity_source_line_by_name = entity_source_line_by_name,
  }, PlanTaskModel), nil
end

return M
