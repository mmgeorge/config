local M = {}

local declaration_kind_list = {
  "abstract class",
  "interface",
  "resource",
  "struct",
  "class",
  "trait",
  "config",
  "cache",
  "adapter",
  "enum",
  "fn",
}

local builtin_type = {
  bool = true,
  boolean = true,
  f32 = true,
  f64 = true,
  i8 = true,
  i16 = true,
  i32 = true,
  i64 = true,
  i128 = true,
  isize = true,
  never = true,
  number = true,
  string = true,
  u8 = true,
  u16 = true,
  u32 = true,
  u64 = true,
  u128 = true,
  unit = true,
  unknown = true,
  usize = true,
  void = true,
}

local function add_span(span_list, start_index, end_index, highlight)
  if start_index and end_index and start_index <= end_index then
    span_list[#span_list + 1] = {
      start_index = start_index,
      end_index = end_index,
      highlight = highlight,
    }
  end
end

local function path_start(line)
  return line:find("%[[^%]]+%]%s*$")
end

---@param line string
---@param target table?
---@param width integer?
---@return string
function M.align_owner(line, target, width)
  local target_type = target and target.target_type or nil
  if target_type ~= "entity"
      and target_type ~= "flow_step"
      and target_type ~= "flow_operation" then
    return line
  end
  local inline_path_start = path_start(line)
  if not inline_path_start or not width then return line end
  local content = line:sub(1, inline_path_start - 1):gsub("%s+$", "")
  local owner = line:sub(inline_path_start):gsub("%s+$", "")
  local gap = width - vim.fn.strdisplaywidth(content) - vim.fn.strdisplaywidth(owner)
  if gap < 1 then return line end
  return content .. string.rep(" ", gap) .. owner
end

local function identifier_span_list(line, limit)
  local result = {}
  local cursor = 1
  while cursor <= limit do
    local start_index, end_index = line:find("[%a_][%w_]*", cursor)
    if not start_index or start_index > limit then break end
    result[#result + 1] = {
      text = line:sub(start_index, end_index),
      start_index = start_index,
      end_index = math.min(end_index, limit),
    }
    cursor = end_index + 1
  end
  return result
end

local function declaration_spans(line, span_list, limit)
  local keyword_start
  local keyword_end
  local declaration_kind
  for _, kind in ipairs(declaration_kind_list) do
    local start_index, end_index = line:find(kind, 1, true)
    if start_index and (not keyword_start or start_index < keyword_start) then
      keyword_start = start_index
      keyword_end = end_index
      declaration_kind = kind
    end
  end
  if keyword_start then
    add_span(span_list, keyword_start, keyword_end, "@keyword")
    local name_start, name_end = line:find("[%a_][%w_]*", keyword_end + 1)
    if name_start and name_start <= limit then
      add_span(
        span_list,
        name_start,
        name_end,
        declaration_kind == "fn" and "@function" or "@type"
      )
    end
  end

  local extends_start, extends_end = line:find("extends", 1, true)
  if extends_start and extends_start <= limit then
    add_span(span_list, extends_start, extends_end, "@keyword")
  end

  for _, identifier in ipairs(identifier_span_list(line, limit)) do
    local follows_relation = extends_end and identifier.start_index > extends_end
    local follows_conformance = line:sub(1, identifier.start_index - 1):match(":%s*$") ~= nil
    local continuation_type = not keyword_start and identifier.text:match("^[A-Z]") ~= nil
    if follows_relation or follows_conformance or continuation_type then
      add_span(span_list, identifier.start_index, identifier.end_index, "@type")
    end
  end
end

local function member_spans(line, span_list, limit)
  local name_start, name_end = line:find("[%a_][%w_]*", 1)
  if not name_start or name_start > limit then return end
  local open_parenthesis = line:find("(", name_end + 1, true)
  local is_operation = open_parenthesis ~= nil and open_parenthesis <= limit
  add_span(span_list, name_start, name_end, is_operation and "@function.method" or "@variable.member")

  local parameter_name = {}
  if is_operation then
    local close_parenthesis = line:find(")", open_parenthesis + 1, true) or limit
    local cursor = open_parenthesis + 1
    while cursor < close_parenthesis do
      local parameter_start, parameter_end = line:find("[%a_][%w_]*%s*:", cursor)
      if not parameter_start or parameter_start >= close_parenthesis then break end
      local name = line:sub(parameter_start, parameter_end):match("^[%a_][%w_]*")
      local name_finish = parameter_start + #name - 1
      parameter_name[name] = true
      add_span(span_list, parameter_start, name_finish, "@variable.parameter")
      cursor = parameter_end + 1
    end
  end

  for _, identifier in ipairs(identifier_span_list(line, limit)) do
    local text = identifier.text
    local is_type = text:find("::", 1, true)
      or text:match("^[A-Z]") ~= nil
      or builtin_type[text:lower()] == true
    if text == "mut" then
      add_span(span_list, identifier.start_index, identifier.end_index, "@keyword.modifier")
    elseif is_type and not parameter_name[text] and identifier.start_index ~= name_start then
      add_span(span_list, identifier.start_index, identifier.end_index, "@type")
    end
  end
end

local dependency_action_highlight = {
  Add = "DiffReviewWalkthroughActionAdd",
  Modify = "DiffReviewWalkthroughActionModify",
  Remove = "DiffReviewWalkthroughActionRemove",
}

---@param line string
---@param span_list table[]
local function dependency_spans(line, span_list)
  local _, marker_end = line:find("^├─ ", 1)
  if not marker_end then _, marker_end = line:find("^└─ ", 1) end
  if not marker_end then
    add_span(span_list, 1, #line, "Normal")
    return
  end
  local action_start, action_end, action = line:find("([%a]+)%s+", marker_end + 1)
  local action_highlight = action and dependency_action_highlight[action] or nil
  if action_start ~= marker_end + 1 or not action_highlight then
    add_span(span_list, 1, #line, "Normal")
    return
  end
  local name_start, name_end = line:find("[^%s]+", action_end + 1)
  add_span(span_list, action_start, action_end - 1, action_highlight)
  add_span(span_list, name_start, name_end, "DiffReviewDependencyName")
  local metadata_start, metadata_end = line:find("%b()", (name_end or 0) + 1)
  add_span(span_list, metadata_start, metadata_end, "DiffReviewPlanMetadata")
end

local function segments_from_spans(line, span_list)
  table.sort(span_list, function(left, right)
    if left.start_index == right.start_index then return left.end_index > right.end_index end
    return left.start_index < right.start_index
  end)
  local segment_list = {}
  local cursor = 1
  for _, span in ipairs(span_list) do
    if span.start_index >= cursor then
      if span.start_index > cursor then
        segment_list[#segment_list + 1] = { line:sub(cursor, span.start_index - 1) }
      end
      segment_list[#segment_list + 1] = {
        line:sub(span.start_index, span.end_index),
        span.highlight,
      }
      cursor = span.end_index + 1
    end
  end
  if cursor <= #line then segment_list[#segment_list + 1] = { line:sub(cursor) } end
  return #segment_list > 0 and segment_list or nil
end

---@param line string
---@param target table?
---@return table[]?
function M.segments(line, target)
  local target_type = target and target.target_type or nil
  local is_flow_row = target_type == "flow_step"
    or target_type == "flow_operation"
    or target_type == "flow_value"
  if target_type ~= "entity"
      and target_type ~= "entity_member"
      and target_type ~= "enum_variant"
      and target_type ~= "enum_variant_field"
      and target_type ~= "dependency"
      and target_type ~= "test"
      and not is_flow_row then
    return nil
  end

  local inline_path_start = path_start(line)
  local limit = (inline_path_start or (#line + 1)) - 1
  local span_list = {}
  if target_type == "flow_operation" then
    add_span(span_list, 1, limit, "Normal")
  elseif target_type == "flow_value" then
    local _, marker_end = line:find("├─", 1, true)
    if not marker_end then _, marker_end = line:find("└─", 1, true) end
    local value_start = marker_end and line:find("%S", marker_end + 1) or nil
    local value_end = line:find("%s*$") - 1
    if value_start and target.value_kind == "type" then
      add_span(span_list, 1, value_start - 1, "Normal")
      add_span(span_list, value_start, value_end, "@type")
      add_span(span_list, value_end + 1, #line, "Normal")
    elseif value_start then
      add_span(span_list, 1, #line, "Normal")
    end
  elseif target_type == "entity" then
    declaration_spans(line, span_list, limit)
  elseif target_type == "dependency" then
    dependency_spans(line, span_list)
  elseif target_type == "enum_variant" then
    local start_index, end_index = line:find("[%a_][%w_]*", 1)
    add_span(span_list, start_index, end_index, "@variable")
  elseif target_type == "test" then
    local category_start, category_end = line:find("[%a_][%w_]*Test")
    add_span(span_list, category_start, category_end, "@type")
  elseif not is_flow_row then
    member_spans(line, span_list, limit)
  end
  if inline_path_start then
    local inline_path_end = line:find("%]%s*$", inline_path_start)
    add_span(span_list, inline_path_start, inline_path_end, "DiffReviewPlanMetadata")
  end
  return segments_from_spans(line, span_list)
end

return M
