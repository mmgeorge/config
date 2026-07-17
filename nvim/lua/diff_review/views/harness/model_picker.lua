local ModelPicker = {}

local picker = require("diff_review.views.picker")

local function format_token_count(token_count)
  if not token_count then return nil end
  if token_count >= 1000000 then
    local value = token_count / 1000000
    return value == math.floor(value) and ("%dM"):format(value) or ("%.1fM"):format(value)
  end
  if token_count >= 1000 then return ("%dK"):format(math.floor((token_count + 500) / 1000)) end
  return tostring(token_count)
end

local function selected_value(value_list, selected, default_value)
  for _, value in ipairs(value_list) do
    if value == selected then return selected end
  end
  for _, value in ipairs(value_list) do
    if value == default_value then return default_value end
  end
  return value_list[1]
end

local function context_id_list(model)
  return vim.tbl_map(function(context) return context.id end, model.context_window or {})
end

local function selectable_field_list(model)
  local field_list = {}
  if #(model.reasoning or {}) > 1 then field_list[#field_list + 1] = "reasoning" end
  if #(model.context_window or {}) > 1 then field_list[#field_list + 1] = "context_window" end
  return field_list
end

local function initialize_model(model)
  model.picker_reasoning = selected_value(
    model.reasoning or {},
    model.selected_reasoning,
    model.default_reasoning
  )
  model.picker_context_window = selected_value(
    context_id_list(model),
    model.selected_context_window,
    model.default_context_window
  )
end

local function find_model(model_list, model_id)
  for _, model in ipairs(model_list) do
    if model.id == model_id then return model end
  end
end

local function field_value(model, field)
  if field == "reasoning" then return model.picker_reasoning end
  return model.picker_context_window
end

local function field_option_list(model, field)
  if field == "reasoning" then return model.reasoning or {} end
  return context_id_list(model)
end

local function cycle_field(model, field, delta)
  local value_list = field_option_list(model, field)
  if #value_list < 2 then return end
  local current = field_value(model, field)
  local current_index = 1
  for index, value in ipairs(value_list) do
    if value == current then current_index = index break end
  end
  local next_value = value_list[((current_index - 1 + delta) % #value_list) + 1]
  if field == "reasoning" then model.picker_reasoning = next_value else model.picker_context_window = next_value end
end

local function context_label(model)
  for _, context in ipairs(model.context_window or {}) do
    if context.id == model.picker_context_window then
      return format_token_count(context.token_limit) or context.id
    end
  end
  return nil
end

local function decorated(value, active)
  if not value or value == "" then return nil end
  return active and ("← %s →"):format(value) or value
end

local function model_detail(model, active_field)
  local segment_list = {}
  local function append(value)
    if value and value ~= "" then segment_list[#segment_list + 1] = value end
  end
  append(decorated(model.picker_reasoning, active_field == "reasoning"))
  append(decorated(context_label(model), active_field == "context_window"))
  if model.vision then append("Vision") end
  append(model.description)
  return table.concat(segment_list, "  ")
end

---@param options table
function ModelPicker.open(options)
  local model_list = options.model_list
  for _, model in ipairs(model_list) do initialize_model(model) end
  local selected_model_id = options.current_model
  if not find_model(model_list, selected_model_id) then
    local default_model
    for _, model in ipairs(model_list) do if model.is_default then default_model = model break end end
    selected_model_id = (default_model or model_list[1]).id
  end
  local active_field_by_model = {}
  local api

  local function active_field(model)
    local field_list = selectable_field_list(model)
    local current = active_field_by_model[model.id]
    if vim.tbl_contains(field_list, current) then return current end
    active_field_by_model[model.id] = field_list[1]
    return field_list[1]
  end

  local function build_spec()
    local selected_index = 1
    local option_list = {}
    for index, model in ipairs(model_list) do
      if model.id == selected_model_id then selected_index = index end
      option_list[#option_list + 1] = {
        id = model.id,
        label = model.id,
        detail = model_detail(model, model.id == selected_model_id and active_field(model) or nil),
        value = model,
      }
    end
    local spec = {
      host = options.host,
      page_list = {
        {
          id = "model",
          title = "Select model",
          subtitle = "Choose the model, reasoning effort, and context window exposed by the active backend.",
          option_list = option_list,
          selected_index = selected_index,
          footer = "↑↓ model  ←→ value  Tab field  Enter confirm  q close",
        },
      },
      on_change = function(context)
        local model = context.option and context.option.value
        if model and selected_model_id ~= model.id then
          selected_model_id = model.id
          active_field(model)
          vim.schedule(function() if api then api.update(build_spec()) end end)
        end
      end,
      on_confirm = function(result)
        local model = result.option.value
        options.on_confirm({
          model = model.id,
          effort = model.picker_reasoning,
          context_window = model.picker_context_window or vim.NIL,
        })
      end,
    }
    spec.action_list = {
      {
        key = "<Tab>",
        id = "next-field",
        callback = function()
          local model = find_model(model_list, selected_model_id)
          if not model then return end
          local field_list = selectable_field_list(model)
          if #field_list == 0 then return end
          local current = active_field(model)
          local index = vim.fn.index(field_list, current) + 1
          active_field_by_model[model.id] = field_list[(index % #field_list) + 1]
          api.update(build_spec())
        end,
      },
      {
        key = "<Left>",
        id = "previous-value",
        callback = function()
          local model = find_model(model_list, selected_model_id)
          if model and active_field(model) then
            cycle_field(model, active_field(model), -1)
            api.update(build_spec())
          end
        end,
      },
      {
        key = "<Right>",
        id = "next-value",
        callback = function()
          local model = find_model(model_list, selected_model_id)
          if model and active_field(model) then
            cycle_field(model, active_field(model), 1)
            api.update(build_spec())
          end
        end,
      },
    }
    return spec
  end

  api = picker
  return picker.open(build_spec())
end

ModelPicker._format_token_count = format_token_count

return ModelPicker
