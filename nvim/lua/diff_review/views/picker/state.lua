local PickerState = {}

local picker_filter = require("diff_review.views.picker.filter")

---@class DiffReviewPickerState
---@field page_index integer
---@field selected_index_by_page table<string, integer>
---@field draft_by_page table<string, string>
---@field selected_set_by_page table<string, table<string, boolean>>
---@field query_by_page table<string, string>

---@param spec table
---@return DiffReviewPickerState
function PickerState.new(spec)
  assert(type(spec) == "table", "picker spec must be a table")
  assert(type(spec.page_list) == "table" and #spec.page_list > 0, "picker requires at least one page")
  local draft_by_page = {}
  local query_by_page = {}
  local page_id_set = {}
  for page_index, page in ipairs(spec.page_list) do
    page.id = page.id or tostring(page_index)
    assert(not page_id_set[page.id], "picker page ids must be unique")
    page_id_set[page.id] = true
    page.option_list = page.option_list or {}
    local key_set = {}
    local option_id_set = {}
    for _, option in ipairs(page.option_list) do
      assert(option.label, "picker option requires a label")
      if option.key then
        assert(not key_set[option.key], "picker option keys must be unique within a page")
        key_set[option.key] = true
      end
      if option.id then
        assert(not option_id_set[option.id], "picker option ids must be unique within a page")
        option_id_set[option.id] = true
      end
    end
    if page.initial_draft then draft_by_page[page.id] = page.initial_draft end
    if page.search then query_by_page[page.id] = page.search.initial_query or "" end
  end
  return {
    page_index = math.max(1, math.min(#spec.page_list, spec.initial_page or 1)),
    selected_index_by_page = {},
    draft_by_page = draft_by_page,
    selected_set_by_page = {},
    query_by_page = query_by_page,
  }
end

---@param state DiffReviewPickerState
---@param spec table
---@return table
function PickerState.page(state, spec)
  local source = spec.page_list[state.page_index]
  if not source.search then return source end
  local page = {}
  for key, value in pairs(source) do page[key] = value end
  page.option_list = picker_filter.apply(source.option_list, state.query_by_page[source.id] or "")
  for option_index, option in ipairs(page.option_list) do
    option.key = source.search.choice_keys and source.search.choice_keys[option_index] or option.key
  end
  return page
end

---@param state DiffReviewPickerState
---@param spec table
---@return string
function PickerState.query(state, spec)
  local page = spec.page_list[state.page_index]
  return state.query_by_page[page.id] or ""
end

---@param state DiffReviewPickerState
---@param spec table
---@param query string
function PickerState.set_query(state, spec, query)
  local page = spec.page_list[state.page_index]
  state.query_by_page[page.id] = query
  state.selected_index_by_page[page.id] = 1
end

---@param state DiffReviewPickerState
---@param spec table
---@return integer
function PickerState.selected_index(state, spec)
  local page = PickerState.page(state, spec)
  local count = math.max(1, #page.option_list)
  local selected = state.selected_index_by_page[page.id] or page.selected_index or 1
  selected = math.max(1, math.min(count, selected))
  state.selected_index_by_page[page.id] = selected
  return selected
end

---@param state DiffReviewPickerState
---@param spec table
---@param delta integer
function PickerState.move(state, spec, delta)
  local page = PickerState.page(state, spec)
  if #page.option_list == 0 then return end
  local selected = PickerState.selected_index(state, spec)
  state.selected_index_by_page[page.id] = ((selected - 1 + delta) % #page.option_list) + 1
end

---@param state DiffReviewPickerState
---@param spec table
---@param delta integer
function PickerState.move_page(state, spec, delta)
  state.page_index = math.max(1, math.min(#spec.page_list, state.page_index + delta))
end

---@param state DiffReviewPickerState
---@param spec table
---@return table?
function PickerState.selected_option(state, spec)
  local page = PickerState.page(state, spec)
  return page.option_list[PickerState.selected_index(state, spec)]
end

---@param state DiffReviewPickerState
---@param spec table
function PickerState.toggle_selected(state, spec)
  local page = PickerState.page(state, spec)
  local option = PickerState.selected_option(state, spec)
  if not option then return end
  local selected_set = state.selected_set_by_page[page.id] or {}
  local option_id = tostring(option.id or option.value or PickerState.selected_index(state, spec))
  selected_set[option_id] = not selected_set[option_id]
  state.selected_set_by_page[page.id] = selected_set
end

---@param state DiffReviewPickerState
---@param spec table
---@return table[]
function PickerState.selected_option_list(state, spec)
  local page = PickerState.page(state, spec)
  local selected_set = state.selected_set_by_page[page.id] or {}
  local result = {}
  for index, option in ipairs(page.option_list) do
    local option_id = tostring(option.id or option.value or index)
    if selected_set[option_id] then result[#result + 1] = option end
  end
  return result
end

return PickerState
