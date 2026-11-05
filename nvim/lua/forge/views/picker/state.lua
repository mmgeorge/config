local PickerState = {}

local picker_filter = require("forge.views.picker.filter")

--- Caller-owned pages and optional navigation presentation for one picker.
---@class ForgePickerSpec
---@field on_dispose? fun() Release owned resources on every close, including silent close.
---@field owner? string Identity used to isolate resumed state.
---@field page_list ForgePickerPage[] Pages in display order.
---@field page_navigation? "tabs" Optional horizontal category navigation.
---@field wrap_pages? boolean Wrap category navigation at the first and last pages.
---@field hint_mode? "help" Show only the help shortcut instead of inline command hints.
---@field size_ratio? number Fixed fraction of host width and height, including borders; centers the popup.
---@field width_ratio? number Override the fraction of host width, including borders.
---@field placement? "center"|"bottom" Override popup placement within its host.
---@field keymaps? table<string, string|string[]|boolean> Override command keys for this picker instance.
---@field on_field_submit? fun(context: ForgePickerSelectionContext, value: string|number, complete: fun(error?: string)) Save an edited field and report completion.
---@field resume_state? ForgePickerResumeState Detached state captured before departure.
---@field initial_page? integer Explicit initial page, overriding a resumed page.
---@field [string] any Existing host, callback, sizing, and input options.

--- Caller-owned page and its existing content and interaction options.
---@class ForgePickerPage
---@field id? string Stable page identity, defaulted when omitted.
---@field title? string Category label.
---@field option_list? table[] Page option rows.
---@field field_descriptions? boolean Show focused-field help beneath the list. Defaults to true.
---@field field_description_height? integer Reserve this many rows for focused-field help, clipping overflow with an ellipsis.
---@field toggle_on_confirm? boolean Also toggle boolean fields with the confirm command.
---@field [string] any Existing field, search, and input options.

--- Detached view state for re-entry into the same logical picker.
---@class ForgePickerResumeState
---@field owner? string Originating picker owner.
---@field page_id string Selected page identity.
---@field selected_index_by_page table<string, integer> Fallback row positions.
---@field selected_option_id_by_page table<string, string> Stable selected row identities.
---@field draft_by_page table<string, string> Unsubmitted page drafts.
---@field selected_set_by_page table<string, table<string, boolean>> Multi-selection identities.
---@field query_by_page table<string, string> Page search text.
---@field field_by_page table<string, table<string, string>> Field identities by page and row.

---@class ForgePickerState
---@field page_index integer
---@field selected_index_by_page table<string, integer>
---@field draft_by_page table<string, string>
---@field selected_set_by_page table<string, table<string, boolean>>
---@field query_by_page table<string, string>
---@field field_by_page table<string, table<string, string>>

---@param spec ForgePickerSpec
---@return ForgePickerState
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
      if option.fields then
        assert(option.id and #option.fields > 0, "structured picker rows require an id and fields")
        local field_id_set = {}
        for _, field in ipairs(option.fields) do
          assert(type(field.id) == "string" and not field_id_set[field.id], "picker field ids must be unique within a row")
          field_id_set[field.id] = true
        end
      end
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
  local state = {
    page_index = math.max(1, math.min(#spec.page_list, spec.initial_page or 1)),
    selected_index_by_page = {},
    draft_by_page = draft_by_page,
    selected_set_by_page = {},
    query_by_page = query_by_page,
    field_by_page = {},
  }
  if spec.resume_state then PickerState.restore(state, spec, spec.resume_state) end
  return state
end

--- Get the current searchable rows without changing selection or page ownership.
local function page_options(state, page)
  if not page.search then return page.option_list end
  return picker_filter.apply(page.option_list, state.query_by_page[page.id] or "")
end

--- Get a stable row identity when the caller supplies one.
local function option_identity(option)
  if not option then return nil end
  if option.id ~= nil then return tostring(option.id) end
  if option.value ~= nil then return tostring(option.value) end
  return nil
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return table
function PickerState.page(state, spec)
  local source = spec.page_list[state.page_index]
  if not source.search then return source end
  local page = {}
  for key, value in pairs(source) do page[key] = value end
  page.option_list = page_options(state, source)
  for option_index, option in ipairs(page.option_list) do
    option.key = source.search.choice_keys and source.search.choice_keys[option_index] or option.key
  end
  return page
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return string
function PickerState.query(state, spec)
  local page = spec.page_list[state.page_index]
  return state.query_by_page[page.id] or ""
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@param query string
function PickerState.set_query(state, spec, query)
  local page = spec.page_list[state.page_index]
  state.query_by_page[page.id] = query
  state.selected_index_by_page[page.id] = 1
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return integer
function PickerState.selected_index(state, spec)
  local page = PickerState.page(state, spec)
  local count = math.max(1, #page.option_list)
  local selected = state.selected_index_by_page[page.id] or page.selected_index or 1
  selected = math.max(1, math.min(count, selected))
  state.selected_index_by_page[page.id] = selected
  return selected
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@param delta integer
function PickerState.move(state, spec, delta)
  local page = PickerState.page(state, spec)
  if #page.option_list == 0 then return end
  local selected = PickerState.selected_index(state, spec)
  if page.wrap_selection == false then
    state.selected_index_by_page[page.id] = math.max(1, math.min(#page.option_list, selected + delta))
    return
  end
  state.selected_index_by_page[page.id] = ((selected - 1 + delta) % #page.option_list) + 1
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@param delta integer
function PickerState.move_page(state, spec, delta)
  if spec.wrap_pages then
    state.page_index = (state.page_index - 1 + delta) % #spec.page_list + 1
    return
  end
  state.page_index = math.max(1, math.min(#spec.page_list, state.page_index + delta))
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return table?
function PickerState.selected_option(state, spec)
  local page = PickerState.page(state, spec)
  return page.option_list[PickerState.selected_index(state, spec)]
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
function PickerState.toggle_selected(state, spec)
  local page = PickerState.page(state, spec)
  local option = PickerState.selected_option(state, spec)
  if not option or option.disabled_reason or option.action == "submit" then return end
  local selected_set = state.selected_set_by_page[page.id] or {}
  local option_id = tostring(option.id or option.value or PickerState.selected_index(state, spec))
  selected_set[option_id] = not selected_set[option_id]
  state.selected_set_by_page[page.id] = selected_set
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
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

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return ForgePickerField?, integer?
function PickerState.selected_field(state, spec)
  local option = PickerState.selected_option(state, spec)
  if not option or not option.fields or #option.fields == 0 then return end
  local page = PickerState.page(state, spec)
  local selected = (state.field_by_page[page.id] or {})[option.id]
  for index, field in ipairs(option.fields) do
    if field.id == selected then return field, index end
  end
  return option.fields[1], 1
end

---@param state ForgePickerState
---@param spec ForgePickerSpec
---@param direction integer
function PickerState.move_field(state, spec, direction)
  local _, index = PickerState.selected_field(state, spec)
  if not index then return end
  local page = PickerState.page(state, spec)
  local option = PickerState.selected_option(state, spec)
  if not option then return end
  state.field_by_page[page.id] = state.field_by_page[page.id] or {}
  state.field_by_page[page.id][option.id] = option.fields[(index - 1 + direction) % #option.fields + 1].id
end

--- Capture detached values at departure or immediately before a synchronous update.
---@param state ForgePickerState
---@param spec ForgePickerSpec
---@return ForgePickerResumeState
function PickerState.capture(state, spec)
  local snapshot = {
    owner = spec.owner,
    page_id = spec.page_list[state.page_index].id,
    selected_index_by_page = vim.deepcopy(state.selected_index_by_page),
    selected_option_id_by_page = {},
    draft_by_page = vim.deepcopy(state.draft_by_page),
    selected_set_by_page = vim.deepcopy(state.selected_set_by_page),
    query_by_page = vim.deepcopy(state.query_by_page),
    field_by_page = vim.deepcopy(state.field_by_page),
  }
  for _, page in ipairs(spec.page_list) do
    local options = page_options(state, page)
    local index = math.max(1, math.min(math.max(1, #options), state.selected_index_by_page[page.id] or page.selected_index or 1))
    local option = options[index]
    snapshot.selected_index_by_page[page.id] = index
    snapshot.selected_option_id_by_page[page.id] = option_identity(option)
    if option and option.fields and option.fields[1] then
      local fields = snapshot.field_by_page[page.id] or {}
      fields[option.id] = fields[option.id] or option.fields[1].id
      snapshot.field_by_page[page.id] = fields
    end
  end
  return snapshot
end

--- Restore surviving page, row, and field identities without retaining snapshot tables.
---@param state ForgePickerState
---@param spec ForgePickerSpec
---@param resume_state ForgePickerResumeState
function PickerState.restore(state, spec, resume_state)
  if resume_state.owner ~= spec.owner then return end
  for page_index, page in ipairs(spec.page_list) do
    local page_id = assert(page.id, "picker pages must be initialized before restoring navigation")
    if not spec.initial_page and page_id == resume_state.page_id then state.page_index = page_index end
    if resume_state.draft_by_page[page_id] ~= nil then
      state.draft_by_page[page_id] = resume_state.draft_by_page[page_id]
    end
    if page.search and resume_state.query_by_page[page_id] ~= nil then
      state.query_by_page[page_id] = resume_state.query_by_page[page_id]
    end
    local selected_set, field_map = {}, {}
    local previous_set = resume_state.selected_set_by_page[page_id] or {}
    local previous_fields = resume_state.field_by_page[page_id] or {}
    for index, option in ipairs(page.option_list) do
      local identity = option_identity(option) or tostring(index)
      if previous_set[identity] then selected_set[identity] = true end
      for _, field in ipairs(option.fields or {}) do
        if previous_fields[option.id] == field.id then field_map[option.id] = field.id end
      end
    end
    state.selected_set_by_page[page_id] = selected_set
    state.field_by_page[page_id] = field_map
    local options = page_options(state, page)
    local selected_index = resume_state.selected_index_by_page[page_id] or page.selected_index or 1
    local selected_id = resume_state.selected_option_id_by_page[page_id]
    if selected_id then
      for index, option in ipairs(options) do
        if option_identity(option) == selected_id then selected_index = index break end
      end
    end
    state.selected_index_by_page[page_id] = math.max(1, math.min(math.max(1, #options), selected_index))
  end
end

return PickerState
