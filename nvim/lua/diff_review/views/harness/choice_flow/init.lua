local M = {}

local config = require("diff_review.infra.config")
local navigation = require("diff_review.views.harness.choice_flow.navigation")
local window = require("diff_review.views.harness.choice_flow.window")

local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessChoiceFlow")

---@class DiffReviewChoiceFlowOption
---@field key? string
---@field value any
---@field label string

---@class DiffReviewChoiceFlowPage
---@field prompt string
---@field detail? string
---@field option_list DiffReviewChoiceFlowOption[]
---@field footer? string

---@class DiffReviewChoiceFlowSpec
---@field parent_win integer
---@field title string
---@field filetype? string
---@field width? integer
---@field zindex? integer
---@field page_list DiffReviewChoiceFlowPage[]
---@field choice_key_list? string[]
---@field on_select? fun(option: DiffReviewChoiceFlowOption, page_index: integer, close: function)
---@field on_close? function

---@class DiffReviewChoiceFlowState
---@field spec DiffReviewChoiceFlowSpec
---@field buf integer
---@field win integer
---@field host_buf integer
---@field page_index integer
---@field selected_index integer
---@field mapping_list table[]
---@field closed boolean

---@type DiffReviewChoiceFlowState?
local active

local function append_wrapped(line_list, text, width, prefix)
  prefix = prefix or "  "
  local current = prefix
  for word in tostring(text or ""):gmatch("%S+") do
    local separator = current == prefix and "" or " "
    if vim.fn.strdisplaywidth(current .. separator .. word) > width and current ~= prefix then
      line_list[#line_list + 1] = current
      current = string.rep(" ", vim.fn.strdisplaywidth(prefix)) .. word
    else
      current = current .. separator .. word
    end
  end
  line_list[#line_list + 1] = current ~= prefix and current or prefix
end

local function restore_keymaps(flow)
  for _, mapping in ipairs(flow.mapping_list) do
    pcall(vim.keymap.del, "n", mapping.key, { buffer = flow.host_buf })
    if mapping.previous and next(mapping.previous) then
      vim.api.nvim_buf_call(flow.host_buf, function() vim.fn.mapset("n", false, mapping.previous) end)
    end
  end
  flow.mapping_list = {}
end

local function close(flow, notify, restore)
  if not flow or flow.closed then return end
  flow.closed = true
  restore_keymaps(flow)
  window.close(flow.win, restore)
  if active == flow then active = nil end
  if notify and flow.spec.on_close then flow.spec.on_close() end
end

local function page(flow)
  return flow.spec.page_list[flow.page_index]
end

local function render(flow)
  if flow.closed or not vim.api.nvim_win_is_valid(flow.win) then return end
  local current = page(flow)
  local width = vim.api.nvim_win_get_width(flow.win)
  local line_list = { "" }
  append_wrapped(line_list, current.prompt, width - 4)
  if current.detail and current.detail ~= "" then
    line_list[#line_list + 1] = ""
    append_wrapped(line_list, current.detail, width - 4)
  end
  line_list[#line_list + 1] = ""
  local option_line = {}
  for index, option in ipairs(current.option_list or {}) do
    option_line[index] = #line_list + 1
    local key = option.key and (option.key .. "  ") or "   "
    append_wrapped(line_list, option.label, width - 4, "  " .. key)
  end
  line_list[#line_list + 1] = ""
  line_list[#line_list + 1] = current.footer or "  ↑↓ navigate  Enter select  q close"
  local height = math.min(#line_list, math.max(6, vim.api.nvim_win_get_height(flow.spec.parent_win) - 8))
  window.resize(flow.win, flow.spec.parent_win, width, height)
  vim.bo[flow.buf].modifiable = true
  vim.api.nvim_buf_set_lines(flow.buf, 0, -1, false, line_list)
  vim.api.nvim_buf_clear_namespace(flow.buf, namespace, 0, -1)
  for index, line in ipairs(option_line) do
    local option = current.option_list[index]
    if option and option.key then
      vim.api.nvim_buf_add_highlight(flow.buf, namespace, "DiffReviewHarnessQuestionKey", line - 1, 2, 2 + #option.key)
    end
  end
  local selected_line = option_line[flow.selected_index]
  if selected_line then
    vim.api.nvim_buf_add_highlight(flow.buf, namespace, "DiffReviewHarnessQuestionSelected", selected_line - 1, 0, -1)
  end
  vim.api.nvim_buf_add_highlight(flow.buf, namespace, "DiffReviewHarnessQuestionHint", #line_list - 1, 0, -1)
  vim.bo[flow.buf].modifiable = false
  window.clamp_view(flow.win, flow.buf)
  window.set_cursor_hidden(flow.win, true)
  window.set_focusable(flow.win, false)
  window.set_title(flow.win, flow.spec.title)
end

local function move(flow, delta)
  local count = #(page(flow).option_list or {})
  if count == 0 then return end
  flow.selected_index = navigation.cycle(flow.selected_index, count, delta)
  render(flow)
end

local function move_page(flow, delta)
  local count = #flow.spec.page_list
  if count < 2 then return end
  flow.page_index = navigation.cycle(flow.page_index, count, delta)
  flow.selected_index = 1
  render(flow)
end

local function preserve_mapping(flow, key, callback)
  if vim.iter(flow.mapping_list):any(function(mapping) return mapping.key == key end) then return end
  local previous = vim.api.nvim_buf_call(flow.host_buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  flow.mapping_list[#flow.mapping_list + 1] = { key = key, previous = previous }
  vim.keymap.set("n", key, callback, { buffer = flow.host_buf, silent = true, nowait = true })
end

local function install_keymaps(flow)
  preserve_mapping(flow, "<Up>", function() move(flow, -1) end)
  preserve_mapping(flow, "<Down>", function() move(flow, 1) end)
  preserve_mapping(flow, "s", function() move(flow, -1) end)
  preserve_mapping(flow, "t", function() move(flow, 1) end)
  preserve_mapping(flow, "<Left>", function() move_page(flow, -1) end)
  preserve_mapping(flow, "<Right>", function() move_page(flow, 1) end)
  preserve_mapping(flow, "<CR>", function()
    local option = page(flow).option_list[flow.selected_index]
    if option and flow.spec.on_select then flow.spec.on_select(option, flow.page_index, function() close(flow, false) end) end
  end)
  preserve_mapping(flow, "q", function() close(flow, true) end)
  for index, key in ipairs(flow.spec.choice_key_list or config.options.harness.question_choice_keys) do
    preserve_mapping(flow, key, function()
      if page(flow).option_list[index] then
        flow.selected_index = index
        render(flow)
      end
    end)
  end
end

---@param spec DiffReviewChoiceFlowSpec
---@return DiffReviewChoiceFlowState
function M.open(spec)
  if active then close(active, false, false) end
  local parent_width = vim.api.nvim_win_get_width(spec.parent_win)
  local width = math.max(44, math.min(spec.width or 72, parent_width - 8))
  local buf, win = window.open(spec.parent_win, width, 8, spec.title, spec.filetype, spec.zindex)
  local flow = {
    spec = spec,
    buf = buf,
    win = win,
    host_buf = vim.api.nvim_win_get_buf(spec.parent_win),
    page_index = 1,
    selected_index = 1,
    mapping_list = {},
    closed = false,
  }
  active = flow
  render(flow)
  vim.api.nvim_set_current_win(spec.parent_win)
  install_keymaps(flow)
  return flow
end

function M.is_open()
  return active ~= nil and not active.closed and vim.api.nvim_win_is_valid(active.win)
end

function M.close()
  close(active, false)
end

---@return DiffReviewChoiceFlowState?
function M._state_for_test()
  return active
end

return M
