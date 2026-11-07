local Approval = {}

local config = require("forge.infra.config")
local picker = require("forge.views.picker")
local keymaps = require("forge.shared.keymaps")
local perf = require("forge.infra.perf")
local command_detail = require("forge.views.harness.command_detail")

---@class ForgeApprovalChoice
---@field choice_id string
---@field label string

---@class ForgeApprovalItem
---@field target_id string
---@field title string
---@field detail string
---@field commands ForgeApprovalCommand[]
---@field choices ForgeApprovalChoice[]

---@class ForgeApprovalSourceRange
---@field start integer Zero-based source byte offset.
---@field end integer Exclusive source byte offset.

---@class ForgeApprovalCommand
---@field source string
---@field shell string
---@field focus_range_list ForgeApprovalSourceRange[]
---@field highlight_list {range: ForgeApprovalSourceRange, group: string, priority: integer}[]

---@class ForgeApprovalRequest
---@field request_id string
---@field execution_kind "managed"|"native_observed"
---@field reason? string
---@field deadline? {expires_at_ms: number}
---@field targets ForgeApprovalItem[]

---@class ForgeApprovalAnswer
---@field item_id string
---@field choice_id string

---@class ForgeApprovalHost
---@field owner_label? string
---@field transcript_win integer
---@field window_list? integer[]
---@field control_win? integer
---@field interrupt fun()
---@field closed fun()
---@field resolve fun(id: string, answer_list: ForgeApprovalAnswer[], callback: fun(resolved: boolean))

---@class ForgeApprovalSession
---@field request ForgeApprovalRequest
---@field host ForgeApprovalHost
---@field selection table<string, ForgeApprovalChoice>
---@field page_index integer
---@field submitting boolean

---@class ForgeApprovalPickerOption
---@field key? string
---@field value string
---@field label string
---@field cancel_request? boolean
---@field confirm_on_key? boolean
---@field wrap_label? boolean

---@class ForgeApprovalPickerPage: ForgePickerPage
---@field target_id string
---@field title string
---@field subtitle? string
---@field content_list ForgePickerContent[]
---@field scroll_content? boolean
---@field column_headers string[]
---@field option_list ForgeApprovalPickerOption[]
---@field selected_index? integer
---@field footer string

---@class ForgeApprovalPickerSpec: ForgePickerSpec
---@field owner string
---@field action_list {key: string, modes: string[], desc: string, callback: fun()}[]
---@field host {window_list: integer[], control_win: integer}
---@field page_list ForgeApprovalPickerPage[]
---@field initial_page integer
---@field on_confirm fun(result: {page: ForgeApprovalPickerPage, option: ForgeApprovalPickerOption}): boolean
---@field on_close fun()

---@type ForgeApprovalSession?
local active = nil

---@param item ForgeApprovalItem
---@return ForgePickerContent[]
local function detail_content(item)
  if item.title == "Run command" then
    local content_list = {}
    for _, command in ipairs(item.commands) do content_list[#content_list + 1] = command_detail.format(command) end
    return content_list
  end
  return { { text = item.detail, group = "ForgePickerText", preformatted = true } }
end

---@param item ForgeApprovalItem
---@param choice ForgeApprovalChoice
---@return string
local function choice_label(item, choice)
  if item.title == "Run command" then
    if choice.choice_id == "allow_exact" then return "Always allow highlighted command" end
    if choice.choice_id == "deny_exact" then return "Always deny highlighted command" end
  end
  return choice.label
end

---@param instance ForgeApprovalSession
---@param cancel boolean
local function submit(instance, cancel)
  if instance.submitting then return end
  local answer_list = {}
  for _, item in ipairs(instance.request.targets) do
    answer_list[#answer_list + 1] = {
      item_id = item.target_id,
      choice_id = cancel and "cancel" or instance.selection[item.target_id].choice_id,
    }
  end
  instance.submitting = true
  instance.host.resolve(instance.request.request_id, answer_list, function(resolved)
    instance.submitting = false
    if active == instance and resolved then
      active = nil
      picker.close(false)
    end
  end)
end

---@param instance ForgeApprovalSession
---@return ForgeApprovalPickerSpec
local function build_spec(instance)
  local request, host = instance.request, instance.host
  local interrupt_keys = keymaps.view_keys_for("harness", "cancel")
  local actions = {}
  for _, action in ipairs({ { "scroll_previous", -1 }, { "scroll_next", 1 } }) do
    actions[#actions + 1] = {
      command = action[1], modes = "i", desc = "Scroll permission details",
      callback = function() picker.scroll_content(action[2]) end,
    }
  end
  for _, key in ipairs(interrupt_keys) do
    actions[#actions + 1] = {
      key = key, modes = { "n", "i" }, desc = request.execution_kind == "native_observed" and "Cancel permission request" or "Interrupt Harness task",
      callback = function()
        if request.execution_kind == "native_observed" then submit(instance, true)
        else picker.close(true) host.interrupt() end
      end,
    }
  end
  local page_list = {}
  for _, item in ipairs(request.targets) do
      local option_list, selected_index = {}, 1
      for index, choice in ipairs(item.choices) do
        option_list[#option_list + 1] = {
          key = config.options.picker.choice_keys[index], value = choice.choice_id,
          label = choice_label(item, choice), wrap_label = true,
        }
        if instance.selection[item.target_id] == choice then selected_index = index end
      end
      option_list[#option_list + 1] = {
        key = config.options.picker.choice_keys[#item.choices + 1], value = "", cancel_request = true,
        label = "Cancel permission request", wrap_label = true,
      }
      local content_list = detail_content(item)
      if host.owner_label then table.insert(content_list, 1, { text = host.owner_label, group = "ForgePickerText" }) end
      if request.reason then content_list[#content_list + 1] = { text = "Reason: " .. request.reason, group = "ForgePickerText" } end
      page_list[#page_list + 1] = {
        id = item.target_id, title = "Review Permission", subtitle = item.title ~= "Run command" and item.title or nil,
        content_list = content_list,
        scroll_content = true,
        column_headers = { "Decision" }, option_list = option_list, selected_index = selected_index,
        footer = "←→ page  ↑↓ select  Enter confirm  PgUp/PgDn details  q close"
          .. (interrupt_keys[1] and ("  " .. interrupt_keys[1] .. (request.execution_kind == "native_observed" and " cancel request" or " interrupt")) or ""),
      }
    end
  return {
    owner = "approval", action_list = actions,
    deadline = request.deadline,
    host = {
      window_list = host.window_list or { host.transcript_win },
      control_win = host.control_win or host.transcript_win,
    },
    page_list = page_list, initial_page = instance.page_index,
    on_confirm = function(result)
      if active ~= instance or instance.submitting then return false end
      if result.option.cancel_request then
        submit(instance, true)
        return false
      end
      for _, item in ipairs(request.targets) do
        if item.target_id == result.page.id then
          for _, choice in ipairs(item.choices) do
            if choice.choice_id == result.option.value then instance.selection[item.target_id] = choice end
          end
        end
      end
      for index, item in ipairs(request.targets) do
        if not instance.selection[item.target_id] then
          instance.page_index = index
          picker.update(build_spec(instance))
          return false
        end
      end
      submit(instance, false)
      return false
    end,
    on_close = function()
      if active == instance then active = nil end
      host.closed()
    end,
  }
end

---@param request ForgeApprovalRequest
---@param host ForgeApprovalHost
function Approval.open(request, host)
  picker.close(true)
  local instance = {
    request = request, host = host, selection = {}, page_index = 1, submitting = false,
  }
  active = instance
  perf.trace("harness", "ui.approval.open", { request_id = request.request_id, count = #request.targets }, function()
    picker.open(build_spec(instance))
  end)
end

---@return boolean
function Approval.is_open()
  return picker.is_open("approval")
end

function Approval.close()
  picker.close()
end

return Approval
