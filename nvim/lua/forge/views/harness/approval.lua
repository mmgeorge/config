local Approval = {}

local config = require("forge.infra.config")
local picker = require("forge.views.picker")
local keymaps = require("forge.shared.keymaps")
local perf = require("forge.infra.perf")
local command_detail = require("forge.views.harness.command_detail")

---@class ForgeApprovalChoice
---@field id string
---@field label string

---@class ForgeApprovalItem
---@field id string
---@field title string
---@field detail string
---@field command_list ForgeApprovalCommand[]
---@field choice_list ForgeApprovalChoice[]

---@class ForgeApprovalSourceRange
---@field start integer Zero-based source byte offset.
---@field end integer Exclusive source byte offset.

---@class ForgeApprovalCommand
---@field source string
---@field shell string
---@field focus_range_list ForgeApprovalSourceRange[]
---@field highlight_list {range: ForgeApprovalSourceRange, group: string, priority: integer}[]

---@class ForgeApprovalRequest
---@field id string
---@field reason? string
---@field deadline? {expires_at_ms: number}
---@field resolution_source? 'user'|'timeout'
---@field item_list ForgeApprovalItem[]
---@field required_mode? 'write'

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
---@field switch_mode? fun(mode: string, callback: fun(changed: boolean))

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
---@field confirm_on_key? boolean
---@field wrap_label? boolean

---@class ForgeApprovalPickerPage
---@field id string
---@field title string
---@field subtitle? string
---@field content_list ForgePickerContent[]
---@field scroll_content? boolean
---@field column_headers string[]
---@field option_list ForgeApprovalPickerOption[]
---@field selected_index? integer
---@field footer string

---@class ForgeApprovalPickerSpec
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
    for _, command in ipairs(item.command_list) do content_list[#content_list + 1] = command_detail.format(command) end
    return content_list
  end
  return { { text = item.detail, group = "ForgePickerText", preformatted = true } }
end

---@param item ForgeApprovalItem
---@param choice ForgeApprovalChoice
---@return string
local function choice_label(item, choice)
  if item.title == "Run command" then
    if choice.id == "allow_exact" then return "Always allow highlighted command" end
    if choice.id == "deny_exact" then return "Always deny highlighted command" end
  end
  return choice.label
end

---@param instance ForgeApprovalSession
---@param cancel boolean
local function submit(instance, cancel)
  if instance.submitting then return end
  local answer_list = {}
  for _, item in ipairs(instance.request.item_list) do
    answer_list[#answer_list + 1] = {
      item_id = item.id,
      choice_id = cancel and "cancel" or instance.selection[item.id].id,
    }
  end
  instance.submitting = true
  instance.host.resolve(instance.request.id, answer_list, function(resolved)
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
      key = key, modes = { "n", "i" }, desc = "Interrupt Harness task",
      callback = function()
        picker.close(true)
        host.interrupt()
      end,
    }
  end
  local page_list = {}
  if request.required_mode == "write" then
    page_list[1] = {
      id = "write-mode", title = "Switch to Write mode?",
      content_list = { { text = "This task needs to edit files. Write mode allows changes within your configured access and permission rules. The task will restart and continue.",
        group = "ForgePickerText" } },
      column_headers = { "Decision" },
      option_list = {
        { key = config.options.picker.choice_keys[1], value = "switch_write", label = "Switch to Write mode" },
        { key = config.options.picker.choice_keys[2], value = "stay_read", label = "Stay in Read mode" },
      },
      footer = "↑↓ select  Enter confirm  q close"
        .. (interrupt_keys[1] and ("  " .. interrupt_keys[1] .. " interrupt") or ""),
    }
  else
    for _, item in ipairs(request.item_list) do
      local option_list, selected_index = {}, 1
      for index, choice in ipairs(item.choice_list) do
        option_list[#option_list + 1] = {
          key = config.options.picker.choice_keys[index], value = choice.id,
          label = choice_label(item, choice), wrap_label = true,
        }
        if instance.selection[item.id] == choice then selected_index = index end
      end
      local content_list = detail_content(item)
      if host.owner_label then table.insert(content_list, 1, { text = host.owner_label, group = "ForgePickerText" }) end
      if request.reason then content_list[#content_list + 1] = { text = "Reason: " .. request.reason, group = "ForgePickerText" } end
      page_list[#page_list + 1] = {
        id = item.id, title = "Review Permission", subtitle = item.title ~= "Run command" and item.title or nil,
        content_list = content_list,
        scroll_content = true,
        column_headers = { "Decision" }, option_list = option_list, selected_index = selected_index,
        footer = "←→ page  ↑↓ select  Enter confirm  PgUp/PgDn details  q close"
          .. (interrupt_keys[1] and ("  " .. interrupt_keys[1] .. " interrupt") or ""),
      }
    end
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
      if request.required_mode == "write" then
        if result.option.value == "switch_write" then
          instance.submitting = true
          assert(host.switch_mode, "write mode prompt requires a task transition")("write", function(changed)
            instance.submitting = false
            if active == instance and changed then active = nil picker.close(false) end
          end)
        else
          for _, item in ipairs(request.item_list) do instance.selection[item.id] = { id = "deny_once" } end
          submit(instance, false)
        end
        return false
      end
      if result.option.value == "cancel" then
        submit(instance, true)
        return false
      end
      for _, item in ipairs(request.item_list) do
        if item.id == result.page.id then
          for _, choice in ipairs(item.choice_list) do
            if choice.id == result.option.value then instance.selection[item.id] = choice end
          end
        end
      end
      for index, item in ipairs(request.item_list) do
        if not instance.selection[item.id] then
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
  perf.trace("harness", "ui.approval.open", { request_id = request.id, count = #request.item_list }, function()
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
