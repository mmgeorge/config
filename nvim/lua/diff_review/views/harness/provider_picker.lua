local notifications = require("diff_review.infra.notifications")
local picker = require("diff_review.views.picker")
local provider_catalog = require("diff_review.views.harness.provider_catalog")

local M = {}
local owner = "harness-provider-catalog"
local loading_icon_list = { "○", "◔", "◑", "◕", "●", "◕", "◑", "◔" }

---@param value integer?
---@param estimated boolean?
---@return string
local function token_label(value, estimated)
  if value == nil then return "—" end
  local label
  if value >= 1000 then
    label = string.format("%.1fk", value / 1000):gsub("%.0k$", "k")
  else
    label = tostring(value)
  end
  return (estimated and "~" or "") .. label .. " tokens"
end

---@param definition table
---@param loading_name string?
---@param animation_index integer
---@return string
local function mcp_icon(definition, loading_name, animation_index)
  if definition.name == loading_name or definition.status == "loading" then
    return loading_icon_list[animation_index]
  end
  if not definition.enabled or definition.status == "disabled" then return "○" end
  if definition.status == "connected" then return "✓" end
  if definition.status == "needs_authentication" then return "!" end
  if definition.status == "failed" then return "×" end
  return "○"
end

---@param definition table
---@return string
local function mcp_status_label(definition)
  local label_by_status = {
    disabled = "disabled",
    loading = "starting",
    connected = "connected",
    needs_authentication = "authentication required",
    failed = "failed",
    unavailable = "status unavailable",
  }
  local label = label_by_status[definition.status] or definition.status or "status unavailable"
  if definition.status_detail and definition.status_detail ~= "" then
    label = label .. ": " .. definition.status_detail
  end
  return label
end

---@param definition table
---@return string[]
local function tool_line_list(definition)
  local result = {}
  local tool_name_list = vim.tbl_map(function(tool) return tool.name end, definition.tools or {})
  if #tool_name_list > 0 then
    result[1] = "└ Tools: " .. table.concat(tool_name_list, ", ")
  elseif definition.tool_error then
    result[1] = "└ Tools unavailable: " .. definition.tool_error
  else
    result[1] = "└ Tools: none advertised"
  end
  return result
end

---@param options table
function M.open_skills(options)
  provider_catalog.skills(true, function(skill_list, request_error)
    if request_error then
      notifications.error(request_error, "Harness skills")
      return
    end
    local busy = false
    local current_skill_list = skill_list or {}
    local function spec()
      local option_list = {}
      for _, skill in ipairs(current_skill_list) do
        if skill.user_invocable ~= false then
          option_list[#option_list + 1] = {
            id = skill.name,
            value = skill,
            label = (skill.enabled and "✓ " or "× ") .. skill.name,
            detail = skill.description,
          }
        end
      end
      return {
        owner = owner,
        host = options.host,
        page_list = {
          {
            id = "provider-skills",
            title = "Provider skills",
            subtitle = "Enable skills for future turns or insert an enabled skill invocation.",
            option_list = option_list,
            empty_text = "The provider advertised no user-invocable skills.",
            footer = "↑↓ select  Enter insert  Space enable/disable  q close",
          },
        },
        action_list = {
          {
            id = "toggle-skill",
            key = "<Space>",
            desc = "Enable or disable provider skill",
            callback = function(context)
              local skill = context.option and context.option.value
              if not skill or busy then return end
              busy = true
              provider_catalog.set_skill_enabled(skill.name, not skill.enabled, function(_, toggle_error)
                if toggle_error then
                  notifications.error(toggle_error, "Harness skills")
                end
                provider_catalog.skills(true, function(refreshed, refresh_error)
                  busy = false
                  if refresh_error then
                    notifications.error(refresh_error, "Harness skills")
                    return
                  end
                  current_skill_list = refreshed or {}
                  if picker.is_open(owner) then picker.update(spec()) end
                end)
              end)
            end,
          },
        },
        on_confirm = function(result)
          local skill = result.option and result.option.value
          if not skill then return false end
          if not skill.enabled then
            notifications.warn("Enable $" .. skill.name .. " before invoking it", "Harness skills")
            return false
          end
          options.on_insert("$" .. skill.name .. " ")
        end,
      }
    end
    picker.open(spec())
  end)
end

---@param options table
function M.open_mcp(options)
  provider_catalog.mcp(true, function(mcp_list, request_error)
    if request_error then
      notifications.error(request_error, "Harness MCP")
      return
    end
    local current_mcp_list = mcp_list or {}
    local expanded_by_name = {}
    local loading_name = nil
    local animation_index = 1
    local animation_timer = nil
    local busy = false
    local build_spec
    local start_animation

    local function provider_is_loading()
      return vim.iter(current_mcp_list):any(function(definition) return definition.status == "loading" end)
    end

    local function stop_animation()
      if animation_timer then
        animation_timer:stop()
        animation_timer:close()
        animation_timer = nil
      end
      loading_name = nil
      animation_index = 1
    end

    local function refresh(mutation_result)
      provider_catalog.mcp(true, function(refreshed, refresh_error)
        busy = false
        stop_animation()
        if refresh_error then
          notifications.error(refresh_error, "Harness MCP")
          if picker.is_open(owner) then picker.update(build_spec()) end
          if mutation_result and options.on_mutation_error then
            options.on_mutation_error("MCP changed but its refreshed status failed: " .. refresh_error)
          end
          return
        end
        current_mcp_list = refreshed or {}
        if provider_is_loading() then start_animation(nil) end
        if picker.is_open(owner) then picker.update(build_spec()) end
        if mutation_result and options.on_mutation then options.on_mutation(mutation_result) end
      end)
    end

    start_animation = function(name)
      if name then loading_name = name end
      if animation_timer then return end
      animation_index = 1
      animation_timer = vim.uv.new_timer()
      animation_timer:start(120, 120, vim.schedule_wrap(function()
        if not picker.is_open(owner) then
          stop_animation()
          return
        end
        animation_index = animation_index % #loading_icon_list + 1
        picker.update(build_spec())
      end))
    end

    build_spec = function()
      local option_list = {}
      for _, definition in ipairs(current_mcp_list) do
        option_list[#option_list + 1] = {
          id = definition.name,
          value = definition,
          label = mcp_icon(definition, loading_name, animation_index) .. " " .. definition.name,
          detail = table.concat({
            definition.transport or "unknown",
            token_label(definition.token_count, definition.token_estimated),
            definition.name == loading_name and "starting" or mcp_status_label(definition),
          }, "  "),
          child_line_list = expanded_by_name[definition.name] and tool_line_list(definition) or nil,
        }
      end
      return {
        owner = owner,
        host = options.host,
        page_list = {
          {
            id = "provider-mcp",
            title = "MCP servers",
            subtitle = "Provider-reported server state. Token counts prefixed with ~ are estimates.",
            option_list = option_list,
            empty_text = "The provider advertised no MCP servers.",
            footer = "↑↓ select  Space enable/disable  Tab tools  q close",
          },
        },
        action_list = {
          {
            id = "toggle-mcp",
            key = "<Space>",
            desc = "Enable or disable MCP server",
            callback = function(context)
              local definition = context.option and context.option.value
              if not definition or busy then return end
              busy = true
              if options.on_mutation_start then options.on_mutation_start(definition, not definition.enabled) end
              start_animation(definition.name)
              provider_catalog.set_mcp_enabled(definition.name, not definition.enabled, function(result, toggle_error)
                if toggle_error then
                  if options.on_mutation_error then options.on_mutation_error(toggle_error) end
                  notifications.error(toggle_error, "Harness MCP")
                  refresh(nil)
                  return
                end
                refresh(result or {})
              end)
            end,
          },
          {
            id = "toggle-tools",
            key = "<Tab>",
            desc = "Fold MCP tool list",
            callback = function(context)
              local definition = context.option and context.option.value
              if not definition then return end
              expanded_by_name[definition.name] = not expanded_by_name[definition.name]
              picker.update(build_spec())
            end,
          },
        },
        on_confirm = function() return false end,
        on_close = stop_animation,
      }
    end
    picker.open(build_spec())
    if provider_is_loading() then start_animation(nil) end
  end)
end

return M
