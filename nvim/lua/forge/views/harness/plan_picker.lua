local M = {}
local picker = require("forge.views.picker")
local datetime = require("forge.integrations.datetime")

---Select a submitted plan and revision without changing its saved history.
---@param options table
function M.open(options)
  local plans = options.plan_list
  local selected = plans[1].id
  local revision = {}
  for _, plan in ipairs(plans) do revision[plan.id] = plan.revision_count end
  local build_spec
  local function cycle(delta)
    for _, plan in ipairs(plans) do
      if plan.id == selected then
        revision[selected] = ((revision[selected] - 1 + delta) % plan.revision_count) + 1
        picker.update(build_spec())
        return
      end
    end
  end
  build_spec = function()
    local rows, selected_index = {}, 1
    for index, plan in ipairs(plans) do
      if plan.id == selected then selected_index = index end
      rows[#rows + 1] = {
        id = plan.id,
        label = ("%-24s  %s"):format(datetime.relative_ms(plan.created_at_ms),
          plan.title ~= "" and plan.title or "[unnamed plan]"),
        value = plan,
        columns = {
          datetime.relative_ms(plan.created_at_ms),
          plan.title ~= "" and plan.title or "[unnamed plan]",
          ("%s%d/%d%s"):format(plan.id == selected and "← " or "  ", revision[plan.id], plan.revision_count,
            plan.id == selected and " →" or "  "),
          plan.state == "awaiting_review" and "reviewing" or plan.state,
          plan.session_name or "",
        },
        detail = ("%s%d/%d%s  · %s · %s"):format(
          plan.id == selected and "← " or "  ", revision[plan.id], plan.revision_count,
          plan.id == selected and " →" or "  ",
          plan.state, plan.session_name),
      }
    end
    return {
      host = options.host,
      page_list = { { id = "replan", title = "Replan", subtitle = "Start a new plan from a saved revision.",
        column_headers = { "Created", "Plan", "Revision", "Status", "Session" },
        option_list = rows, selected_index = selected_index,
        footer = "↑↓ plan  ←→ revision  Enter start planning  q close" } },
      on_change = function(context)
        local plan = context.option and context.option.value
        if plan and plan.id ~= selected then
          selected = plan.id
          vim.schedule(function() picker.update(build_spec()) end)
        end
      end,
      on_confirm = function(result)
        options.on_confirm({ plan_id = result.option.value.id, revision = revision[result.option.value.id] })
      end,
      action_list = {
        { key = "<Left>", id = "previous-revision", callback = function() cycle(-1) end },
        { key = "<Right>", id = "next-revision", callback = function() cycle(1) end },
      },
    }
  end
  return picker.open(build_spec())
end

return M
