-- Unit tests for `forge.views.harness.provider_catalog` (provider_catalog.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["context_catalog"] = function()
  local session = require("forge.session")
  local catalog = require("forge.views.harness.provider_catalog")
  local context_status = require("forge.views.harness.context_status")
  local model_picker = require("forge.views.harness.model_picker")
  local picker = require("forge.views.picker")

  ---@type ForgeHarnessModel[]
  local source = {
    {
      id = "gpt-test",
      default_context_window = "default",
      context_window = {
        { id = "default", token_limit = 400000 },
        { id = "long_context" },
      },
    },
    {
      id = "other-model",
      default_context_window = "default",
      context_window = { { id = "default", token_limit = 128000 } },
    },
    { id = "auto", is_default = true, default_context_window = "default" },
  }
  local requests = mock_client.respond({
    ["session.provider.model.list"] = function(_, callback) callback(vim.deepcopy(source), nil) end,
  })
  local state = session.harness
  state.session = {
    id = "context-catalog",
    provider = "fixture_beta",
    workspace = vim.fn.getcwd(),
    model = "gpt-test",
    context_usage = { used = 60000, size = 272000, remaining_percent = 78 },
  }
  state.model_list, state.model_provider = nil, nil
  catalog.clear()

  ---@return ForgeHarnessModel[]
  local function models()
    ---@type ForgeHarnessModel[]?
    local result
    catalog.models(false, function(value, failure)
      assert(not failure, failure)
      result = value
    end)
    assert(result, "model catalog did not resolve")
    return result
  end

  local delivered = models()
  assert(delivered[1].context_window[1].token_limit == state.session.context_usage.size,
    "active picker capacity differs from the winbar's provider-reported limit")
  assert(delivered[1].context_window[2].token_limit == nil, "live default capacity leaked into long context")
  assert(delivered[2].context_window[1].token_limit == 128000, "live capacity leaked into another model")
  assert(state.model_list[1].context_window[1].token_limit == 400000, "live capacity mutated the catalog cache")
  assert(source[1].context_window[1].token_limit == 400000, "live capacity mutated provider metadata")

  local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
  model_picker.open({
    host = host,
    provider = "fixture_beta",
    model_list = delivered,
    current_model = "gpt-test",
    on_confirm = function() end,
  })
  local picker_view = picker._state_for_test()
  assert(vim.iter(picker_view.frame.lines):any(function(line)
    return line:find("gpt-test", 1, true) and line:find("272K", 1, true)
  end), "rendered picker did not show the live capacity")
  assert(context_status.segment(state.session.context_usage)[1].text == "78% · 272K",
    "winbar did not show the same capacity as the picker")
  picker.close(false)

  state.session.context_usage.size = 264000
  assert(models()[1].context_window[1].token_limit == 264000, "cache reuse ignored updated live capacity")
  assert(#requests == 1, "usage changes unnecessarily reloaded the model catalog")

  state.session.context_window = "long_context"
  state.session.context_usage.size = 1000000
  delivered = models()
  assert(delivered[1].context_window[1].token_limit == 400000, "long-context budget replaced the default limit")
  assert(delivered[1].context_window[2].token_limit == 1000000, "active long context did not use live usage")

  state.session.model = "default"
  state.session.resolved_model = "gpt-test"
  assert(models()[1].context_window[2].token_limit == 1000000, "default model did not resolve to its runtime model")
  assert(models()[3].context_window == nil, "resolved model usage leaked into the auto catalog entry")
  state.session.resolved_model = nil
  state.session.context_window = nil
  delivered = models()
  assert(delivered[3].context_window == nil,
    "observed capacity must not manufacture a selectable context-window override")
  assert(delivered[3].observed_context_token_limit == 1000000,
    "an active auto model without catalog limits lost its display-only capacity")
  local confirmed
  model_picker.open({ host = host, provider = "fixture_beta", model_list = { delivered[3] },
    current_model = "auto", on_confirm = function(value) confirmed = value end })
  picker_view = picker._state_for_test()
  assert(vim.iter(picker_view.frame.lines):any(function(line) return line:find("1M", 1, true) end),
    "display-only context capacity disappeared from the picker")
  picker_view.spec.on_confirm({ option = { value = delivered[3] } })
  assert(confirmed and confirmed.context_window == vim.NIL,
    "confirming a display-only capacity sent a context-window override")
  picker.close(false)

  state.session.model = "gpt-test"
  for _, size in ipairs({ 0, -1 }) do
    state.session.context_usage.size = size
    assert(models()[1].context_window[1].token_limit == 400000, "invalid usage replaced the explicit model limit")
  end
  state.session.context_usage = nil
  assert(models()[1].context_window[1].token_limit == 400000, "missing usage discarded the explicit model limit")

  state.session.id = "another-session"
  state.session.provider = "fixture_alpha"
  state.model_list, state.model_provider = nil, nil
  delivered = models()
  assert(#requests == 2, "switching sessions did not request an independent catalog")
  assert(delivered[1].context_window[1].token_limit == 400000, "session switch retained another session's limit")
end


-- A `default` thinking selection sends no level, so the provider uses its own default. The winbar
-- and the model picker must name that one level: the catalog's named default when it has one, and
-- `default` otherwise, never the first level of the model's list.
T["default_thinking_names_one_level_in_winbar_and_picker"] = function()
  local session = require("forge.session")
  local catalog = require("forge.views.harness.provider_catalog")
  local controller = require("forge.views.harness.controller")
  local model_picker = require("forge.views.harness.model_picker")
  local picker = require("forge.views.picker")
  local source = {
    -- The host's catalog carries a level saved by an earlier selection; this session chose `default`.
    { id = "unnamed-default", label = "Unnamed", reasoning = { "low", "medium", "high" }, selected_reasoning = "low" },
    { id = "named-default", label = "Named", reasoning = { "low", "medium", "high" }, default_reasoning = "medium" },
  }
  mock_client.respond({
    ["session.provider.model.list"] = function(_, callback) callback(vim.deepcopy(source), nil) end,
  })
  local header = ""
  require("forge.shared.keymaps").apply_view_winbar = function(_, _, _, _, segments)
    header = table.concat(vim.tbl_map(function(segment) return segment.text end, segments))
  end
  local state = session.harness
  state.command_set = controller.command_set()
  state.transcript_win = vim.api.nvim_get_current_win()
  state.session = { id = "default-thinking", provider = "fixture_beta", workspace = vim.fn.getcwd(),
    model = "unnamed-default", resolved_model = "unnamed-default", effort = "default" }
  state.model_list, state.model_provider = nil, nil
  catalog.clear()
  local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
  local function shown(model_id)
    local delivered
    catalog.models(false, function(value) delivered = value end)
    assert(delivered, "model catalog did not resolve")
    local confirmed
    model_picker.open({ host = host, provider = "fixture_beta", model_list = delivered, current_model = model_id,
      on_confirm = function(value) confirmed = value end })
    local view = picker._state_for_test()
    local label = model_id == "named-default" and "Named" or "Unnamed"
    local row = vim.iter(view.frame.lines):find(function(line) return line:find(label, 1, true) end)
    local option = vim.iter(view.spec.page_list[1].option_list):find(function(entry) return entry.id == model_id end)
    view.spec.on_confirm({ option = option })
    picker.close(false)
    controller.refresh_winbar()
    return row, header, confirmed.effort
  end
  local row, winbar, effort = shown("unnamed-default")
  assert(row:find("default", 1, true) and not row:find("low", 1, true), row)
  assert(winbar:find("Unnamed default", 1, true), winbar)
  MiniTest.expect.equality(effort, "default")
  state.session.model, state.session.resolved_model = "named-default", "named-default"
  row, winbar, effort = shown("named-default")
  assert(row:find("medium", 1, true), row)
  assert(winbar:find("Named medium", 1, true), winbar)
  MiniTest.expect.equality(effort, "medium")
end

T["a_view_switched_to_a_provider_labels_its_model_from_that_provider_catalog_at_once"] = function()
  local eq = MiniTest.expect.equality
  local session = require("forge.session")
  local catalog = require("forge.views.harness.provider_catalog")
  local requests = mock_client.record()
  catalog.clear()
  local first = session.harness
  first.session = { id = "first", provider = "fixture_beta", workspace = vim.fn.getcwd(), model = "beta-model" }
  catalog.models(true, function() end)
  requests[1].callback({ { id = "beta-model", label = "Beta Model" } })
  eq(first.model_list[1].label, "Beta Model")

  -- A view that a provider switch built for another session of that provider labels its model from
  -- the catalog already held, while its own catalog request is still unanswered.
  local switched = session.new_harness_state()
  switched.session = { id = "second", provider = "fixture_beta", workspace = vim.fn.getcwd(), model = "beta-model" }
  switched.model_provider, switched.model_list = "fixture_alpha", { { id = "alpha-model", label = "Alpha Model" } }
  session.activate_harness(switched)
  local delivered
  catalog.models(true, function(value) delivered = value end)
  eq(#requests, 2)
  eq({ switched.model_provider, switched.model_list and switched.model_list[1].label },
    { "fixture_beta", "Beta Model" })
  eq(delivered, nil)
  -- Its own catalog replaces the held one when it arrives, and later reads use it.
  requests[2].callback({ { id = "beta-model", label = "Beta Model (session)" } })
  eq(delivered[1].label, "Beta Model (session)")
  catalog.models(false, function(value) delivered = value end)
  eq({ #requests, delivered[1].label }, { 2, "Beta Model (session)" })
end

T["live_catalog_discards_workspace_responses"] = function()
  local equal = MiniTest.expect.equality
  local session = require("forge.session")
  local catalog = require("forge.views.harness.provider_catalog")
  local requests = mock_client.record()
  local state = session.harness
  state.session = { id = "lazy-binding", provider = "antigravity", workspace = vim.fn.getcwd() }
  local prebind_result, bound_result
  catalog.models(false, function(models) prebind_result = models end)
  catalog.invalidate_models(state)
  catalog.models(false, function(models) bound_result = models end)
  equal(#requests, 2)

  requests[1].callback({ { id = "workspace-model" } })
  equal(prebind_result, nil)
  equal(#requests, 2)
  requests[2].callback({ { id = "bound-model", reasoning = { "high" } } })
  equal(prebind_result, bound_result)
  equal(state.model_list, bound_result)
  equal(bound_result[1].id, "bound-model")
end

T["empty_catalog_has_no_model_to_confirm"] = function()
  local model_picker = require("forge.views.harness.model_picker")
  local picker = require("forge.views.picker")
  local window = vim.api.nvim_get_current_win()
  local confirmed = false
  model_picker.open({ host = { window_list = { window }, control_win = window }, provider = "antigravity",
    model_list = {}, on_confirm = function() confirmed = true end })
  local view = assert(picker._state_for_test())
  MiniTest.expect.equality(view.spec.page_list[1].empty_text, "The provider advertised no models.")
  MiniTest.expect.equality(assert(view.spec.on_confirm)({}), false)
  MiniTest.expect.equality(confirmed, false)
  picker.close(false)
end

return T
