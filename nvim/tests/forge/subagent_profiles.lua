vim.loader.enable(false)

local client = require("forge.client")
local picker = require("forge.views.picker")
local form = require("forge.shared.form")
local original_request, original_open, original_notify = client.request_for, form.open, vim.notify
local current_form, pending_save, pending_models
local delayed_save, delayed_models = false, false
local errors, saves = {}, 0
local model_failure
local catalog_by_provider = {
  codex = { capability = { fast_mode = true, ultrafast_mode = true }, models = {
    { id = "fixture-model", reasoning = { "low", "high" }, default_reasoning = "low", is_default = true, service_tiers = { "fast", "ultrafast" } },
    { id = "simple-model", reasoning = {}, service_tiers = {} },
  } },
  copilot = { capability = { fast_mode = false, ultrafast_mode = false }, models = {
    { id = "copilot-first", reasoning = { "medium" }, default_reasoning = "medium" },
    { id = "copilot-default", reasoning = {}, is_default = true },
  } },
  claude = { capability = {}, models = {} },
}
local snapshot = { path = "global/forge/subagents.json", revision = "1", document = {
  default_profile = "worker", profiles = {
    worker = { enabled = true, description = "Implement", instructions = "Worker instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", permission_mode = "inherit" },
    reviewer = { enabled = true, description = "Review", instructions = "Reviewer instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", permission_mode = "inherit" },
  },
} }
snapshot.document.profiles.disabled = vim.tbl_extend("force", snapshot.document.profiles.reviewer, { enabled = false })
local state = { session = { id = "profiles", backend = "codex", model = "fixture-model", effort = "high", service_tier = "fast" } }
vim.notify = function(message) errors[#errors + 1] = message end
form.open = function(options) current_form = original_open(options) return current_form end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "profiles")
  if method == "agent.profiles.get" then callback(vim.deepcopy(snapshot))
  elseif method == "agent.profiles.save" then
    assert(params.expected_revision == snapshot.revision, "editor lost its revision")
    local function complete(failure)
      if failure then callback(nil, failure) return end
      local default_profile = params.document.profiles[params.document.default_profile]
      if not default_profile or not default_profile.enabled then
        callback(nil, "Default profile must be enabled") return
      end
      saves = saves + 1
      snapshot.document = vim.deepcopy(params.document)
      snapshot.revision = tostring(saves + 1)
      callback(vim.deepcopy(snapshot))
    end
    if delayed_save then pending_save = complete else complete() end
  elseif method == "agent.models" then
    if delayed_models then pending_models = callback
    else callback(vim.deepcopy(catalog_by_provider[params.provider]), model_failure) end
  else error("unexpected profile request: " .. method) end
end

local function select(id)
  local instance = assert(picker._state_for_test())
  for _, option in ipairs(instance.spec.page_list[1].option_list) do
    if option.id == id then instance.spec.on_confirm({ option = option }) return end
  end
  error("missing option " .. id)
end

local function action(id, selected)
  local instance = assert(picker._state_for_test())
  for _, entry in ipairs(instance.spec.action_list or {}) do
    if entry.id == id then entry.callback({ option = selected and { value = selected } }) return end
  end
  error("missing action " .. id)
end

local function focus(id)
  for _, field in ipairs(current_form.fields) do
    if field.id == id then
      local anchor = current_form.editable.native.anchor[id]
      local mark = vim.api.nvim_buf_get_extmark_by_id(current_form.buffer, current_form.namespace, field.mark, {})
      vim.api.nvim_win_set_cursor(current_form.window, { (anchor and anchor.start.row or mark[1]) + 1, 0 })
      vim.api.nvim_exec_autocmds("CursorMoved", { buffer = current_form.buffer })
      return field
    end
  end
  error("missing field " .. id)
end

local function edit(id, value)
  focus(id)
  local anchor = current_form.editable.native.anchor[id]
  assert(vim.bo[current_form.buffer].modifiable, "text field is locked")
  vim.api.nvim_buf_set_text(current_form.buffer, anchor.start.row, anchor.start.column,
    anchor.finish.row, anchor.finish.column, vim.split(value, "\n", { plain = true }))
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = current_form.buffer })
end

local succeeded, failure = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  local window, origin_tab = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_tabpage()
  local host = { window_list = { window }, control_win = window }
  require("forge.views.harness.subagent_profiles").open(state, host)
  local option_list = picker._state_for_test().spec.page_list[1].option_list
  assert(#option_list == 3, "profile picker must contain only profiles, including disabled profiles")
  assert(option_list[1].id == "disabled" and option_list[1].columns[2] == "←No→")
  assert(vim.trim(picker._state_for_test().spec.page_list[1].column_headers[2]) == "Enable")
  local baseline_position_list
  for _, width in ipairs({ 90, 100, 123 }) do
    local page = picker._state_for_test().spec.page_list[1]
    local frame = require("forge.views.picker.layout").build(page, 1, width, {})
    local heading, profile_row
    for _, line in ipairs(frame.lines) do
      if line:find("Profile", 1, true) and line:find("Provider", 1, true) then heading = line end
      if line:find("disabled", 1, true) then profile_row = line end
    end
    assert(heading and profile_row, "profile table did not render")
    assert(not profile_row:find("…", 1, true), "profile values must fit at width " .. width)
    assert(not profile_row:find("(", 1, true), "inherit label includes the resolved parent value")
    local search_start = 1
    local column_position_list = {}
    for _, label in ipairs({ "Provider", "Model", "Thinking", "Speed" }) do
      local heading_start = assert(heading:find(label, 1, true))
      local value_start, value_end = profile_row:find("inherit", search_start, true)
      assert(value_start and value_end, "inherit label was truncated at width " .. width)
      assert(vim.fn.strdisplaywidth(heading:sub(1, heading_start - 1))
        == vim.fn.strdisplaywidth(profile_row:sub(1, value_start - 1)), label .. " column is misaligned")
      column_position_list[#column_position_list + 1] = vim.fn.strdisplaywidth(heading:sub(1, heading_start - 1))
      search_start = value_end + 1
    end
    baseline_position_list = baseline_position_list or column_position_list
    assert(vim.deep_equal(column_position_list, baseline_position_list), "extra width must not spread the columns")
  end
  select("reviewer")
  assert(not picker.is_open("subagent-profiles"))
  assert(vim.api.nvim_get_current_tabpage() ~= origin_tab, "form did not open a full page")
  assert(#vim.api.nvim_tabpage_list_wins(0) == 1 and vim.bo.filetype == "ForgeForm")
  local form_lines = vim.api.nvim_buf_get_lines(current_form.buffer, 0, -1, false)
  assert(form_lines[1] == "reviewer", "form must start with the Name field")
  assert(#form_lines == #current_form.fields + 1, "form retained header or spacer rows")
  local label_style = vim.api.nvim_get_hl(0, { name = "ForgeFormLabel", link = false })
  assert(label_style.fg == 0xffffff and label_style.bold, "form labels must be white and bold")
  for index, field in ipairs(current_form.fields) do
    local mark = vim.api.nvim_buf_get_extmark_by_id(current_form.buffer, current_form.namespace, field.mark, { details = true })
    assert(mark[1] == index - 1, "form rows must be consecutive")
    assert(mark[3].virt_text[1][1] == field.label .. ": " and mark[3].virt_text[1][2] == "ForgeFormLabel")
    if field.choices then assert(mark[3].virt_text[2][2] == "Normal", "choice value inherited label styling") end
  end
  if vim.g.forge_profile_form_interactive then return end
  focus("model")
  assert(not vim.bo.modifiable, "choice value allowed native text edits")
  current_form:cycle(1)
  current_form:cycle(1)
  assert(current_form:values().model == "fixture-model")
  focus("reasoning_effort")
  for _ = 1, 3 do current_form:cycle(1) end
  assert(current_form:values().reasoning_effort == "high")
  edit("description", "Review carefully")
  edit("instructions", "Review instructions\nSecond line\n")
  focus("default")
  current_form:cycle(1)
  assert(saves == 0, "field edits saved separately")
  current_form:save()
  assert(saves == 1 and not vim.bo.modified)
  assert(snapshot.document.default_profile == "reviewer")
  assert(snapshot.document.profiles.reviewer.instructions == "Review instructions\nSecond line\n")
  focus("instructions")
  local field_text = current_form:values().instructions
  current_form:refresh()
  assert(current_form:values().instructions == field_text, "focus refresh rewrote native input")

  delayed_save = true
  edit("instructions", "Submitted instructions")
  current_form:save()
  edit("instructions", "Newer instructions")
  pending_save()
  assert(vim.bo.modified and current_form:values().instructions == "Newer instructions")
  assert(snapshot.document.profiles.reviewer.instructions == "Submitted instructions")
  current_form:save()
  pending_save("stale revision")
  assert(vim.bo.modified and errors[#errors]:find("stale revision"))
  current_form:close()
  assert(current_form.active and errors[#errors]:find("Unsaved changes"))
  current_form:reload()
  assert(not vim.bo.modified and current_form:values().instructions == "Submitted instructions")
  current_form:close()
  assert(vim.api.nvim_get_current_tabpage() == origin_tab)
  assert(picker.is_open("subagent-profiles"))

  delayed_save = false
  select("worker")
  assert(current_form:values().instructions == "Worker instructions", "another profile reused the draft")
  focus("provider")
  current_form:cycle(1)
  assert(current_form:values().provider == "codex")
  current_form:reload()
  current_form:close()
  action("add")
  edit("name", "custom")
  edit("instructions", "Custom instructions")
  current_form:save()
  assert(snapshot.document.profiles.custom.provider == "inherit")
  edit("name", "renamed")
  current_form:save()
  assert(snapshot.document.profiles.renamed and not snapshot.document.profiles.custom)
  current_form:close()
  action("delete", "renamed")
  assert(not snapshot.document.profiles.renamed)

  local instance = picker._state_for_test()
  local function press_arrow(key, name)
    for index, option in ipairs(instance.spec.page_list[1].option_list) do
      if option.id == name then instance.state.selected_index_by_page["subagent-profiles"] = index end
    end
    for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(instance.buf, "n")) do
      if mapping.lhs == key then mapping.callback() return end
    end
    error("missing arrow mapping " .. key)
  end
  local before_toggle = saves
  press_arrow("r", "disabled")
  assert(snapshot.document.profiles.disabled.enabled and saves == before_toggle + 1)
  assert(picker._state_for_test() == instance, "toggle reopened the picker")
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "←Yes→")
  press_arrow("h", "disabled")
  assert(not snapshot.document.profiles.disabled.enabled and saves == before_toggle + 2)
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "←No→")
  press_arrow("<Right>", "reviewer")
  assert(snapshot.document.profiles.reviewer.enabled and saves == before_toggle + 2)
  assert(errors[#errors]:find("Default profile must be enabled"))

  delayed_save = true
  press_arrow("<Left>", "worker")
  local first_pending = pending_save
  press_arrow("<Right>", "worker")
  select("worker")
  action("open-config")
  action("reload")
  assert(picker.is_open("subagent-profiles") and pending_save == first_pending,
    "pending toggle allowed another operation")
  pending_save("stale revision")
  assert(snapshot.document.profiles.worker.enabled and errors[#errors]:find("stale revision"))
  press_arrow("<Right>", "worker")
  pending_save()
  assert(not snapshot.document.profiles.worker.enabled)
  assert(instance.state.selected_index_by_page["subagent-profiles"] == 3,
    "toggle lost the selected profile")
  assert(instance.spec.page_list[1].option_list[3].columns[2] == "←No→")
  delayed_save = false

  for _, option in ipairs(instance.spec.page_list[1].option_list) do
    assert(option.value, "unexpected non-profile menu option")
  end

  picker.close()
  require("forge.views.harness.subagent_profiles").open(state, host)
  select("worker")
  focus("model")
  delayed_models = true
  current_form:cycle(1)
  current_form:close()
  pending_models(catalog_by_provider.codex)
  assert(picker.is_open("subagent-profiles"), "late catalog callback reopened the form")
  picker.close()

  delayed_models = false
  local worker = snapshot.document.profiles.worker
  worker.provider, worker.model, worker.reasoning_effort, worker.service_tier = "codex", "fixture-model", "high", "ultrafast"
  require("forge.views.harness.subagent_profiles").open(state, host)
  instance = picker._state_for_test()
  action("next-field", "worker")
  local before_provider = saves
  press_arrow("h", "worker")
  worker = snapshot.document.profiles.worker
  assert(worker.provider == "copilot" and worker.model == "copilot-first",
    "provider change must select the first advertised model, not the default model")
  assert(worker.reasoning_effort == "default" and worker.service_tier == "default",
    "provider change retained unsupported thinking or speed")
  assert(saves == before_provider + 1, "dependent changes were not saved atomically")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.model == "copilot-default", "model column did not cycle")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.reasoning_effort == "default", "non-reasoning model offered thinking")
  action("next-field", "worker")
  press_arrow("h", "worker")
  assert(snapshot.document.profiles.worker.service_tier == "default", "unsupported provider offered fast speed")

  select("worker")
  focus("provider")
  local before_empty = current_form:values()
  current_form:cycle(1)
  assert(vim.deep_equal(current_form:values(), before_empty) and errors[#errors]:find("No models available"),
    "empty catalog changed the profile")
  model_failure = "catalog offline"
  current_form:cycle(1)
  assert(vim.deep_equal(current_form:values(), before_empty) and errors[#errors]:find("catalog offline"))
  model_failure = nil
  current_form:cycle(-1)
  assert(current_form:values().provider == "codex" and current_form:values().model == "fixture-model")
  focus("reasoning_effort")
  current_form:cycle(1)
  current_form:cycle(1)
  assert(current_form:values().reasoning_effort == "high")
  focus("model")
  current_form:cycle(1)
  assert(current_form:values().model == "simple-model" and current_form:values().reasoning_effort == "default",
    "model change retained unsupported thinking")
  focus("reasoning_effort")
  current_form:cycle(1)
  assert(current_form:values().reasoning_effort == "default", "incompatible inherited thinking was offered")
  focus("service_tier")
  current_form:cycle(1)
  assert(current_form:values().service_tier == "default", "model without speed tiers offered fast")
  focus("provider")
  current_form:cycle(1)
  assert(current_form:values().service_tier == "default", "full-page editor retained unsupported speed")

  delayed_models = true
  current_form:cycle(1)
  local stale_catalog = pending_models
  edit("instructions", "Newer draft during catalog lookup")
  stale_catalog({ capability = {}, models = { { id = "claude-first", reasoning = {} } } })
  assert(current_form:values().provider == "copilot" and current_form:values().instructions == "Newer draft during catalog lookup",
    "late catalog response overwrote newer form edits")
  current_form:reload()
  current_form:close()
  picker.close()

  require("forge.views.harness.subagent_profiles").open(state, host)
  instance = picker._state_for_test()
  action("next-field", "worker")
  local before_late = saves
  press_arrow("h", "worker")
  local pending_provider = pending_models
  press_arrow("h", "worker")
  assert(pending_models == pending_provider, "pending provider switch admitted another mutation")
  picker.close()
  pending_provider({ capability = {}, models = { { id = "claude-first", reasoning = {} } } })
  assert(saves == before_late and not picker.is_open("subagent-profiles"), "closed picker saved a late provider switch")
  delayed_models = false

  local config_path = vim.fn.tempname() .. " subagents.json"
  local config_text = vim.json.encode(snapshot.document)
  vim.fn.writefile({ config_text }, config_path)
  snapshot.path = config_path
  require("forge.views.harness.subagent_profiles").open(state, host)
  local before_open = saves
  assert(vim.fn.maparg("oc", "n", false, true).buffer == 1, "config hotkey must be local to the picker")
  vim.api.nvim_feedkeys("oc", "xt", false)
  assert(not picker.is_open("subagent-profiles"), "config hotkey left the picker open")
  assert(saves == before_open, "config hotkey mutated a profile")
  assert(vim.api.nvim_buf_get_name(0) == vim.fn.fnamemodify(config_path, ":p"), "config hotkey opened the wrong file")
  assert(vim.api.nvim_get_current_tabpage() ~= origin_tab, "config did not open in its own tab")
  assert(vim.bo.buftype == "" and vim.bo.modifiable and not vim.bo.readonly)
  assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == config_text, "config contents changed on open")
  vim.cmd("tabclose")
  vim.fn.delete(config_path)
end, debug.traceback)
if succeeded and vim.g.forge_profile_form_interactive then return end
client.request_for, form.open, vim.notify = original_request, original_open, original_notify
if not succeeded then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("subagent_profiles: passed")
  vim.cmd("qa!")
end
