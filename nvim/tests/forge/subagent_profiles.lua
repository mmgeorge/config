vim.loader.enable(false)

local client = require("forge.client")
local picker = require("forge.views.picker")
local form = require("forge.shared.form")
local original_request, original_open, original_notify = client.request_for, form.open, vim.notify
local current_form, pending_save, pending_models
local delayed_save, delayed_models = false, false
local errors, saves = {}, 0
local snapshot = { path = "global/forge/subagents.json", revision = "1", document = {
  default_profile = "worker", activity_limit = 5, profiles = {
    worker = { enabled = true, description = "Implement", instructions = "Worker instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", permission_mode = "inherit" },
    reviewer = { enabled = true, description = "Review", instructions = "Reviewer instructions", provider = "inherit",
      model = "inherit", reasoning_effort = "inherit", service_tier = "inherit", permission_mode = "inherit" },
  },
} }
snapshot.document.profiles.disabled = vim.tbl_extend("force", snapshot.document.profiles.reviewer, { enabled = false })
local state = { session = { id = "profiles", backend = "codex", model = "fixture-model" } }
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
    assert(params.provider == "codex")
    if delayed_models then pending_models = callback
    else callback({ { id = "fixture-model", reasoning = { "low", "high" }, is_default = true } }) end
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
  assert(#option_list == 5, "profile picker must include disabled profiles, activity settings, and config access")
  assert(option_list[1].id == "disabled" and option_list[1].columns[2] == "← Off →")
  assert(option_list[#option_list].label == "Open config file" and not option_list[#option_list].value,
    "config access must be the final non-profile option")
  select("reviewer")
  assert(not picker.is_open("subagent-profiles"))
  assert(vim.api.nvim_get_current_tabpage() ~= origin_tab, "form did not open a full page")
  assert(#vim.api.nvim_tabpage_list_wins(0) == 1 and vim.bo.filetype == "ForgeForm")
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
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "← On →")
  press_arrow("h", "disabled")
  assert(not snapshot.document.profiles.disabled.enabled and saves == before_toggle + 2)
  assert(instance.spec.page_list[1].option_list[1].columns[2] == "← Off →")
  press_arrow("<Right>", "reviewer")
  assert(snapshot.document.profiles.reviewer.enabled and saves == before_toggle + 2)
  assert(errors[#errors]:find("Default profile must be enabled"))

  delayed_save = true
  press_arrow("<Left>", "worker")
  local first_pending = pending_save
  press_arrow("<Right>", "worker")
  select("worker")
  select("config:" .. snapshot.path)
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
  assert(instance.spec.page_list[1].option_list[3].columns[2] == "← Off →")
  delayed_save = false

  press_arrow("<Right>", "activity-limit")
  assert(snapshot.document.activity_limit == 6, "activity entry limit did not persist")
  press_arrow("r", "activity-limit")
  assert(snapshot.document.activity_limit == 5, "shared previous-value key did not decrease the limit")
  snapshot.document.activity_limit = 20
  action("reload")
  local before_limit = saves
  press_arrow("h", "activity-limit")
  assert(snapshot.document.activity_limit == 20 and saves == before_limit, "activity entry limit exceeded 20")
  select("activity-limit")
  assert(picker.is_open("subagent-profiles"), "activity setting opened the config file")

  select("worker")
  focus("model")
  delayed_models = true
  current_form:cycle(1)
  current_form:close()
  pending_models({ { id = "late", reasoning = {} } })
  assert(picker.is_open("subagent-profiles"), "late catalog callback reopened the form")
  picker.close()

  local config_path = vim.fn.tempname() .. " subagents.json"
  local config_text = vim.json.encode(snapshot.document)
  vim.fn.writefile({ config_text }, config_path)
  snapshot.path = config_path
  require("forge.views.harness.subagent_profiles").open(state, host)
  local config_picker = picker._state_for_test()
  local config_option = config_picker.spec.page_list[1].option_list[#config_picker.spec.page_list[1].option_list]
  local before_open = saves
  for _, entry in ipairs(config_picker.spec.action_list) do
    if entry.id == "previous-value" or entry.id == "next-value" or entry.id == "delete" then
      entry.callback({ option = config_option })
    end
  end
  assert(saves == before_open, "config option was treated as a profile")
  config_picker.state.selected_index_by_page["subagent-profiles"] = #config_picker.spec.page_list[1].option_list
  for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(config_picker.buf, "n")) do
    if mapping.lhs == "<CR>" then mapping.callback() break end
  end
  assert(not picker.is_open("subagent-profiles"), "config selection left the picker open")
  assert(vim.api.nvim_buf_get_name(0) == vim.fn.fnamemodify(config_path, ":p"), "config selection opened the wrong file")
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
